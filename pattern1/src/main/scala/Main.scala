import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.Await
import domain.*
import mock.*

given ExecutionContext = ExecutionContext.global

type Compensation = SagaState => Future[Either[String, SagaState]]

case class SagaState(steps: Vector[String], compensations: Vector[Compensation]):
  def addStep(step: String): SagaState               = copy(steps = steps :+ step)
  def addCompensation(comp: Compensation): SagaState = copy(compensations = compensations :+ comp)

case class SagaFailure(message: String, state: SagaState)

case class Saga[A](run: SagaState => Future[Either[SagaFailure, (SagaState, A)]]):

  def map[B](f: A => B): Saga[B] =
    Saga(state => run(state).map(_.map { case (s, a) => (s, f(a)) }))

  def flatMap[B](f: A => Saga[B]): Saga[B] =
    Saga { state =>
      run(state).flatMap {
        case Right((s1, a)) => f(a).run(s1)
        case Left(err)      => Future.successful(Left(err))
      }
    }

  def withCompensation(undo: Compensation): Saga[A] =
    Saga { state =>
      run(state).map {
        case Left(err) => Left(err)
        case Right((s1, a)) =>
          val updated = s1.addCompensation(undo)
          Right((updated, a))
      }
    }

  def exec(initial: SagaState): Future[Either[SagaFailure, (SagaState, A)]] =
    run(initial).flatMap {
      case Right((finalState, result)) => Future.successful(Right((finalState, result)))
      case Left(err) =>
        err.state.compensations.foldRight(Future.successful(err.state)) { (comp, accState) =>
          accState.flatMap { currentState =>
            comp(currentState).map {
              case Right(nextState) => nextState
              case Left(compErr)    => currentState.addStep(s"compensation failed: $compErr")
            }
          }
        }.map(finalState => Left(err.copy(state = finalState)))
    }

object Saga:

  def step[A](
      name: String,
      op: () => Future[Either[String, A]],
      undo: () => Future[Either[String, Unit]]
  ): Saga[A] =
    val runStep: Saga[A] = Saga { state =>
      op().map {
        case Left(err) => Left(SagaFailure(s"$name failed: $err", state.addStep(s"$name failed")))
        case Right(result) => Right((state.addStep(s"$name success"), result))
      }
    }

    val compensation: Compensation = { state =>
      undo().map {
        case Left(err) =>
          Right(state.addStep(s"$name rollback failed: $err"))
        case Right(_) =>
          Right(state.addStep(s"$name rollback success"))
      }
    }

    runStep.withCompensation(compensation)

// 補償不要なステップに使う no-op undo
val noCompensation: () => Future[Either[String, Unit]] =
  () => Future.successful(Right(()))

@main def runCheckoutDemo(): Unit =
  val userId = "user-123" // Mock user ID

  val cart         = CartService()
  val inventory    = InventoryService()
  val payment      = PaymentService()
  val order        = OrderService()
  val shipping     = ShippingService()
  val notification = NotificationService()

  val cartItems = cart.getCartItems(userId)
  val products  = inventory.getProducts(cartItems)

  val flow: Saga[OrderEntity] = for {
    _ <- Saga.step(
      "reserve_inventory",
      () => inventory.reserve(cartItems.toProductIds),
      () => inventory.release(cartItems.toProductIds)
    )
    _ <- Saga.step(
      "charge_payment",
      () => payment.charge(userId, products.totalPrice),
      () => payment.refund(userId, products.totalPrice)
    )
    orderEntity <- Saga.step(
      "create_order",
      () => order.createOrder(userId, cartItems),
      () => order.cancelOrder()
    )
    _ <- Saga.step(
      "create_shipment",
      () => shipping.ship(orderEntity.id),
      () => shipping.cancel(orderEntity.id)
    )
    _ <- Saga.step(
      "send_notification",
      () => notification.sendOrderConfirmation(userId, orderEntity.id),
      noCompensation
    )
  } yield orderEntity

  Await.result(
    flow.exec(SagaState(Vector.empty, Vector.empty)),
    scala.concurrent.duration.Duration.Inf
  ) match
    case Left(error)           => println(s"Checkout failed: ${error.message}")
    case Right((state, order)) => println(s"Checkout succeeded: Order ID ${order.id}")
