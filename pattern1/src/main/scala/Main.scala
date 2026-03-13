import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.concurrent.Await

given ExecutionContext = ExecutionContext.global

type Compensation = SagaState => Future[Either[String, SagaState]]

case class SagaState(steps: Vector[String], compensations: Vector[Compensation]):
  def addStep(step: String): SagaState = copy(steps = steps :+ step)
  def addCompensation(comp: Compensation): SagaState = copy(compensations = compensations :+ comp)

case class SagaFailure(message: String, state: SagaState)

case class Saga[A](run: SagaState => Future[Either[SagaFailure, (SagaState, A)]]):
  def map[B](f: A => B): Saga[B] =
    Saga(state => run(state).map(_.map { case (s, a) => (s, f(a)) }))

  def flatMap[B](f: A => Saga[B]): Saga[B] =
    Saga { state =>
      run(state).flatMap {
        case Right((s1, a)) => f(a).run(s1)
        case Left(err) => Future.successful(Left(err))
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
        err.state.compensations.foldRight(Future.successful(())) { (comp, acc) =>
          acc.flatMap(_ => comp(err.state).map(_ => ()))
        }.map(_ => Left(err))
    }

object Saga:
  def step[A](name: String, op: () => Future[Either[String, A]], undo: () => Future[Either[String, Unit]]): Saga[A] =
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

// --- ドメインモデル ---

case class CartItem(productId: String, quantity: Int)

case class CartItems(items: List[CartItem]):
  def toProductIds: List[String] = items.map(_.productId)

case class Product(id: String, price: Double)

case class Products(products: List[Product]):
  def totalPrice: Double = products.map(_.price).sum

case class OrderEntity(id: String, userId: String, items: CartItems)

// 補償不要なステップに使う no-op undo
val noCompensation: () => Future[Either[String, Unit]] =
  () => Future.successful(Right(()))

// --- モック共通設定 ---

object MockConfig:
  val failureRate: Double = 0.25

def callWithLog(service: String, method: String, args: String = "", canFail: Boolean = true): Either[String, Unit] =
  val argStr = if args.nonEmpty then s"($args)" else "()"
  println(s"[${service}] ${method}${argStr} を呼び出しました")
  if canFail && scala.util.Random.nextDouble() < MockConfig.failureRate then
    val msg = s"${service}.${method}: 障害が発生しました"
    println(s"[${service}] ${method} 障害発生")
    Left(msg)
  else
    Right(())

// --- サービス実装（モック） ---

final class CartService:
  def getCartItems(userId: String): CartItems =
    println(s"[CartService] getCartItems(userId=$userId) を呼び出しました")
    CartItems(List(CartItem("product-A", 2), CartItem("product-B", 1)))

final class InventoryService:
  private var reserved: Set[String] = Set.empty

  def getProducts(cartItems: CartItems): Products =
    println(s"[InventoryService] getProducts(${cartItems.items.size}件) を呼び出しました")
    Products(cartItems.items.map(item => Product(item.productId, 500.0)))

  def reserve(productIds: List[String]): Future[Either[String, Unit]] = Future {
    callWithLog("InventoryService", "reserve", s"productIds=${productIds.mkString(",")}") match
      case Left(err) => Left(err)
      case Right(_) =>
        reserved = reserved ++ productIds
        Right(())
  }

  def release(productIds: List[String]): Future[Either[String, Unit]] = Future {
    callWithLog("InventoryService", "release", s"productIds=${productIds.mkString(",")}", canFail = false) match
      case Left(err) => Left(err)
      case Right(_) =>
        reserved = reserved -- productIds
        Right(())
  }

final class PaymentService:
  private var charged = false

  def charge(userId: String, amount: Double): Future[Either[String, Unit]] = Future {
    callWithLog("PaymentService", "charge", s"userId=$userId, amount=$amount") match
      case Left(err) => Left(err)
      case Right(_) =>
        if charged then Left("already charged")
        else
          charged = true
          Right(())
  }

  def refund(userId: String, amount: Double): Future[Either[String, Unit]] = Future {
    callWithLog("PaymentService", "refund", s"userId=$userId, amount=$amount", canFail = false) match
      case Left(err) => Left(err)
      case Right(_) =>
        if charged then charged = false
        Right(())
  }

final class OrderService:
  private var lastOrder: Option[OrderEntity] = None

  def createOrder(userId: String, cartItems: CartItems): Future[Either[String, OrderEntity]] = Future {
    callWithLog("OrderService", "createOrder", s"userId=$userId, items=${cartItems.items.size}件") match
      case Left(err) => Left(err)
      case Right(_) =>
        val entity = OrderEntity(java.util.UUID.randomUUID().toString, userId, cartItems)
        lastOrder = Some(entity)
        Right(entity)
  }

  def cancelOrder(): Future[Either[String, Unit]] = Future {
    callWithLog("OrderService", "cancelOrder", canFail = false) match
      case Left(err) => Left(err)
      case Right(_) =>
        lastOrder = None
        Right(())
  }

final class ShippingService:
  private var shipped = false

  def ship(orderId: String): Future[Either[String, Unit]] = Future {
    callWithLog("ShippingService", "ship", s"orderId=$orderId") match
      case Left(err) => Left(err)
      case Right(_) =>
        if shipped then Left("already shipped")
        else
          shipped = true
          Right(())
  }

  def cancel(orderId: String): Future[Either[String, Unit]] = Future {
    callWithLog("ShippingService", "cancel", s"orderId=$orderId", canFail = false) match
      case Left(err) => Left(err)
      case Right(_) =>
        if shipped then shipped = false
        Right(())
  }

final class NotificationService:
  def sendOrderConfirmation(userId: String, orderId: String): Future[Either[String, Unit]] = Future {
    callWithLog("NotificationService", "sendOrderConfirmation", s"userId=$userId, orderId=$orderId") match
      case Left(err) => Left(err)
      case Right(_) => Right(())
  }

@main def runCheckoutDemo(): Unit =
  val userId = "user-123" // Mock user ID

  val cart = CartService()
  val inventory = InventoryService()
  val payment = PaymentService()
  val order = OrderService()
  val shipping = ShippingService()
  val notification = NotificationService()

  val cartItems = cart.getCartItems(userId)
  val products = inventory.getProducts(cartItems)

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
    case Left(error) => println(s"Checkout failed: ${error.message}")
    case Right((state, order)) => println(s"Checkout succeeded: Order ID ${order.id}")
