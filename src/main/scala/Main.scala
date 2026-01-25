import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

given ExecutionContext = ExecutionContext.global

final case class SagaState(log: Vector[String], compensations: List[Compensation]):
  def addLog(entry: String): SagaState = copy(log = log :+ entry)
  def addCompensation(comp: Compensation): SagaState = copy(compensations = comp :: compensations)

final case class SagaFailure(message: String, state: SagaState)

type Compensation = SagaState => Future[Either[String, SagaState]]

final case class Saga[A](run: SagaState => Future[Either[SagaFailure, (SagaState, A)]]):
  def map[B](f: A => B): Saga[B] =
    Saga(state => run(state).map(_.map { case (s, a) => (s, f(a)) }))

  def flatMap[B](f: A => Saga[B]): Saga[B] =
    Saga { state =>
      run(state).flatMap {
        case Left(err) => Future.successful(Left(err))
        case Right((s1, a)) => f(a).run(s1)
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

object Saga:
  def pure[A](value: A): Saga[A] = Saga(state => Future.successful(Right((state, value))))

  def step(name: String)(op: => Future[Either[String, Unit]])(undo: => Future[Either[String, Unit]]): Saga[Unit] =
    val runStep: Saga[Unit] = Saga { state =>
      op.map {
        case Left(err) => Left(SagaFailure(s"$name failed: $err", state.addLog(s"$name failed")))
        case Right(_) => Right((state.addLog(s"$name success"), ()))
      }
    }

    val compensation: Compensation = { state =>
      undo.map {
        case Left(err) =>
          Right(state.addLog(s"$name rollback failed: $err"))
        case Right(_) =>
          Right(state.addLog(s"$name rollback success"))
      }
    }

    runStep.withCompensation(compensation)

object SagaRunner:
  def execute[A](saga: Saga[A], initial: SagaState): Future[Either[SagaFailure, (SagaState, A)]] =
    saga.run(initial).flatMap {
      case Right(result) => Future.successful(Right(result))
      case Left(err) =>
        rollback(err.state).map { rolledBack =>
          Left(err.copy(state = rolledBack))
        }
    }

  private def rollback(state: SagaState): Future[SagaState] =
    state.compensations.foldLeft(Future.successful(state)) { (acc, comp) =>
      acc.flatMap { s =>
        comp(s).map(_.getOrElse(s))
      }
    }

final class InventoryService:
  private var reserved = false

  def reserve(): Future[Either[String, Unit]] = Future {
    if reserved then Left("already reserved")
    else
      reserved = true
      Right(())
  }

  def release(): Future[Either[String, Unit]] = Future {
    if reserved then
      reserved = false
      Right(())
    else Right(())
  }

final class PaymentService(shouldFail: Boolean):
  private var charged = false

  def charge(): Future[Either[String, Unit]] = Future {
    if charged then Left("already charged")
    else if shouldFail then Left("payment rejected")
    else
      charged = true
      Right(())
  }

  def refund(): Future[Either[String, Unit]] = Future {
    if charged then
      charged = false
      Right(())
    else Right(())
  }

final class ShippingService:
  private var shipped = false

  def ship(): Future[Either[String, Unit]] = Future {
    if shipped then Left("already shipped")
    else
      shipped = true
      Right(())
  }

  def cancel(): Future[Either[String, Unit]] = Future {
    if shipped then
      shipped = false
      Right(())
    else Right(())
  }

@main def runSagaDemo(): Unit =
  val inventory = InventoryService()
  val payment = PaymentService(shouldFail = false)
  val shipping = ShippingService()

  val placeOrder: Saga[Unit] = for
    _ <- Saga.step("reserve inventory")(inventory.reserve())(inventory.release())
    _ <- Saga.step("charge payment")(payment.charge())(payment.refund())
    _ <- Saga.step("create shipment")(shipping.ship())(shipping.cancel())
  yield ()

  val notifyCustomer: Saga[Unit] =
    Saga.step("send email")(Future.successful(Right(())))(Future.successful(Right(())))

  val fullFlow: Saga[Unit] = for
    _ <- placeOrder
    _ <- notifyCustomer
  yield ()

  val initial = SagaState(log = Vector.empty, compensations = Nil)
  SagaRunner.execute(fullFlow, initial).onComplete {
    case Success(Right((state, _))) =>
      println("Saga success")
      state.log.foreach(println)
    case Success(Left(failure)) =>
      println(s"Saga failed: ${failure.message}")
      failure.state.log.foreach(println)
    case Failure(err) =>
      println(s"Unexpected error: ${err.getMessage}")
  }
