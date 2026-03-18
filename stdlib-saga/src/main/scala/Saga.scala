import scala.concurrent.{ExecutionContext, Future}

type Compensation = SagaState => Future[Either[String, SagaState]]

enum SagaOutcome[+A]:
  case Succeeded(state: SagaState, value: A)
  case Compensated(state: SagaState, error: String)
  case CompensationFailed(state: SagaState, error: String, compensationError: String)

case class SagaState(steps: Vector[String], compensations: Vector[Compensation]):
  def addStep(step: String): SagaState               = copy(steps = steps :+ step)
  def addCompensation(comp: Compensation): SagaState = copy(compensations = compensations :+ comp)
  def stepsSummary: String                           = steps.map(s => s"- $s").mkString("\n")

  def compensate()(using ExecutionContext): Future[Either[String, SagaState]] =
    compensations.foldRight(Future.successful(Right(this): Either[String, SagaState])) {
      (comp, accState) =>
        accState.flatMap { result =>
          result.fold(
            err => Future.successful(Left(err)),
            currentState =>
              comp(currentState).map {
                case Left(err)   => Right(currentState.addStep(s"compensation failed: $err"))
                case Right(next) => Right(next)
              }
          )
        }
    }

object SagaState:
  def empty: SagaState = SagaState(Vector.empty, Vector.empty)

case class Saga[A](run: SagaState => Future[(SagaState, Either[String, A])]):

  def map[B](f: A => B)(using ExecutionContext): Saga[B] =
    Saga(state => run(state).map { case (s, result) => (s, result.map(f)) })

  def flatMap[B](f: A => Saga[B])(using ExecutionContext): Saga[B] =
    Saga { state =>
      run(state).flatMap {
        case (s1, Right(a))  => f(a).run(s1)
        case (s1, Left(err)) => Future.successful((s1, Left(err)))
      }
    }

  def exec(initial: SagaState)(using
      ExecutionContext
  ): Future[SagaOutcome[A]] =
    run(initial).flatMap {
      case (state, Right(value)) =>
        Future.successful(SagaOutcome.Succeeded(state, value))
      case (state, Left(error)) =>
        state.compensate().map {
          case Right(finalState) =>
            SagaOutcome.Compensated(finalState, error)
          case Left(compensationError) =>
            SagaOutcome.CompensationFailed(state, error, compensationError)
        }
    }

object Saga:

  val noCompensation: () => Future[Either[String, Unit]] =
    () => Future.successful(Right(()))

  def step[A](
      name: String,
      op: () => Future[Either[String, A]],
      undo: () => Future[Either[String, Unit]]
  )(using ExecutionContext): Saga[A] =
    val compensation: Compensation = { state =>
      undo().map {
        case Left(err) => Right(state.addStep(s"compensation failed: $name ($err)"))
        case Right(_)  => Right(state.addStep(s"compensation success: $name"))
      }
    }

    Saga { state =>
      op().map {
        case Left(err) => (state.addStep(s"failed: $name"), Left(s"$name failed: $err"))
        case Right(result) =>
          (state.addStep(s"success: $name").addCompensation(compensation), Right(result))
      }
    }
