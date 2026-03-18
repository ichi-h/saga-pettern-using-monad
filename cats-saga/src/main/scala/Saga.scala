import cats.data.{EitherT, StateT}
import cats.effect.IO

type Compensation = SagaState => EitherT[IO, String, SagaState]

enum SagaOutcome[+A]:
  case Succeeded(state: SagaState, value: A)
  case Compensated(state: SagaState, error: String)
  case CompensationFailed(state: SagaState, error: String, compensationError: String)

case class SagaState(steps: Vector[String], compensations: Vector[Compensation]):
  def addStep(step: String): SagaState               = copy(steps = steps :+ step)
  def addCompensation(comp: Compensation): SagaState = copy(compensations = compensations :+ comp)

  def stepsSummary: String = steps.map(s => s"- $s").mkString("\n")

  def compensate(): EitherT[IO, String, SagaState] =
    compensations.foldRight(EitherT.rightT[IO, String](this)) { (comp, accState) =>
      accState.flatMap(comp)
    }

object SagaState:
  def empty: SagaState = SagaState(Vector.empty, Vector.empty)

type Saga[A] = EitherT[[X] =>> StateT[IO, SagaState, X], String, A]

object Saga:

  val noCompensation: () => EitherT[IO, String, Unit] =
    () => EitherT.rightT(())

  def execWithCompensation[A](
      flow: Saga[A],
      initialState: SagaState = SagaState.empty
  ): IO[SagaOutcome[A]] =
    flow.value.run(initialState).flatMap {
      case (state, Right(value)) =>
        IO.pure(SagaOutcome.Succeeded(state, value))
      case (state, Left(error)) =>
        state.compensate().value.map {
          case Right(finalState) =>
            SagaOutcome.Compensated(finalState, error)
          case Left(compensationError) =>
            SagaOutcome.CompensationFailed(state, error, compensationError)
        }
    }

  def step[A](
      name: String,
      op: () => EitherT[IO, String, A],
      undo: () => EitherT[IO, String, Unit]
  ): EitherT[[X] =>> StateT[IO, SagaState, X], String, A] =
    val undoComp: Compensation = state =>
      EitherT {
        undo().value.map {
          case Left(err) =>
            Left(s"compensation failed: $name ($err)"): Either[String, SagaState]
          case Right(_) =>
            Right(state.addStep(s"compensation success: $name")): Either[String, SagaState]
        }
      }

    EitherT {
      StateT { (state: SagaState) =>
        op().value.map {
          case r @ Right(_) =>
            (state.addStep(s"success: $name").addCompensation(undoComp), r)
          case l @ Left(err) => (state.addStep(s"failed: $name ($err)"), l)
        }
      }
    }
