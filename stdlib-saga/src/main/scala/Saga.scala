import scala.concurrent.{ExecutionContext, Future}

type Compensation = SagaState => Future[Either[String, SagaState]]

case class SagaState(steps: Vector[String], compensations: Vector[Compensation]):
  def addStep(step: String): SagaState               = copy(steps = steps :+ step)
  def addCompensation(comp: Compensation): SagaState = copy(compensations = compensations :+ comp)
  def stepsSummary: String                           = steps.map(s => s"- $s").mkString("\n")

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

  def withCompensation(undo: Compensation)(using ExecutionContext): Saga[A] =
    Saga { state =>
      run(state).map {
        case (s1, Right(a))  => (s1.addCompensation(undo), Right(a))
        case (s1, Left(err)) => (s1, Left(err))
      }
    }

  def exec(initial: SagaState)(using
      ExecutionContext
  ): Future[(SagaState, Either[String, A])] =
    run(initial).flatMap {
      case success @ (_, Right(_)) => Future.successful(success)
      case (failedState, Left(err)) =>
        failedState.compensations.foldRight(Future.successful(failedState)) { (comp, accState) =>
          accState.flatMap { currentState =>
            comp(currentState).map {
              case Right(nextState) => nextState
              case Left(compErr)    => currentState.addStep(s"compensation failed: $compErr")
            }
          }
        }.map(finalState => (finalState, Left(err)))
    }

object Saga:

  val noCompensation: () => Future[Either[String, Unit]] =
    () => Future.successful(Right(()))

  def step[A](
      name: String,
      op: () => Future[Either[String, A]],
      undo: () => Future[Either[String, Unit]]
  )(using ExecutionContext): Saga[A] =
    val runStep: Saga[A] = Saga { state =>
      op().map {
        case Left(err)     => (state.addStep(s"failed: $name"), Left(s"$name failed: $err"))
        case Right(result) => (state.addStep(s"success: $name"), Right(result))
      }
    }

    val compensation: Compensation = { state =>
      undo().map {
        case Left(err) => Right(state.addStep(s"compensation failed: $name ($err)"))
        case Right(_)  => Right(state.addStep(s"compensation success: $name"))
      }
    }

    runStep.withCompensation(compensation)
