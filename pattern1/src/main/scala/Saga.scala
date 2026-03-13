import scala.concurrent.{ExecutionContext, Future}

type Compensation = SagaState => Future[Either[String, SagaState]]

case class SagaState(steps: Vector[String], compensations: Vector[Compensation]):
  def addStep(step: String): SagaState               = copy(steps = steps :+ step)
  def addCompensation(comp: Compensation): SagaState = copy(compensations = compensations :+ comp)

object SagaState:
  def empty: SagaState = SagaState(Vector.empty, Vector.empty)

case class SagaFailure(message: String, state: SagaState)

case class Saga[A](run: SagaState => Future[Either[SagaFailure, (SagaState, A)]]):

  def map[B](f: A => B)(using ExecutionContext): Saga[B] =
    Saga(state => run(state).map(_.map { case (s, a) => (s, f(a)) }))

  def flatMap[B](f: A => Saga[B])(using ExecutionContext): Saga[B] =
    Saga { state =>
      run(state).flatMap {
        case Right((s1, a)) => f(a).run(s1)
        case Left(err)      => Future.successful(Left(err))
      }
    }

  def withCompensation(undo: Compensation)(using ExecutionContext): Saga[A] =
    Saga { state =>
      run(state).map(_.map { case (s1, a) =>
        (s1.addCompensation(undo), a)
      })
    }

  def exec(initial: SagaState)(using
      ExecutionContext
  ): Future[Either[SagaFailure, (SagaState, A)]] =
    run(initial).flatMap {
      case r @ Right(_) => Future.successful(r)
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

  val noCompensation: () => Future[Either[String, Unit]] =
    () => Future.successful(Right(()))

  def step[A](
      name: String,
      op: () => Future[Either[String, A]],
      undo: () => Future[Either[String, Unit]]
  )(using ExecutionContext): Saga[A] =
    val runStep: Saga[A] = Saga { state =>
      op().map {
        case Left(err) => Left(SagaFailure(s"$name failed: $err", state.addStep(s"$name failed")))
        case Right(result) => Right((state.addStep(s"$name success"), result))
      }
    }

    val compensation: Compensation = { state =>
      undo().map {
        case Left(err) => Right(state.addStep(s"$name rollback failed: $err"))
        case Right(_)  => Right(state.addStep(s"$name rollback success"))
      }
    }

    runStep.withCompensation(compensation)
