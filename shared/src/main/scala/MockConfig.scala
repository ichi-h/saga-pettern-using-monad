package mock

object MockConfig:
  val failureRate: Double = 0.25

def callWithLog(
    service: String,
    method: String,
    args: String = "",
    canFail: Boolean = true
): Either[String, Unit] =
  val argStr = if args.nonEmpty then s"($args)" else "()"
  println(s"[$service] $method$argStr を呼び出しました")
  if canFail && scala.util.Random.nextDouble() < MockConfig.failureRate then
    val msg = s"$service.$method: 障害が発生しました"
    println(s"[$service] $method 障害発生")
    Left(msg)
  else
    Right(())
