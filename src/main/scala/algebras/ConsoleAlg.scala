package algebras

trait ConsoleAlg[F[_]]:
  def putStrLn(s: String): F[Unit]
  def putStr(s: String): F[Unit]
  def readStr: F[String]