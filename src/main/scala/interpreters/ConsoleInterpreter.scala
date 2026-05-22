package interpreters

import algebras.ConsoleAlg
import cats.effect.IO

class ConsoleInterpreter extends ConsoleAlg[IO]:
  def putStrLn(s: String): IO[Unit] = IO.println(s)
  def putStr(s: String): IO[Unit]   = IO.print(s)
  def readStr: IO[String]           = IO.blocking(scala.io.StdIn.readLine())