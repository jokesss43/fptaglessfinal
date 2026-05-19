package interpreters

import algebras.ConsoleAlg
import models.{Reader, State, IO}

class ConsoleInterpreter extends ConsoleAlg[AtmStack]:
  def putStrLn(s: String): AtmStack[Unit] =
    Reader(_ => State(st => (IO.delay(println(s)), st)))

  def putStr(s: String): AtmStack[Unit] =
    Reader(_ => State(st => (IO.delay(print(s)), st)))

  def readStr: AtmStack[String] =
    Reader(_ => State(st => (IO.delay(scala.io.StdIn.readLine()), st)))