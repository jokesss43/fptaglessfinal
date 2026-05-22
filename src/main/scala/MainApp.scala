import models.*
import algebras.*
import interpreters.*
import programs.AtmProgram
import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global
import cats.Monad

@main def main(): Unit =
  val config = AtmConfig(
    daylim = 10000.0,
    comission = 2.0,
    banknotes = List(100, 200, 500, 1000, 2000),
    round = false
  )

  val startState = AtmState(
    balances = Map("Ivan" -> 15000.0, "Masha" -> 3000.0, "Oleg" -> 500.0),
    cashInMachine = Map(2000 -> 5, 1000 -> 10, 500 -> 10, 200 -> 15, 100 -> 20),
    todayWithdraw = Map("Ivan" -> 0.0)
  )

  val stateRef = Ref.unsafe[IO, AtmState](startState)

  val consoleAlg: ConsoleAlg[IO] = new ConsoleInterpreter
  val logicAlg: AtmLogicAlg[IO]   = new LogicInterpreter(config, stateRef)

  val program = new AtmProgram[IO](consoleAlg, logicAlg)(
    using summon[Monad[IO]]
  )

  program.run.unsafeRunSync()