import models.*
import algebras.*
import interpreters.*
import interpreters.given
import programs.AtmProgram

object MainApp:

  def main(args: Array[String]): Unit =
    // Компилятор теперь без проблем увидит здесь неявный экземпляр Monad
    given consoleImpl: ConsoleAlg[AtmStack] = new ConsoleInterpreter
    given logicImpl: AtmLogicAlg[AtmStack]   = new LogicInterpreter

    val atmProgram = new AtmProgram[AtmStack]

    val config = AtmConfig(
      daylim = 10000.0,
      comission = 2.0,
      banknotes = List(100, 200, 500, 1000, 2000),
      round = false
    )

    val startState = AtmState(
      balances = Map("Ivan" -> 15000.0, "Masha" -> 3000.0, "Oleg" -> 500.0),
      cashInMachine = Map(2000 -> 5, 1000 -> 10, 500 -> 10, 200 -> 15, 100 -> 20),
      todayWithdraw = Map("Ivan" -> 2000.0)
    )

    val appStateIO = atmProgram.userMenu().run(config).run(startState)._1
    appStateIO.unsafeRun()