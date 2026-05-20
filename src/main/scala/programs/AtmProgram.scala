package programs

import models.*
import algebras.*
import interpreters.* 

class AtmProgram[F[_]](using F: Monad[F], console: ConsoleAlg[F], logic: AtmLogicAlg[F]):

  case class MenuItem(name: String, exec: (String, AtmState) => F[Unit])

  case class Menu(header: String, items: Seq[MenuItem]):
    def show: String =

      val lines = items.zipWithIndex.map { (item, i) =>
        s"${i + 1}. ${item.name}"
      }
      s"$header\n" + lines.mkString("\n")

    def handleInput(user: String, state: AtmState, input: String): F[Unit] =
      val index = input.toIntOption.map(_ - 1).getOrElse(-1)
      if (index >= 0 && index < items.length)
        items(index).exec(user, state)
      else
        console.putStrLn("Неверный выбор. Попробуйте снова.")
          .flatMap(_ => mainMenu(user))

  val atmMenu = Menu(
    "ГЛАВНОЕ МЕНЮ",
    Seq(
      MenuItem("Снять наличные", (u, s) => doWithdraw(u)),
      MenuItem("Пополнить счет", (u, s) => doDeposit(u)),
      MenuItem("Сделать перевод", (u, s) => doTransfer(u)), 
      MenuItem("Сменить пользователя", (u, s) => userMenu())
    )
  )

  def checkWithdrawRule(balance: Double, alreadyToday: Double, amount: Double): F[Writer[Boolean]] =
    for {
      cfg <- logic.getContext
    } yield {
      val r1 = amount > 0
      val r2 = balance >= amount
      val r3 = (alreadyToday + amount) <= cfg.daylim
      Writer(
        List(
          if (r1) "Сумма корректна" else "Сумма должна быть больше нуля",
          if (r2) "Баланс проверен" else "Недостаточно денег",
          if (r3) "Лимит в норме" else "Превышен лимит"
        ),
        r1 && r2 && r3
      )
    }

  def calculatePlan(amount: Int, cash: Map[Int, Int]): F[Writer[Option[Map[Int, Int]]]] =
    for {
      cfg <- logic.getContext
    } yield {
      val sortedNotes = cfg.banknotes.sorted.reverse
      val (leftover, plan) = sortedNotes.foldLeft((amount, Map.empty[Int, Int])) { (acc, banknote) =>
        val left = acc._1
        val currentPlan = acc._2
        val cashIn = cash.getOrElse(banknote, 0)
        val needed = left / banknote
        val toGive = if (needed < cashIn) needed else cashIn

        if (toGive > 0) (left - (toGive * banknote), currentPlan + (banknote -> toGive))
        else (left, currentPlan)
      }

      if (leftover == 0)
        val lines = plan.map((note, count) => s"Купюры $note руб. -> $count шт.").toList
        Writer(List("План выдачи успешно составлен") ++ lines, Some(plan))
      else
        Writer(List("В банкомате нет нужных купюр для выдачи такой суммы"), None)
    }

  def printLines(logs: List[String]): F[Unit] =
    for {
      _ <- console.putStrLn("ЧЕК ОПЕРАЦИИ")
      _ <- logs.foldLeft(F.pure(()))((acc, line) => acc.flatMap(_ => console.putStrLn(s"> $line")))
    } yield ()

  def userMenu(): F[Unit] =
    for {
      state <- logic.getState
      _     <- console.putStrLn(s"\nПользователи: ${state.balances.keys.mkString(", ")}")
      _     <- console.putStr("Введите имя (или 'exit' / 'next'): ")
      input <- console.readStr
      _     <- input match {
        case "exit" => console.putStrLn("Программа завершена.")
        case "next" =>
          for {
            _ <- logic.updateState(_.copy(todayWithdraw = Map.empty))
            _ <- console.putStrLn("> Наступил новый день. Лимиты сброшены.")
            _ <- userMenu()
          } yield ()
        case name if state.balances.contains(name) =>
          mainMenu(name)
        case _ =>
          for {
            _ <- console.putStrLn("Пользователь не найден!")
            _ <- userMenu()
          } yield ()
      }
    } yield ()

  def mainMenu(user: String): F[Unit] =
    for {
      state <- logic.getState
      bal   = state.balances.getOrElse(user, 0.0)
      _     <- console.putStrLn(s"\n👤 Аккаунт: $user | 💰 Доступно: $bal руб.")
      _     <- console.putStrLn(atmMenu.show)
      _     <- console.putStr("Ваш выбор: ")
      choice <- console.readStr
      _     <- atmMenu.handleInput(user, state, choice)
    } yield ()

  def doWithdraw(user: String): F[Unit] =
    for {
      _       <- console.putStr("Введите сумму: ")
      input   <- console.readStr
      amount  = input.toIntOption.getOrElse(0)
      state   <- logic.getState
      bal     = state.balances.getOrElse(user, 0.0)
      today   = state.todayWithdraw.getOrElse(user, 0.0)
      chk     <- checkWithdrawRule(bal, today, amount.toDouble)
      _       <- if (!chk.value) printLines(chk.log).flatMap(_ => mainMenu(user))
      else for {
        planChk <- calculatePlan(amount, state.cashInMachine)
        _       <- planChk.value match {
          case Some(plan) =>
            for {
              _ <- logic.updateState { st =>
                val oldB = st.balances.getOrElse(user, 0.0)
                val oldW = st.todayWithdraw.getOrElse(user, 0.0)
                val nextCash = st.cashInMachine.map { (n, c) => (n, c - plan.getOrElse(n, 0)) }
                st.copy(
                  balances = st.balances.updated(user, oldB - amount),
                  cashInMachine = nextCash,
                  todayWithdraw = st.todayWithdraw.updated(user, oldW + amount)
                )
              }
              _ <- printLines(chk.log ++ planChk.log :+ s"Выдано: $amount")
              _ <- mainMenu(user)
            } yield ()
          case None =>
            printLines(chk.log ++ planChk.log).flatMap(_ => mainMenu(user))
        }
      } yield ()
    } yield ()

  def doDeposit(user: String): F[Unit] =
    for {
      _      <- console.putStr("Сумма пополнения: ")
      input  <- console.readStr
      amount = input.toDoubleOption.getOrElse(0.0)
      _      <- if (amount <= 0) console.putStrLn("Ошибка суммы!").flatMap(_ => mainMenu(user))
      else for {
        _ <- logic.updateState { st =>
          val old = st.balances.getOrElse(user, 0.0)
          st.copy(balances = st.balances.updated(user, old + amount))
        }
        _ <- printLines(List(s"Успешное пополнение на $amount"))
        _ <- mainMenu(user)
      } yield ()
    } yield ()

  def doTransfer(user: String): F[Unit] =
    for {
      _      <- console.putStr("Имя получателя: ")
      toUser <- console.readStr
      _      <- handleTransferTarget(toUser, user)
    } yield ()

  def handleTransferTarget(toUser: String, user: String): F[Unit] =
    for {
      exists <- logic.userExists(toUser)
      _      <- if (!exists)
        console.putStrLn("Такого пользователя нет!").flatMap(_ => mainMenu(user))
      else if (user == toUser)
        console.putStrLn("Ошибка: перевод самому себе невозможен.").flatMap(_ => mainMenu(user))
      else
        for {
          _     <- console.putStr("Сумма перевода: ")
          input <- console.readStr
          _     <- processTransfer(input, user, toUser)
        } yield ()
    } yield ()

  def processTransfer(input: String, user: String, toUser: String): F[Unit] =
    val amount = input.toDoubleOption.getOrElse(0.0)
    if (amount <= 0) then
      console.putStrLn("Сумма перевода должна быть больше нуля!").flatMap(_ => mainMenu(user))
    else
      for {
        cfg   <- logic.getContext
        state <- logic.getState

        fee = amount * (cfg.comission / 100.0)
        totalAmount = amount + fee

        fromBalance = state.balances.getOrElse(user, 0.0)

        _ <- if (fromBalance >= totalAmount) then
          for {
            _ <- logic.updateState { st =>
              val toBalance = st.balances.getOrElse(toUser, 0.0)
              val updatedBalances = st.balances
                .updated(user, fromBalance - totalAmount)
                .updated(toUser, toBalance + amount)
              st.copy(balances = updatedBalances)
            }
            _ <- printLines(List(s"Перевод совершен: $amount отдан $toUser. Списана комиссия $fee"))
            _ <- mainMenu(user)
          } yield ()
        else
          for {
            _ <- printLines(List("Недостаточно средств для перевода и комиссии"))
            _ <- mainMenu(user)
          } yield ()
      } yield ()
