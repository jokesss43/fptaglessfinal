package programs

import models.*
import algebras.*
import cats.Monad
import cats.syntax.all.*

case class MenuItem[F[_]](name: String, exec: F[Unit])

case class Menu[F[_]](header: String, items: List[MenuItem[F]])

class AtmProgram[F[_]](
                        console: ConsoleAlg[F],
                        logic: AtmLogicAlg[F]
                      )(using F: Monad[F]):

  def checkWithdrawRule(balance: Double, alreadyToday: Double, amount: Double): F[Boolean] =
    logic.getContext.map(cfg => amount > 0 && balance >= amount && (alreadyToday + amount) <= cfg.daylim)

  def calculatePlan(amount: Int, cash: Map[Int, Int]): F[Option[Map[Int, Int]]] =
    logic.getContext.map { cfg =>
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
      if (leftover == 0) Some(plan) else None
    }

  def askAmount(user: String, action: Double => F[Unit]): F[Unit] =
    console.putStr("Введите сумму операции: ") *> console.readStr.flatMap { input =>
      input.toDoubleOption match {
        case Some(amt) => action(amt)
        case None      => console.putStrLn("Введите корректное число!") *> mainMenuLoop(user)
      }
    }

  def handleWithdraw(user: String): F[Unit] =
    askAmount(user, amt =>
      for {
        state <- logic.getState
        bal   = state.balances.getOrElse(user, 0.0)
        today = state.todayWithdraw.getOrElse(user, 0.0)
        isOk  <- checkWithdrawRule(bal, today, amt)
        _     <- if (!isOk) then console.putStrLn("Ошибка: Проверьте баланс или лимит!") *> mainMenuLoop(user)
        else processWithdrawal(user, amt.toInt, state)
      } yield ()
    )

  def processWithdrawal(user: String, amount: Int, state: AtmState): F[Unit] =
    calculatePlan(amount, state.cashInMachine).flatMap {
      case Some(plan) =>
        logic.updateState { st =>
          val nextCash = st.cashInMachine.map { (n, c) => (n, c - plan.getOrElse(n, 0)) }
          val currentToday = st.todayWithdraw.getOrElse(user, 0.0)
          st.copy(
            balances = st.balances.updated(user, st.balances.getOrElse(user, 0.0) - amount),
            cashInMachine = nextCash,
            todayWithdraw = st.todayWithdraw.updated(user, currentToday + amount)
          )
        } *> console.putStrLn("ЧЕК ОПЕРАЦИИ") *>
          plan.map((n, c) => s"Выдано купюр номинала $n: $c шт.").toList.traverse(console.putStrLn) *>
          mainMenuLoop(user)
      case None =>
        console.putStrLn("Ошибка: В банкомате нет подходящих купюр!") *> mainMenuLoop(user)
    }

  def handleDeposit(user: String): F[Unit] =
    askAmount(user, amt =>
      if (amt <= 0) then
        console.putStrLn("Сумма должна быть больше нуля!") *> mainMenuLoop(user)
      else
        logic.updateState { st =>
          val old = st.balances.getOrElse(user, 0.0)
          st.copy(balances = st.balances.updated(user, old + amt))
        } *> console.putStrLn(s"Успешно зачислено: $amt руб.") *> mainMenuLoop(user)
    )

  def handleTransfer(user: String): F[Unit] =
    console.putStr("Введите имя получателя: ") *> console.readStr.flatMap { toUser =>
      logic.userExists(toUser).flatMap {
        case false => console.putStrLn("Получатель не найден!") *> mainMenuLoop(user)
        case true if user == toUser => console.putStrLn("Нельзя переводить самому себе!") *> mainMenuLoop(user)
        case true => askAmount(user, amt => processTransfer(user, toUser, amt))
      }
    }

  def processTransfer(user: String, toUser: String, amt: Double): F[Unit] =
    if (amt <= 0) then
      console.putStrLn("Некорректная сумма!") *> mainMenuLoop(user)
    else
      for {
        cfg   <- logic.getContext
        state <- logic.getState
        fee   = amt * (cfg.comission / 100.0)
        total = amt + fee
        fromB = state.balances.getOrElse(user, 0.0)
        toB   = state.balances.getOrElse(toUser, 0.0)
        _     <- if (fromB < total) then
          console.putStrLn(s"Недостаточно средств! Требуется с комиссией: $total руб.") *> mainMenuLoop(user)
        else
          logic.updateState { st =>
            st.copy(balances = st.balances.updated(user, fromB - total).updated(toUser, toB + amt))
          } *> console.putStrLn(s"Успешный перевод $amt руб. (Комиссия: $fee руб.)") *> mainMenuLoop(user)
      } yield ()


  def run: F[Unit] = userMenuLoop

  def userMenuLoop: F[Unit] =
    val startMenu = Menu[F](
      "\nДОБРО ПОЖАЛОВАТЬ В БАНКОМАТ",
      List(
        MenuItem("Войти в аккаунт",       console.putStr("Введите имя: ") *> console.readStr.flatMap(handleLogin)),
        MenuItem("Сбросить лимиты суток", logic.updateState(_.copy(todayWithdraw = Map.empty)) *>
          console.putStrLn("> Лимиты суток сброшены.") *> userMenuLoop),
        MenuItem("Завершить работу",       console.putStrLn("Программа завершена."))
      )
    )

    for {
      state <- logic.getState
      _     <- console.putStrLn(startMenu.header)
      _     <- console.putStrLn(s"Зарегистрированные пользователи: ${state.balances.keys.mkString(", ")}")
      _     <- startMenu.items.zipWithIndex.map((item, i) => s"  [${i + 1}] ${item.name}").traverse(console.putStrLn)
      _     <- console.putStr("> ")
      choice <- console.readStr
      index = choice.trim.toIntOption.map(_ - 1).getOrElse(-1)
      _     <- if (index >= 0 && index < startMenu.items.length) then
        startMenu.items(index).exec
      else
        console.putStrLn("Неверный выбор. Попробуйте снова.") *> userMenuLoop
    } yield ()

  def handleLogin(name: String): F[Unit] =
    logic.userExists(name.trim).flatMap {
      case true  => mainMenuLoop(name.trim)
      case false => console.putStrLn("Пользователь не найден!") *> userMenuLoop
    }

  def mainMenuLoop(user: String): F[Unit] =
    val atmMenu = Menu[F](
      "ГЛАВНОЕ МЕНЮ",
      List(
        MenuItem("Снять наличные",       handleWithdraw(user)),
        MenuItem("Пополнить счет",       handleDeposit(user)),
        MenuItem("Сделать перевод",      handleTransfer(user)),
        MenuItem("Выйти из аккаунта",    console.putStrLn(s"Сессия $user завершена.") *> userMenuLoop)
      )
    )

    for {
      state <- logic.getState
      bal   = state.balances.getOrElse(user, 0.0)
      _     <- console.putStrLn(s"\nАккаунт: $user | Доступно: $bal руб.")
      _     <- console.putStrLn(atmMenu.header)
      _     <- atmMenu.items.zipWithIndex.map((item, i) => s"  [${i + 1}] ${item.name}").traverse(console.putStrLn)
      _     <- console.putStr("Ваш выбор: ")
      choice <- console.readStr
      index = choice.trim.toIntOption.map(_ - 1).getOrElse(-1)
      _     <- if (index >= 0 && index < atmMenu.items.length) then
        atmMenu.items(index).exec
      else
        console.putStrLn("Неверный выбор. Попробуйте снова.") *> mainMenuLoop(user)
    } yield ()