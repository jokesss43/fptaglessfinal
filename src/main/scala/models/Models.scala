package models

case class AtmConfig(daylim: Double, comission: Double, banknotes: List[Int], round: Boolean)
case class AtmState(balances: Map[String, Double], cashInMachine: Map[Int, Int], todayWithdraw: Map[String, Double])
case class Writer[A](log: List[String], value: A)

case class Reader[Env, A](run: Env => A)
case class State[S, A](run: S => (A, S))

case class IO[A](unsafeRun: () => A):
  def map[B](f: A => B): IO[B] =
    IO(() => f(unsafeRun()))

  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(() => f(unsafeRun()).unsafeRun())

object IO:
  def pure[A](a: A): IO[A] = IO(() => a)
  def delay[A](eff: => A): IO[A] = IO(() => eff)

trait Monad[F[_]]:
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))