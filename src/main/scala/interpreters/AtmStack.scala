package interpreters

import models.*

type AtmStack[A] = Reader[AtmConfig, State[AtmState, IO[A]]]

given atmMonad: Monad[AtmStack] with
  def pure[A](a: A): AtmStack[A] =
    Reader(_ => State(s => (IO.pure(a), s)))

  def flatMap[A, B](fa: AtmStack[A])(f: A => AtmStack[B]): AtmStack[B] =
    Reader { cfg =>
      State { s =>
        val (ioA, nextS) = fa.run(cfg).run(s)
        val ioB = ioA.flatMap { a =>
          f(a).run(cfg).run(nextS)._1
        }
        (ioB, nextS)
      }
    }

extension [F[_], A](fa: F[A])(using F: Monad[F])
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(fa)(f)
  def map[B](f: A => B): F[B] = F.map(fa)(f)
