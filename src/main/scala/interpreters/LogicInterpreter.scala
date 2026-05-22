package interpreters

import algebras.AtmLogicAlg
import cats.effect.{IO, Ref}
import models.{AtmConfig, AtmState}

class LogicInterpreter(cfg: AtmConfig, stateRef: Ref[IO, AtmState]) extends AtmLogicAlg[IO]:
  def getContext: IO[AtmConfig] =
    IO.pure(cfg)

  def getState: IO[AtmState] =
    stateRef.get

  def updateState(f: AtmState => AtmState): IO[Unit] =
    stateRef.update(f)

  def userExists(user: String): IO[Boolean] =
    stateRef.get.map(_.balances.contains(user))