package interpreters

import algebras.AtmLogicAlg
import models.{AtmConfig, AtmState, Reader, State, IO}

class LogicInterpreter extends AtmLogicAlg[AtmStack]:
  def getContext: AtmStack[AtmConfig] =
    Reader(cfg => State(st => (IO.pure(cfg), st)))

  def getState: AtmStack[AtmState] =
    Reader(_ => State(st => (IO.pure(st), st)))

  def updateState(f: AtmState => AtmState): AtmStack[Unit] =
    Reader(_ => State(st => (IO.pure(()), f(st))))

  def userExists(user: String): AtmStack[Boolean] =
    Reader(_ => State(st => (IO.pure(st.balances.contains(user)), st)))