package algebras
import models.{AtmConfig, AtmState}

trait AtmLogicAlg[F[_]]:
  def getContext: F[AtmConfig]
  def getState: F[AtmState]
  def updateState(f: AtmState => AtmState): F[Unit]
  def userExists(user: String): F[Boolean]