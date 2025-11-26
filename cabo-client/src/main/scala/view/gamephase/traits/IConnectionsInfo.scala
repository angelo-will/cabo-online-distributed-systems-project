package view.gamephase.traits

import model.PlayerPlaying

trait IConnectionsInfo {
  def playerIsDisconnected(player: PlayerPlaying): Unit

  def lostYourConnection(): Unit
}
