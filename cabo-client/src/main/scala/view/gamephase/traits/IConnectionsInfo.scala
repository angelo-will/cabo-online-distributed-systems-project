package view.gamephase.traits

import model.PlayerPlaying

trait IConnectionsInfo {
  def opponentsDisconnected(player: PlayerPlaying): Unit

  def opponentImpossibleToReach(player: PlayerPlaying): Unit
}
