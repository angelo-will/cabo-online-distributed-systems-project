package view.gamephase.traits

import model.{PlayerInLobby, PlayerPlaying}

trait IConnectionsInfo {
  def opponentsDisconnected(player: PlayerInLobby): Unit

  def opponentImpossibleToReach(player: PlayerInLobby): Unit
}
