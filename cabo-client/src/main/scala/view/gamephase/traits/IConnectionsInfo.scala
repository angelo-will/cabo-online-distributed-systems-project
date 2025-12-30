package view.gamephase.traits

import model.PlayerInLobby

trait IConnectionsInfo {
  def opponentsDisconnected(player: PlayerInLobby): Unit

  def opponentImpossibleToReach(player: PlayerInLobby): Unit
  
  def allOpponentsDisconnected(): Unit
}
