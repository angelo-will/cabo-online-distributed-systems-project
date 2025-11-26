package view.gamephase.traits

import model.Game.GameInProgress
import model.{Card, TurnLog}

trait IGameInfoView {
  def updateGameInfo(gameInfo: GameInProgress): Unit

  def updatePlayerWhoIsPlaying(playerID: String): Unit

  def updateLastTurnLog(turnLog: TurnLog): Unit

  def updateRevealingLog(revealingLog: TurnLog): Unit

  def updateDiscardsTopCard(card: Card): Unit

  def emptyDiscardStack(): Unit
}
