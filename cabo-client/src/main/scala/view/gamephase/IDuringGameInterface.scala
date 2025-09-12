package view.gamephase

import model.Game.GameInProgress
import model.{Card, PlayerPlaying, TurnLog}

trait IDuringGameInterface {
  def setLastTurnLog(turnLog: TurnLog): Unit
  def setNewGameInfo(gameInfo: GameInProgress): Unit
  
  def showCardDrawnFromDeck(cardDrawn: Card): Unit
  def showCardDrawnFromDiscards(cardDrawn: Card): Unit
  
  def newDiscardsTopCard(card: Card): Unit
  
  def showYourNthCard(card: Card): Unit
  def showAdversaryNthCard(adversaryName: String, n: Int, card: Card): Unit
  
  def changeCardWithAdversaryIsDone(): Unit
  
  def playerIsDisconnected(player: PlayerPlaying): Unit
  def lostYourConnection(): Unit
}
