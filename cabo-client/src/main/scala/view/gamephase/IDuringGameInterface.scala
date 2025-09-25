package view.gamephase

import model.Game.GameInProgress
import model.{Card, PlayerPlaying, TurnLog}

trait IDuringGameInterface {
  def updateLastTurnLog(playerName: String, round: Int, turnLog: TurnLog): Unit

  def updateGameInfo(gameInfo: GameInProgress): Unit

  def showCardDrawnFromDeck(cardDrawn: Card): Unit

  def showCardDrawnFromDiscards(cardDrawn: Card): Unit

  def updateDiscardsTopCard(card: Card): Unit

  def emptyDiscardStack(): Unit

  def showYourNthCard(card: Card): Unit

  def showAdversaryNthCard(adversaryName: String, n: Int, card: Card): Unit

  def changeCardWithAdversaryIsDone(): Unit

  def playerIsDisconnected(player: PlayerPlaying): Unit

  def lostYourConnection(): Unit

  /**
   * Notify to interface to enter in waiting phase. Games button are disbled during this phase except the one for exit.
   */
  def enterWaitingPhase(): Unit

  def enterRevealingInitialCardsPhase(): Unit

  def startTurn(): Unit

  def afterDrawPhase(): Unit
}
