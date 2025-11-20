package view.gamephase

import model.Game.GameInProgress
import model.{Card, PlayerPlaying, TurnLog}

trait IDuringGameInterface {
  def updateRevealingLog(revealingLog: TurnLog): Unit

  def updateLastTurnLog(turnLog: TurnLog): Unit

  def updateGameInfo(gameInfo: GameInProgress): Unit

  def updatePlayerWhoIsPlaying(playerID: String): Unit

  def showCardDrawnFromDeck(cardDrawn: Card): Unit

  def showCardDrawnFromDiscards(cardDrawn: Card): Unit

  def updateDiscardsTopCard(card: Card): Unit

  def emptyDiscardStack(): Unit

  def emptyDrawnCardArea(): Unit

  def showYourNthCard(card: Card): Unit

  def showAdversaryNthCard(adversaryName: String, n: Int, card: Card): Unit

  def changeCardWithAdversaryIsDone(): Unit

  def playerIsDisconnected(player: PlayerPlaying): Unit

  def lostYourConnection(): Unit

  def gameEndedByCabo(game: GameInProgress): Unit

  def gameEndedByTurns(game: GameInProgress): Unit

  def gameEndedByEmptyDeck(game: GameInProgress): Unit

  /**
   * Notify to interface to enter in waiting phase. Games button are disbled during this phase except the one for exit.
   */
  def enterWaitingPhase(): Unit

  def enterRevealingInitialCardsPhase(): Unit

  def startTurn(): Unit

  def afterDrawPhase(canDiscardDrawnCard: Boolean): Unit

  def usePowerToSeeOwnCard(): Unit

  def usePowerToSeeAdversaryCard(): Unit

  def usePowerToExchangeCardWithAdversary(): Unit

  def activateAdversariesCards(areActivated: Boolean): Unit

  def activateOwnCards(areActivated: Boolean): Unit

  def notifyYourAdversaryCardSelection(adversaryID: String, index: Int): Unit

  def notifyYourOwnCardSelection(index: Int): Unit

  def afterDiscarded(): Unit
}
