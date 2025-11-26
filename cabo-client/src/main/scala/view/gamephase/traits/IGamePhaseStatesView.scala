package view.gamephase.traits

import model.EndGameReason
import model.Game.GameInProgress

trait IGamePhaseStatesView {
  def enterRevealingInitialCardsPhase(): Unit

  def enterWaitingPhase(): Unit

  def startTurn(): Unit

  def afterDrawPhase(canDiscardDrawnCard: Boolean): Unit

  def afterDiscarded(): Unit

  def gameEndedWithData(game: GameInProgress)(ending: EndGameReason): Unit
}