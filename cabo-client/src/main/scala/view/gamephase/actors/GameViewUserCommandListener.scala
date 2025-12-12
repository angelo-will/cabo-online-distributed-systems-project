package view.gamephase.actors

import akka.actor.typed.ActorRef
import messages.{IViewUserCommand, ViewUserCommandMessages as VUCMessages}
import view.gamephase.traits.IGameViewUserCommandListener

case class GameViewUserCommandListener(whoToResponse: ActorRef[IViewUserCommand]) extends IGameViewUserCommandListener:
  override def ownCardSelected(cardIndex: Int): Unit =
    whoToResponse ! VUCMessages.OwnCardSelected(cardIndex)

  override def adversaryCardSelected(adversaryID: String, cardIndex: Int): Unit =
    whoToResponse ! VUCMessages.AdversaryCardSelected(adversaryID, cardIndex)

  override def drawFromDeck(): Unit =
    whoToResponse ! VUCMessages.DeckSelected()

  override def drawFromDiscard(): Unit =
    whoToResponse ! VUCMessages.DiscardStackSelected()

  override def discardCardDrawn(): Unit =
    whoToResponse ! VUCMessages.DiscardCardDrawnSelected()

  override def endTurn(): Unit =
    whoToResponse ! VUCMessages.EndTurnSelected()

  override def callCabo(): Unit =
    whoToResponse ! VUCMessages.CallCaboSelected()

  override def exit(): Unit =
    whoToResponse ! VUCMessages.ExitSelected()

  override def consultingResultsEnded(): Unit =
    whoToResponse ! VUCMessages.ConsultingResultsEnded()