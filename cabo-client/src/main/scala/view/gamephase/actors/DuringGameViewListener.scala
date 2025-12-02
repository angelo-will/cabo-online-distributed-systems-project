package view.gamephase.actors

import akka.actor.typed.ActorRef
import messages.{IViewUserCommand, ViewUserCommandMessages as VUCMessages}
import view.lobbyphase.ViewListener.IDuringGameViewListener

case class DuringGameViewListener(whoToResponse: ActorRef[IViewUserCommand]) extends IDuringGameViewListener:
  override def ownCardSelected(cardIndex: Int): Unit =
    whoToResponse ! VUCMessages.OwnCardSelected(cardIndex)

  override def adversaryCardSelected(adversaryID: String, cardIndex: Int): Unit =
    whoToResponse ! VUCMessages.AdversaryCardSelected(adversaryID, cardIndex)

  override def showCardNth(cardIndex: Int): Unit = {}
  //    whoToResponse ! VUCMessages.ShowYourNthCard(cardIndex)

  override def drawFromDeck(): Unit =
    whoToResponse ! VUCMessages.DeckSelected()
  //    whoToResponse ! DrawCardFromDeck()

  override def drawFromDiscard(): Unit =
    whoToResponse ! VUCMessages.DiscardStackSelected()
  //    whoToResponse ! DrawCardFromDiscardStack()

  override def discardCardNth(carIndex: Int): Unit = {}
  //    whoToResponse ! VUCMessages.DiscardYourNthCard(carIndex)

  override def discardCardDrawn(): Unit =
    whoToResponse ! VUCMessages.DiscardCardDrawnSelected()

  override def showAdversaryNthCard(adversaryID: String, cardIndex: Int): Unit = {}
  //    whoToResponse ! VUCMessages.ShowAdversaryNthCard(adversaryID, cardIndex)


  override def swapCardWithAdversaryNthCard(ownCardIndex: Int, adversaryID: String, adversaryCardIndex: Int): Unit = {}
  //    whoToResponse ! VUCMessages.ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndex, adversaryID, adversaryCardIndex)

  override def endTurn(): Unit =
    whoToResponse ! VUCMessages.EndTurnSelected()

  override def callCabo(): Unit =
    whoToResponse ! VUCMessages.CallCaboSelected()

  override def exit(): Unit =
    whoToResponse ! VUCMessages.ExitSelected()

  override def consultingResultsEnded(): Unit =
    whoToResponse ! VUCMessages.ConsultingResultsEnded()