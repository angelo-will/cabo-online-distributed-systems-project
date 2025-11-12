package view.gamephase

import akka.actor.typed.ActorRef
import utils.GameCoordinatorMessage.*
import utils.{Message, DuringGameViewMessages}
import view.lobbyphase.ViewListener.IDuringGameViewListener

case class DuringGameViewListener(whoToResponse: ActorRef[Message]) extends IDuringGameViewListener:
  override def ownCardSelected(cardIndex: Int): Unit =
    whoToResponse ! utils.DuringGameViewMessages.OwnCardSelected(cardIndex)

  override def adversaryCardSelected(adversaryID: String, cardIndex: Int): Unit =
    whoToResponse ! DuringGameViewMessages.AdversaryCardSelected(adversaryID, cardIndex)

  override def showCardNth(cardIndex: Int): Unit =
    whoToResponse ! ShowYourNthCard(cardIndex)

  override def drawFromDeck(): Unit =
    whoToResponse ! DuringGameViewMessages.DeckSelected()
  //    whoToResponse ! DrawCardFromDeck()

  override def drawFromDiscard(): Unit =
    whoToResponse ! DuringGameViewMessages.DiscardStackSelected()
  //    whoToResponse ! DrawCardFromDiscardStack()

  override def discardCardNth(carIndex: Int): Unit =
    whoToResponse ! DiscardYourNthCard(carIndex)

  override def discardCardDrawn(): Unit =
    whoToResponse ! DuringGameViewMessages.DiscardCardDrawn()

  override def showAdversaryNthCard(adversaryID: String, cardIndex: Int): Unit =
    whoToResponse ! ShowAdversaryNthCard(adversaryID, cardIndex)

  override def swapCardWithAdversaryNthCard(ownCardIndex: Int, adversaryID: String, adversaryCardIndex: Int): Unit =
    whoToResponse ! ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndex, adversaryID, adversaryCardIndex)

  override def endTurn(): Unit =
    whoToResponse ! DuringGameViewMessages.EndTurn()

  override def callCabo(): Unit =
    whoToResponse ! DuringGameViewMessages.CallCaboSelected ()