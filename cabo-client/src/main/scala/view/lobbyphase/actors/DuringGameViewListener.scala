package view.lobbyphase.actors

import akka.actor.typed.ActorRef
import utils.Message
import view.lobbyphase.ViewListener.IDuringGameViewListener
import utils.GameCoordinatorMessage.*

case class DuringGameViewListener(whoToResponse: ActorRef[Message]) extends IDuringGameViewListener:
  override def showCardNth(cardIndex: Int): Unit =
    whoToResponse ! ShowYourNthCard(cardIndex)

  override def drawFromDeck(): Unit =
    whoToResponse ! DrawCardFromDeck()

  override def drawFromDiscard(): Unit =
    whoToResponse ! DrawCardFromDiscardStack()

  override def discardCardNth(carIndex: Int): Unit =
    whoToResponse ! DiscardYourNthCard(carIndex)

  override def discardCardDrawn(): Unit =
    whoToResponse ! DiscardCardDrawn()

  override def showAdversaryNthCard(adversaryID: String, cardIndex: Int): Unit =
    whoToResponse ! ShowAdversaryNthCard(adversaryID, cardIndex)

  override def swapCardWithAdversaryNthCard(ownCardIndex: Int, adversaryID: String, adversaryCardIndex: Int): Unit =
    whoToResponse ! ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndex, adversaryID, adversaryCardIndex)

  override def endTurn(): Unit =
    whoToResponse ! EndTurn()

  override def callCabo(): Unit = 
    whoToResponse ! CallCabo()