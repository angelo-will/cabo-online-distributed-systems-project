package messages

import akka.actor.typed.ActorRef
import model.{Card, Game, TurnLog}
import utils.Message

object GameCoordinatorMessage:

  trait GameCoordinatorMessage extends Message

  trait GameCoordinatorPlayerCommand extends GameCoordinatorMessage
  // Messages - commands handled by the coordinator

  trait GameCoordinatorSyncMessage extends GameCoordinatorMessage

  /**
   * Represents a command to draw a card from the deck.
   */
  case class DrawCardFromDeck() extends GameCoordinatorPlayerCommand

  /**
   * Represents a command to draw a card from the discard stack.
   */
  case class DrawCardFromDiscardStack() extends GameCoordinatorPlayerCommand

  case class DiscardCardDrawn() extends GameCoordinatorPlayerCommand

  case class DiscardYourNthCard(index: Int) extends GameCoordinatorPlayerCommand

  case class ShowYourNthCard(index: Int) extends GameCoordinatorPlayerCommand

  case class ShowAdversaryNthCard(playerID: String, cardIndex: Int) extends GameCoordinatorPlayerCommand

  case class ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndex: Int, adversaryID: String, adversaryCardIndex: Int) extends GameCoordinatorPlayerCommand

  case class EndTurn() extends GameCoordinatorPlayerCommand

  case class CallCabo() extends GameCoordinatorPlayerCommand

  // SYNC MESSAGES

  case class StartGame() extends GameCoordinatorSyncMessage

  case class StartPlayCycle() extends GameCoordinatorSyncMessage

  case class NewTurn(game: Game.GameInProgress, turnLog: TurnLog) extends GameCoordinatorSyncMessage

  case class GetEmptyTurn(userID: String) extends GameCoordinatorSyncMessage
  
  case class TurnTimeEnded() extends GameCoordinatorSyncMessage

  // FOR TESTING

  case class SendGameStatus(toWhoSend: ActorRef[Message]) extends GameCoordinatorSyncMessage

  case class GameInformation(game: Game.GameInProgress) extends GameCoordinatorSyncMessage
