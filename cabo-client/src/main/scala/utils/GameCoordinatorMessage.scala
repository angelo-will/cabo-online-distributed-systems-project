package utils

import akka.actor.typed.ActorRef
import model.Card
import model.Game
import utils.ViewMessages.ViewCommand

object GameCoordinatorMessage:

  trait PlayerCommand extends Message

  // Messages - commands handled by the coordinator

  /**
   * Represents a command to draw a card from the deck.
   */
  case class DrawCardFromDeck() extends PlayerCommand

  /**
   * Represents a command to draw a card from the discard stack.
   */
  case class DrawCardFromDiscardStack() extends PlayerCommand

  case class DiscardCardDrawn() extends PlayerCommand

  case class DiscardYourNthCard(index: Int) extends PlayerCommand

  case class NewTurn(game: Game.GameInProgress) extends PlayerCommand

  case class ShowYourNthCard(index: Int) extends PlayerCommand

  case class ShowAdversaryNthCard(playerID: String, cardIndex: Int) extends PlayerCommand

  case class ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndex: Int, adversaryID: String, adversaryCardIndex: Int) extends PlayerCommand

  case class StartGame() extends PlayerCommand

  case class EndTurn() extends PlayerCommand

  case class SendGameStatus(toWhoSend: ActorRef[Message]) extends PlayerCommand

  // Messages - command sent by the coordinator

  /**
   * Represents a message send by the coordinator with the card drawn.
   */
  case class CardDrawn(card: Card) extends PlayerCommand

  case class NewTopCardDiscardStack(card: Card) extends PlayerCommand

  case class GameInformation(game: Game.GameInProgress) extends PlayerCommand

  case class CardSeen(card: Card) extends PlayerCommand
  
  
  case class PlayThisTurn(viewRef: ActorRef[ViewCommand]) extends PlayerCommand
