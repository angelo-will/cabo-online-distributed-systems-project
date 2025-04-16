package utils

import model.Card

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
  
  case class EndTurn() extends PlayerCommand

  // Messages - command sent by the coordinator
  
  /**
   * Represents a message send by the coordinator with the card drawn.
   */
  case class CardDrawn(card: Card) extends PlayerCommand
  
  case class NewTopCardDiscardStack(card: Card) extends PlayerCommand
