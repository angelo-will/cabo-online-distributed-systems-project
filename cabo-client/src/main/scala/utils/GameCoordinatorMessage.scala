package utils

import model.Card

object GameCoordinatorMessage:

  trait PlayerCommand extends Message

  case class DrawCardFromDeck() extends PlayerCommand

  case class DrawCardFromDiscardStack() extends PlayerCommand

  case class CardDrawn(card: Card) extends PlayerCommand

