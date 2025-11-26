package view.gamephase.traits

import model.Card

trait ICardActionView {
  def showCardDrawnFromDeck(cardDrawn: Card): Unit

  def showCardDrawnFromDiscards(cardDrawn: Card): Unit

  def emptyDrawnCardArea(): Unit

  def showYourNthCard(card: Card): Unit

  def showAdversaryNthCard(adversaryName: String, n: Int, card: Card): Unit

}
