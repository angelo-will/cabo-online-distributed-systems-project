package view.gamephase.traits

trait IGameViewUserCommandListener {

  def ownCardSelected(cardIndex: Int): Unit

  def adversaryCardSelected(adversaryID: String, cardIndex: Int): Unit

  def drawFromDeck(): Unit

  def drawFromDiscard(): Unit

  def discardCardDrawn(): Unit

  def endTurn(): Unit

  def callCabo(): Unit

  def exit(): Unit

  def consultingResultsEnded(): Unit
}
