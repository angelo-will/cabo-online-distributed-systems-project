package view.gamephase.traits

trait IPowerInteractionView {
  def usePowerToSeeOwnCard(): Unit

  def usePowerToSeeAdversaryCard(): Unit

  def usePowerToExchangeCardWithAdversary(): Unit

  def activateAdversariesCards(areActivated: Boolean): Unit

  def activateOwnCards(areActivated: Boolean): Unit

  def notifyYourAdversaryCardSelection(adversaryID: String, index: Int): Unit

  def notifyYourOwnCardSelection(index: Int): Unit

  def changeCardWithAdversaryIsDone(): Unit
}
