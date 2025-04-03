package model

abstract class Power(val name: String)

object Power:
  case class NoPower() extends Power("This card has no power")
  case class SeeYourCard() extends Power("See one of your cards")
  case class SeeYourOpponentCard() extends Power("See your opponent card")
  case class ChangeOneOfYourCardWithOpponent() extends Power("Change one of your card with opponent")
  
