package model

import utils.Message

object TurnPhase:
  sealed trait TurnPhase extends Message

  case class AwaitingFirstShow() extends TurnPhase

  case class AwaitingSecondShowShow() extends TurnPhase

  case class AwaitDrawCard() extends TurnPhase

  case class AwaitUsePower() extends TurnPhase

  case class AwaitDiscardCard() extends TurnPhase

  case class EndedTurn() extends TurnPhase
