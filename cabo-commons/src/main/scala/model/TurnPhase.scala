package model

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}
import messages.CborSerializable

object TurnPhase:
  @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
  @JsonSubTypes(
    Array(
      new JsonSubTypes.Type(value = classOf[TurnPhase.AwaitingFirstShow], name = "awaitingFirstShow"),
      new JsonSubTypes.Type(value = classOf[TurnPhase.AwaitingSecondShowShow], name = "awaitingSecondShow"),
      new JsonSubTypes.Type(value = classOf[TurnPhase.AwaitDrawCard], name = "awaitDrawCard"),
      new JsonSubTypes.Type(value = classOf[TurnPhase.AwaitUsePower], name = "awaitUsePower"),
      new JsonSubTypes.Type(value = classOf[TurnPhase.AwaitDiscardCard], name = "awaitDiscardCard"),
      new JsonSubTypes.Type(value = classOf[TurnPhase.AwaitEndTurn], name = "awaitEndTurn"),
      new JsonSubTypes.Type(value = classOf[TurnPhase.EndedTurn], name = "endedTurn")
    )
  )
  sealed trait TurnPhase extends CborSerializable

  case class AwaitingFirstShow() extends TurnPhase

  case class AwaitingSecondShowShow() extends TurnPhase

  case class AwaitDrawCard() extends TurnPhase

  case class AwaitUsePower() extends TurnPhase

  case class AwaitDiscardCard() extends TurnPhase
  
  case class AwaitEndTurn() extends TurnPhase

  case class EndedTurn() extends TurnPhase
