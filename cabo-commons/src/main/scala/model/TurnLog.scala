package model

import utils.Message
import model.TurnEvent.*
import model.TurnPhase.*
import model.PhaseEvents.*
import akka.serialization.jackson.CborSerializable
import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}

object TurnEvent:
  @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
  @JsonSubTypes(
    Array(
      new JsonSubTypes.Type(value = classOf[TurnEvent.DrawCardFromDeck], name = "drawCardFromDeck"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.DrawCardFromDiscardStack], name = "drawCardFromDiscardStack"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.SeeSelfCard], name = "seeSelfCard"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.SeeAdversaryCard], name = "seeAdversaryCard"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.ReplaceOwnCardWithAdversaryCard], name = "replaceOwnCard"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.CardDiscarded], name = "cardDiscarded")
    )
  )
  sealed trait TurnEvent extends Message

  case class DrawCardFromDeck(card: Card) extends TurnEvent

  case class DrawCardFromDiscardStack(card: Card) extends TurnEvent

  case class SeeSelfCard(index: Int) extends TurnEvent

  case class SeeAdversaryCard(playerID: String, index: Int) extends TurnEvent

  case class ReplaceOwnCardWithAdversaryCard(ownCardIndex: Int, adversaryID: String, adversaryCardIndex: Int) extends TurnEvent

  case class CardDiscarded(card: Card) extends TurnEvent

object PhaseEvents:
  case class PhaseEvents(phase: TurnPhase, events: List[TurnEvent]) extends Message

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(value = classOf[DuringGameTurnLog], name = "duringGameTurnLog"),
    new JsonSubTypes.Type(value = classOf[InitialPhaseTurnLog], name = "initialPhaseTurnLog")))
trait TurnLog:
  def playerName: String

  def addEvent(event: TurnEvent): Unit

  def events: List[TurnEvent]

  def currentPhase: TurnPhase

  def round:Int

class InvalidTurnEventException(event: TurnEvent.TurnEvent)
  extends IllegalArgumentException(s"Invalid event '$event' in turn phase.")

class DuringGameTurnLog(val ofUserID: String, val round: Int) extends TurnLog with Message:

  private var phaseEvents: PhaseEvents = new PhaseEvents(AwaitDrawCard(), List())

  override def playerName: String = ofUserID

  override def events: List[TurnEvent] = phaseEvents.copy().events

  /**
   * Adds a new TurnEvent to the log, enforcing a specific sequence of events based on the current TurnPhase.
   * The allowed events depend on the current phase of the turn. If an invalid event is provided
   * for the current phase, an [[InvalidTurnEventException]] is thrown.
   *
   * The allowed transitions are as follows:
   *
   * - [[AwaitDrawCard]]: Allows [[DrawCardFromDeck]] or [[DrawCardFromDiscardStack]].
   *
   * - If [[DrawCardFromDeck]] results in a card with no power, transitions to [[AwaitDiscardCard]].
   *
   * - If [[DrawCardFromDeck]] results in a card with power, transitions to [[AwaitUsePower]].
   *
   * - [[DrawCardFromDiscardStack]] always transitions to [[AwaitDiscardCard]].
   *
   * - [[AwaitUsePower]]: Allows [[SeeSelfCard]], [[SeeAdversaryCard]], or [[ReplaceOwnCardWithAdversaryCard]],
   * all of which transition to [[AwaitDiscardCard]].
   *
   * - [[AwaitDiscardCard]]: Allows [[CardDiscarded]], which transitions to [[EndedTurn]].
   *
   * - [[EndedTurn]]: Does not allow any further events to be added.
   *
   * @param event The TurnEvent to add to the log.
   * @throws InvalidTurnEventException if the provided event is not valid for the current TurnPhase.
   */
  override def addEvent(event: TurnEvent): Unit = (phaseEvents.phase, event) match
    case (AwaitDrawCard(), DrawCardFromDeck(card)) =>
      if card.power == Power.NoPower() then
        this.passToNewPhaseWithEvent(AwaitDiscardCard(), event)
      else
        this.passToNewPhaseWithEvent(AwaitUsePower(), event)
    case (AwaitDrawCard(), DrawCardFromDiscardStack(card)) =>
      this.passToNewPhaseWithEvent(AwaitDiscardCard(), event)
    case (AwaitUsePower(), SeeSelfCard(index)) =>
      this.passToNewPhaseWithEvent(AwaitDiscardCard(), event)
    case (AwaitUsePower(), SeeAdversaryCard(adversaryID, index)) =>
      this.passToNewPhaseWithEvent(AwaitDiscardCard(), event)
    case (AwaitUsePower(), ReplaceOwnCardWithAdversaryCard(ownCardIndex, adversaryID, adversaryCardIndex)) =>
      this.passToNewPhaseWithEvent(AwaitDiscardCard(), event)
    case (AwaitDiscardCard(), CardDiscarded(card)) =>
      this.passToNewPhaseWithEvent(EndedTurn(), event)
    case _ =>
      throw new InvalidTurnEventException(event)

  override def currentPhase: TurnPhase = this.phaseEvents.phase

  private def passToNewPhaseWithEvent(newPhase: TurnPhase, event: TurnEvent): Unit =
    phaseEvents = new PhaseEvents(newPhase, phaseEvents.events :+ event)

  override def toString: String = {
    s"DuringGameTurnLog\n\tuserID=$ofUserID,\n\tactual phase = ${phaseEvents.phase} \n\tevents = $events"
  }

class InitialPhaseTurnLog(val userID: String) extends TurnLog with Message:

  private var phaseEvents: PhaseEvents = new PhaseEvents(AwaitingFirstShow(), List())

  override def playerName: String = userID

  override def events: List[TurnEvent] = phaseEvents.events

  override def addEvent(event: TurnEvent): Unit = (phaseEvents.phase, event) match
    case (AwaitingFirstShow(), SeeSelfCard(index)) =>
      phaseEvents = new PhaseEvents(AwaitingSecondShowShow(), phaseEvents.events :+ event)
    case (AwaitingSecondShowShow(), SeeSelfCard(index)) =>
      phaseEvents = new PhaseEvents(EndedTurn(), phaseEvents.events :+ event)
    case _ =>
      throw new InvalidTurnEventException(event)

  override def currentPhase: TurnPhase = phaseEvents.phase

  override def round: Int = 0


