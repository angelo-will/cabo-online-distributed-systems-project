package model

import model.TurnEvent.*
import model.TurnPhase.*
import model.PhaseEvents.*
import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}
import messages.{CborSerializable, Message}

object TurnEvent:
  @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
  @JsonSubTypes(
    Array(
      new JsonSubTypes.Type(value = classOf[TurnEvent.DrawCardFromDeck], name = "drawCardFromDeck"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.DrawCardFromDiscardStack], name = "drawCardFromDiscardStack"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.SeeSelfCard], name = "seeSelfCard"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.SeeAdversaryCard], name = "seeAdversaryCard"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.ReplaceOwnCardWithAdversaryCard], name = "replaceOwnCard"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.CardDrawnDiscarded], name = "cardDrawnDiscarded"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.OwnCardDiscarded], name = "ownCardDiscarded"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.CaboCalled], name = "caboCalled"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.EndTurn], name = "endTurn"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.JumpTurnForTimerEnded], name = "jumpTurnForTimerEnded"),
      new JsonSubTypes.Type(value = classOf[TurnEvent.JumpTurnForDisconnection], name = "jumpTurnForDisconnection"),
    )
  )
  sealed trait TurnEvent extends Message

  case class DrawCardFromDeck(card: Card) extends TurnEvent

  case class DrawCardFromDiscardStack(card: Card) extends TurnEvent

  case class SeeSelfCard(index: Int) extends TurnEvent

  case class SeeAdversaryCard(playerID: String, index: Int) extends TurnEvent

  case class ReplaceOwnCardWithAdversaryCard(ownCardIndex: Int, adversaryID: String, adversaryCardIndex: Int) extends TurnEvent

  case class CardDrawnDiscarded(card: Card) extends TurnEvent

  case class OwnCardDiscarded(card: Card, index: Int) extends TurnEvent

  case class CaboCalled() extends TurnEvent

  case class EndTurn() extends TurnEvent

  case class JumpTurnForTimerEnded() extends TurnEvent

  case class JumpTurnForDisconnection() extends TurnEvent

object PhaseEvents:
  case class PhaseEvents(phase: TurnPhase, events: List[TurnEvent]) extends Message

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(value = classOf[PlayCycleTurnLog], name = "duringGameTurnLog"),
    new JsonSubTypes.Type(value = classOf[RevealingSectionTurnLog], name = "initialPhaseTurnLog")))
trait TurnLog:
  def playerID: String

  def playerName: String
  
  def player: User

  def addEvent(event: TurnEvent): Unit

  def events: List[TurnEvent]

  def currentPhase: TurnPhase

  def round: Int

class InvalidTurnEventException(event: TurnEvent.TurnEvent)
  extends IllegalArgumentException(s"Invalid event '$event' in turn phase.")

class PlayCycleTurnLog(val user: User, val round: Int) extends TurnLog with Message:

  private var phaseEvents: PhaseEvents = new PhaseEvents(AwaitDrawCard(), List())

  override def playerID: String = user.userID

  override def playerName: String = user.name

  override def player: User = user

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
   * - [[AwaitDiscardCard]]: Allows [[CardDrawnDiscarded]] or [[OwnCardDiscarded]] which transitions to [[AwaitEndTurn]].
   *
   * - [[AwaitEndTurn]]: Allows [[CallCabo]] or [[EndTurn]] which transitions to [[EndedTurn]].
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
    case (AwaitDiscardCard(), CardDrawnDiscarded(card)) =>
      this.passToNewPhaseWithEvent(AwaitEndTurn(), event)
    case (AwaitDiscardCard(), OwnCardDiscarded(card, index)) =>
      this.passToNewPhaseWithEvent(AwaitEndTurn(), event)
    case (AwaitEndTurn(), CaboCalled()) =>
      this.passToNewPhaseWithEvent(EndedTurn(), event)
    case (AwaitEndTurn(), EndTurn()) =>
      this.passToNewPhaseWithEvent(EndedTurn(), event)
    case (_, JumpTurnForTimerEnded()) =>
      this.passToNewPhaseWithEvent(EndedTurn(), event)
    case (_, JumpTurnForDisconnection()) =>
      this.passToNewPhaseWithEvent(EndedTurn(), event)
    case _ =>
      throw new InvalidTurnEventException(event)

  override def currentPhase: TurnPhase = this.phaseEvents.phase

  private def passToNewPhaseWithEvent(newPhase: TurnPhase, event: TurnEvent): Unit =
    phaseEvents = new PhaseEvents(newPhase, phaseEvents.events :+ event)

  override def toString: String = {
    s"DuringGameTurnLog\n\tuserID=$user,\n\tactual phase = ${phaseEvents.phase} \n\tevents = $events"
  }

class RevealingSectionTurnLog(val user: User) extends TurnLog with Message:

  private var phaseEvents: PhaseEvents = new PhaseEvents(AwaitingFirstShow(), List())

  override def playerName: String = user.name

  override def playerID: String = user.userID

  override def player: User = user

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


