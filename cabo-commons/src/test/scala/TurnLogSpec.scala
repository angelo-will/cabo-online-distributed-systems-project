package model

import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TurnLogSpec extends AnyWordSpec
  with Matchers
  with BeforeAndAfterEach:

  import model.TurnEvent.*
  import model.Rank.*
  import model.Suit.*

  var turnLog: TurnLog = _
  val genericCard = new Card(Ten(), Spades())
  val cardWithPowerSeeYourCard = new Card(Jack(), Spades())
  val cardWithPowerSeeYourAdversaryCard = new Card(Queen(), Spades())
  val cardWithPowerReplaceYourCardWithAdversaryOne = new Card(King(), Spades())

  override def beforeEach(): Unit =
    super.beforeEach()
    turnLog = new TurnLog("Player01")

  "A TurnLog" when {
    "initially created" must {
      "be no events" in {
        turnLog.events mustBe empty
      }
    }

    "before draw" must {
      "allow draw a card from deck and contain only that event" in {
        turnLog.addEvent(DrawCardFromDeck(genericCard))
        turnLog.events must contain only DrawCardFromDeck(genericCard)
      }

      "allow draw a card from discard stack and contain only that event" in {
        turnLog.addEvent(DrawCardFromDiscardStack(genericCard))
        turnLog.events must contain only DrawCardFromDiscardStack(genericCard)
      }

      "not allow other events" in {
        checkThrowExceptionBeforeDrawPhase(turnLog)
      }
    }

    "after draw a power" must {
      "allow SeeSelfCard" in {
        checkTurnLogCorrectSequence(turnLog, List(
          DrawCardFromDeck(cardWithPowerSeeYourCard),
          SeeSelfCard(0)))
      }

      "allow SeeAdversaryCard" in {
        checkTurnLogCorrectSequence(turnLog, List(
          DrawCardFromDeck(cardWithPowerSeeYourAdversaryCard),
          SeeAdversaryCard("player2", 0)))
      }

      "allow ReplaceOwnCardWithAdversaryCard" in {
        checkTurnLogCorrectSequence(turnLog, List(
          DrawCardFromDeck(cardWithPowerReplaceYourCardWithAdversaryOne),
          ReplaceOwnCardWithAdversaryCard(0, "player2", 0)))
      }

      "not allow other events" in {
        turnLog.addEvent(DrawCardFromDeck(cardWithPowerSeeYourCard))
        an[InvalidTurnEventException] must be thrownBy turnLog.addEvent(DrawCardFromDeck(genericCard))
        an[InvalidTurnEventException] must be thrownBy turnLog.addEvent(DrawCardFromDiscardStack(genericCard))
        an[InvalidTurnEventException] must be thrownBy turnLog.addEvent(CardDiscarded(genericCard))
        turnLog.events must contain only DrawCardFromDeck(cardWithPowerSeeYourCard)
      }
    }

    "draw a card with no power from deck" must {
      "allow discard the card or one of own cards" in {
        checkTurnLogCorrectSequence(turnLog, List(
          DrawCardFromDeck(genericCard),
          CardDiscarded(genericCard)))
      }

      "not allow other events" in {
        turnLog.addEvent(DrawCardFromDeck(genericCard))
        checkThrowExceptionAfterDrawNoPowerPhase(turnLog)
        turnLog.events must contain only DrawCardFromDeck(genericCard)
      }
    }

    "draw a card from discard stack" must {
      "allow discard one of own cards" in {
        checkTurnLogCorrectSequence(turnLog, List(
          DrawCardFromDiscardStack(genericCard),
          CardDiscarded(genericCard)))
      }
      "not allow other events" in {
        turnLog.addEvent(DrawCardFromDiscardStack(genericCard))
        checkThrowExceptionAfterDrawNoPowerPhase(turnLog)
        turnLog.events must contain only DrawCardFromDiscardStack(genericCard)
      }
    }

    "after use power to see own card" must {
      "allow discard the card or one of own cards" in {
        checkTurnLogCorrectSequence(turnLog, List(
          DrawCardFromDeck(cardWithPowerSeeYourCard),
          SeeSelfCard(0),
          CardDiscarded(cardWithPowerSeeYourCard)
        ))
      }
    }

    "after use power to see adversary card" must {
      "allow discard the card or one of own cards" in {
        checkTurnLogCorrectSequence(turnLog, List(
          DrawCardFromDeck(cardWithPowerSeeYourCard),
          SeeAdversaryCard("adversary", 0),
          CardDiscarded(cardWithPowerSeeYourCard)
        ))
      }
    }

    "after use power to replace one of own card with one of adversary" must {
      "allow discard the card or one of own cards" in {
        checkTurnLogCorrectSequence(turnLog, List(
          DrawCardFromDeck(cardWithPowerSeeYourCard),
          ReplaceOwnCardWithAdversaryCard(0, "adversary", 0),
          CardDiscarded(cardWithPowerSeeYourCard)
        ))
      }
    }

    "after discar card" must {
      "not allow any events" in {
        turnLog.addEvent(DrawCardFromDeck(genericCard))
        turnLog.addEvent(CardDiscarded(genericCard))
        checkThrowErrorAfterCardDiscarded(turnLog)
        turnLog.events must contain inOrder(DrawCardFromDeck(genericCard), CardDiscarded(genericCard))
      }
    }
  }

  private def checkThrowExceptionBeforeDrawPhase(turnLog: TurnLog): Unit =
    an[InvalidTurnEventException] must be thrownBy turnLog.addEvent(SeeSelfCard(0))
    an[InvalidTurnEventException] must be thrownBy turnLog.addEvent(SeeAdversaryCard("player2", 0))
    an[InvalidTurnEventException] must be thrownBy turnLog.addEvent(ReplaceOwnCardWithAdversaryCard(0, "player2", 0))
    an[InvalidTurnEventException] must be thrownBy turnLog.addEvent(CardDiscarded(genericCard))

  private def checkThrowExceptionAfterDrawNoPowerPhase(turnLog: TurnLog): Unit =
    an[InvalidTurnEventException] must be thrownBy turnLog.addEvent(DrawCardFromDeck(cardWithPowerSeeYourCard))
    an[InvalidTurnEventException] must be thrownBy turnLog.addEvent(DrawCardFromDiscardStack(genericCard))
    an[InvalidTurnEventException] must be thrownBy turnLog.addEvent(SeeSelfCard(0))
    an[InvalidTurnEventException] must be thrownBy turnLog.addEvent(SeeAdversaryCard("player2", 0))
    an[InvalidTurnEventException] must be thrownBy turnLog.addEvent(ReplaceOwnCardWithAdversaryCard(0, "player2", 0))

  private def checkThrowErrorAfterUsePower(turnLog: TurnLog): Unit =
    checkThrowExceptionAfterDrawNoPowerPhase(turnLog)

  private def checkThrowErrorAfterCardDiscarded(turnLog: TurnLog): Unit =
    checkThrowExceptionAfterDrawNoPowerPhase(turnLog)
    an[InvalidTurnEventException] must be thrownBy turnLog.addEvent(CardDiscarded(genericCard))


  private def checkTurnLogCorrectSequence(turnLog: TurnLog, events: List[TurnEvent]): Unit =
    events.foreach(turnLog.addEvent(_))
    turnLog.events must contain theSameElementsInOrderAs events



