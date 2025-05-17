package model

import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InitialPhaseTurnLogSpec extends AnyWordSpec
  with Matchers
  with BeforeAndAfterEach:

  import model.TurnEvent.*
  import model.Rank.*
  import model.Suit.*

  var initialPhaseLog: InitialPhaseTurnLog = _
  val genericCard01 = new Card(Ten(), Spades())
  val genericCard02 = new Card(Queen(), Hearts())

  override def beforeEach(): Unit =
    super.beforeEach()
    initialPhaseLog = new InitialPhaseTurnLog("player01")

  "An InitialPhaseTurnLog" must {
    "not have events registered" when {
      "just created" in {
        this.initialPhaseLog.events mustBe empty
      }
    }
    "allow see one card" when {
      "just crated" in {
        this.initialPhaseLog.addEvent(SeeSelfCard(0))
        this.initialPhaseLog.events must contain only SeeSelfCard(0)
      }
    }
    "allow see a second card" when {
      "after see the first" in {
        this.initialPhaseLog.addEvent(SeeSelfCard(0))
        this.initialPhaseLog.addEvent(SeeSelfCard(2))
        this.initialPhaseLog.events must contain inOrder(
          SeeSelfCard(0),
          SeeSelfCard(2)
        )
      }
    }
    "not allow other events" when {
      "after see two cards" in {
        this.initialPhaseLog.addEvent(SeeSelfCard(0))
        this.initialPhaseLog.addEvent(SeeSelfCard(2))
        an[InvalidTurnEventException] must be thrownBy this.initialPhaseLog.addEvent(SeeSelfCard(0))
      }
      "try add events different from SeeSelfCard" in {
        an[InvalidTurnEventException] must be thrownBy this.initialPhaseLog.addEvent(DrawCardFromDeck(genericCard01))
        an[InvalidTurnEventException] must be thrownBy this.initialPhaseLog.addEvent(DrawCardFromDiscardStack(genericCard02))
        an[InvalidTurnEventException] must be thrownBy this.initialPhaseLog.addEvent(SeeAdversaryCard("player2", 0))
        an[InvalidTurnEventException] must be thrownBy this.initialPhaseLog.addEvent(ReplaceOwnCardWithAdversaryCard(0, "player2", 0))
        an[InvalidTurnEventException] must be thrownBy this.initialPhaseLog.addEvent(CardDiscarded(genericCard01))
      }
    }
  }  