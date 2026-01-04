package model

import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RevealingSectionTurnLogSpec extends AnyWordSpec
  with Matchers
  with BeforeAndAfterEach:

  import model.TurnEvent.*
  import model.Rank.*
  import model.Suit.*

  var revealingSectionTurnLog: RevealingSectionTurnLog = _
  val genericCard01 = new Card(Ten(), Spades())
  val genericCard02 = new Card(Queen(), Hearts())

  override def beforeEach(): Unit =
    super.beforeEach()
    revealingSectionTurnLog = new RevealingSectionTurnLog("player01")

  "A RevealingSectionTurnLog" must {
    "not have events registered" when {
      "just created" in {
        this.revealingSectionTurnLog.events mustBe empty
      }
    }
    "allow see one card" when {
      "just crated" in {
        this.revealingSectionTurnLog.addEvent(SeeSelfCard(0))
        this.revealingSectionTurnLog.events must contain only SeeSelfCard(0)
      }
    }
    "allow see a second card" when {
      "after see the first" in {
        this.revealingSectionTurnLog.addEvent(SeeSelfCard(0))
        this.revealingSectionTurnLog.addEvent(SeeSelfCard(2))
        this.revealingSectionTurnLog.events must contain inOrder(
          SeeSelfCard(0),
          SeeSelfCard(2)
        )
      }
    }
    "not allow other events" when {
      "after see two cards" in {
        this.revealingSectionTurnLog.addEvent(SeeSelfCard(0))
        this.revealingSectionTurnLog.addEvent(SeeSelfCard(2))
        an[InvalidTurnEventException] must be thrownBy this.revealingSectionTurnLog.addEvent(SeeSelfCard(0))
      }
      "try add events different from SeeSelfCard" in {
        an[InvalidTurnEventException] must be thrownBy this.revealingSectionTurnLog.addEvent(DrawCardFromDeck(genericCard01))
        an[InvalidTurnEventException] must be thrownBy this.revealingSectionTurnLog.addEvent(DrawCardFromDiscardStack(genericCard02))
        an[InvalidTurnEventException] must be thrownBy this.revealingSectionTurnLog.addEvent(SeeAdversaryCard("player2", 0))
        an[InvalidTurnEventException] must be thrownBy this.revealingSectionTurnLog.addEvent(ReplaceOwnCardWithAdversaryCard(0, "player2", 0))
        an[InvalidTurnEventException] must be thrownBy this.revealingSectionTurnLog.addEvent(OwnCardDiscarded(genericCard01,0))
      }
    }
  }  