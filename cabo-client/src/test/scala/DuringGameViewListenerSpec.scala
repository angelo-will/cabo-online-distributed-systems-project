import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.wordspec.AnyWordSpecLike
import utils.Message
import view.lobbyphase.ViewListener.IDuringGameViewListener
import view.lobbyphase.actors.DuringGameViewListener
import utils.GameCoordinatorMessage

import scala.concurrent.duration.{FiniteDuration, SECONDS}

class DuringGameViewListenerSpec extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with Matchers:

  import org.scalatest.matchers.must.Matchers.mustBe

  private var gameCoordinatorActor: ActorRef[Message] = _
  private var testProbe: TestProbe[Message] = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    testProbe = createTestProbe[Message]()
  }

  "During Game view listener" must {
    "send message of show a card" when {
      "method to show card is called" in {
        val duringGameActionListener: IDuringGameViewListener = DuringGameViewListener(testProbe.ref)
        val index = 2
        duringGameActionListener.showCardNth(index)
        val cardIndex = testProbe.expectMessageType[GameCoordinatorMessage.ShowYourNthCard](FiniteDuration(3, SECONDS))
        cardIndex.index mustBe index
      }
    }
    "send message to draw from deck" when {
      "method to draw from deck is called" in {
        val duringGameActionListener: IDuringGameViewListener = DuringGameViewListener(testProbe.ref)
        duringGameActionListener.drawFromDeck()
        testProbe.expectMessageType[GameCoordinatorMessage.DrawCardFromDeck](FiniteDuration(3, SECONDS))
      }
    }
    "send message to draw from discard stack" when {
      "method to draw from discard stack is called" in {
        val duringGameActionListener: IDuringGameViewListener = DuringGameViewListener(testProbe.ref)
        duringGameActionListener.drawFromDiscard()
        testProbe.expectMessageType[GameCoordinatorMessage.DrawCardFromDiscardStack](FiniteDuration(3, SECONDS))
      }
    }
    "send message to discard the drawn card" when {
      "method to discard the drawn card is called" in {
        val duringGameActionListener: IDuringGameViewListener = DuringGameViewListener(testProbe.ref)
        duringGameActionListener.discardCardDrawn()
        testProbe.expectMessageType[GameCoordinatorMessage.DiscardCardDrawn](FiniteDuration(3, SECONDS))
      }
    }
    "send message to discard a specific card" when {
      "method to discard a specific card is called" in {
        val duringGameActionListener: IDuringGameViewListener = DuringGameViewListener(testProbe.ref)
        val index = 1
        duringGameActionListener.discardCardNth(index)
        val cardIndex = testProbe.expectMessageType[GameCoordinatorMessage.DiscardYourNthCard](FiniteDuration(3, SECONDS))
        cardIndex.index mustBe index
      }
    }
    "send message to show an adversary's specific card" when {
      "method to show an adversary's specific card is called" in {
        val duringGameActionListener: IDuringGameViewListener = DuringGameViewListener(testProbe.ref)
        val adversaryID = "adversaryID"
        val adversaryNthCard = 1
        duringGameActionListener.showAdversaryNthCard(adversaryID, adversaryNthCard)
        val msg = testProbe.expectMessageType[GameCoordinatorMessage.ShowAdversaryNthCard](FiniteDuration(3, SECONDS))
        msg.playerID mustBe adversaryID
        msg.cardIndex mustBe adversaryNthCard
      }
    }
    "send message to swap a specific card with an adversary's specific card" when {
      "method to swap a specific card with an adversary's specific card is called" in {
        val duringGameActionListener: IDuringGameViewListener = DuringGameViewListener(testProbe.ref)
        val ownCardIndex = 1
        val adversaryID = "adversaryID"
        val adversaryCardIndex = 2
        duringGameActionListener.swapCardWithAdversaryNthCard(ownCardIndex, adversaryID, adversaryCardIndex)
        val msg = testProbe.expectMessageType[GameCoordinatorMessage.ReplaceOwnNthCardWithAdversaryNthOne](FiniteDuration(3, SECONDS))
        msg.ownCardIndex mustBe ownCardIndex
        msg.adversaryID mustBe adversaryID
        msg.adversaryCardIndex mustBe adversaryCardIndex
      }
    }
    "send message to end the turn" when {
      "method to end the turn is called" in {
        val duringGameActionListener: IDuringGameViewListener = DuringGameViewListener(testProbe.ref)
        duringGameActionListener.endTurn()
        testProbe.expectMessageType[GameCoordinatorMessage.EndTurn](FiniteDuration(3, SECONDS))
      }
    }
    "send message to call cabo" when {
      "method to call cabo is called" in {
        val duringGameActionListener: IDuringGameViewListener = DuringGameViewListener(testProbe.ref)
        duringGameActionListener.callCabo()
        testProbe.expectMessageType[GameCoordinatorMessage.CallCabo](FiniteDuration(3, SECONDS))
      }
    }

  }