import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import messages.{IViewUserCommand, ViewUserCommandMessages as VUCMsg}
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.wordspec.AnyWordSpecLike
import view.lobbyphase.ViewListener.IDuringGameViewListener
import view.gamephase.actors.ViewUserCommandListener

import scala.concurrent.duration.{FiniteDuration, SECONDS}

class ViewUserCommandListenerSpec extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with Matchers:

  import org.scalatest.matchers.must.Matchers.mustBe

  private var testProbe: TestProbe[IViewUserCommand] = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    testProbe = createTestProbe[IViewUserCommand]()
  }

  "During Game view listener" must {
    "send message card selected" when {
      "the method is called" in {
        val duringGameActionListener: IDuringGameViewListener = ViewUserCommandListener(testProbe.ref)
        val index = 2
        duringGameActionListener.ownCardSelected(index)
        val cardIndex = testProbe.expectMessageType[VUCMsg.OwnCardSelected](FiniteDuration(3, SECONDS))
        cardIndex.index mustBe index
      }
    }
    "send message deck selected" when {
      "method to draw from deck is called" in {
        val duringGameActionListener: IDuringGameViewListener = ViewUserCommandListener(testProbe.ref)
        duringGameActionListener.drawFromDeck()
        testProbe.expectMessageType[VUCMsg.DeckSelected](FiniteDuration(3, SECONDS))
      }
    }
    "send message discard stack selected" when {
      "method to draw from discard stack is called" in {
        val duringGameActionListener: IDuringGameViewListener = ViewUserCommandListener(testProbe.ref)
        duringGameActionListener.drawFromDiscard()
        testProbe.expectMessageType[VUCMsg.DiscardStackSelected](FiniteDuration(3, SECONDS))
      }
    }
    "send message discard the drawn card selected" when {
      "method to discard the drawn card is called" in {
        val duringGameActionListener: IDuringGameViewListener = ViewUserCommandListener(testProbe.ref)
        duringGameActionListener.discardCardDrawn()
        testProbe.expectMessageType[VUCMsg.DiscardCardDrawnSelected](FiniteDuration(3, SECONDS))
      }
    }

    "send message adversary's card selected" when {
      "method to select adversary's specific card is called" in {
        val duringGameActionListener: IDuringGameViewListener = ViewUserCommandListener(testProbe.ref)
        val adversaryID = "adversaryID"
        val adversaryNthCard = 1
        duringGameActionListener.adversaryCardSelected(adversaryID, adversaryNthCard)
        val msg = testProbe.expectMessageType[VUCMsg.AdversaryCardSelected](FiniteDuration(3, SECONDS))
        msg.adversaryID mustBe adversaryID
        msg.index mustBe adversaryNthCard
      }
    }

    "send message end turn selected" when {
      "method to end the turn is called" in {
        val duringGameActionListener: IDuringGameViewListener = ViewUserCommandListener(testProbe.ref)
        duringGameActionListener.endTurn()
        testProbe.expectMessageType[VUCMsg.EndTurnSelected](FiniteDuration(3, SECONDS))
      }
    }
    "send message call cabo selected" when {
      "method to call cabo is called" in {
        val duringGameActionListener: IDuringGameViewListener = ViewUserCommandListener(testProbe.ref)
        duringGameActionListener.callCabo()
        testProbe.expectMessageType[VUCMsg.CallCaboSelected](FiniteDuration(3, SECONDS))
      }
    }

    "send message exit selected selected" when {
      "method to exit is called" in {
        val duringGameActionListener: IDuringGameViewListener = ViewUserCommandListener(testProbe.ref)
        duringGameActionListener.exit()
        testProbe.expectMessageType[VUCMsg.ExitSelected](FiniteDuration(3, SECONDS))
      }
    }

    "send message consulting results ended" when {
      "method to end consulting results is called" in {
        val duringGameActionListener: IDuringGameViewListener = ViewUserCommandListener(testProbe.ref)
        duringGameActionListener.consultingResultsEnded()
        testProbe.expectMessageType[VUCMsg.ConsultingResultsEnded](FiniteDuration(3, SECONDS))
      }
    }
  }