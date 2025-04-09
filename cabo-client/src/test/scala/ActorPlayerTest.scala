import akka.actor.testkit.typed.scaladsl.{ActorTestKit, ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import controller.GameLogicActor
import model.Suit.{Clubs, Spades}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*
import model.{Card, GameInConstruction, GameParameters}
import utils.ClientMessages.{CardDrawn, DrawCardFromDeck}
import utils.{ClientMessages, Message, ServerMessages}

class ActorPlayerTest extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with Matchers:

  // test sequence of steps that user makes for a turn
  // instructions of test emulate messages from view actor to actor representing player
  // view expects messages from player actor
  // other players' actors expect messages from player actor in the end of the turn to know what happened

  var gameLogicActor: ActorRef[Message] = _
  var gameCoordinatorProbe: TestProbe[Message] = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    gameCoordinatorProbe = createTestProbe[Message]()
    gameLogicActor = testKit.spawn(GameLogicActor(gameCoordinatorProbe.ref))
  }

  "Actor Player" must {
    // Draw a card when receive draw command and send what he draws
    "send information about the card drawn" when {
      "receive the command to draw a card" in {
        gameLogicActor ! DrawCardFromDeck()
//        gameCoordinatorProbe.expectMessageType[ClientMessages.CardDrawn]
        gameCoordinatorProbe.expectMessage(CardDrawn(Card("5", Spades())))
      }
    }
    
    // Discard when receive the command to discard and send it
    // Ideally in future could be a different card so start to think about how notify what is discarded
    "send ack about card discarded" when {
      "receive the command to discard a card" in {

      }
    }
    
    // Send to other players new status of the game when receive the command to end turn
    "send new status of the game" when {
      "receive the command to end turn" in {

      }
    }
    
    // during the game there are more ways to see the cards
    "send card value" when {
      // at start or power 
      "receive the command to see one of own card" in {

      }
      // with power
      "receive the command to see one card of opponents" in {

      }
    }
  }
