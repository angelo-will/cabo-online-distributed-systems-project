import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import messages.{ClientMessages, Message}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.BeforeAndAfterEach
import view.pregamephase.ViewApplication
import view.pregamephase.actors.PreGameViewListener


class PreGameViewListenerSpec extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with BeforeAndAfterEach
  with Matchers:

  import scala.concurrent.duration.{FiniteDuration, SECONDS}

//////////////////////////////////////////////////////////////
// This test uses user interface, so to pass it there must be
// a user that interacts with the view.
// Commented because github actions will fail otherwise.
///////////////////////////////////////////////////////////////


//  private var probe: TestProbe[Message] = _
//
//  override def beforeEach(): Unit =
//    super.beforeEach()
//    probe = testKit.createTestProbe[Message]()
//    ViewApplication.startView(ViewActorListener(probe.ref), _ => {})
//
//  "View Listener" must {
//    "send create game message" when {
//      "create game button is pressed in view" in {
//        // User must create the game in ui
//        // Then
//        // User must insert data of game and send the request
//        val msg = probe.expectMessageType[ClientMessages.CreateNewGame](FiniteDuration(20, SECONDS))
//        println(s"Received message: $msg")
//      }
//    }
//
//    "send the request to see list of games which are in construction" when {
//      "join game button is pressed in view" in {
//        // User must press the button to see the list of games
//        val msg = probe.expectMessageType[ClientMessages.JoinAGame](FiniteDuration(20, SECONDS))
//        println(s"Received message: $msg")
//      }
//    }
//
//    "sent the request to join a game of with know entire game" when {
//      "join game button in game list view is pressed" in {
//        // User must press the button to see the list of games
//        // Then
//        // User must select the game to join and send request
//        val _ = probe.expectMessageType[ClientMessages.JoinAGame](FiniteDuration(5, SECONDS))
//        val msg = probe.expectMessageType[ClientMessages.JoinGame](FiniteDuration(20, SECONDS))
//        println(s"Received message: $msg")
//      }
//    }
//
//    "send the link of player with which he wants to play" when {
//      "join game with link button is pressed in view" in {
//        // User must press the button to join a game with link
//        // Then
//        // User must enter the link a send the request
//        val msg = probe.expectMessageType[ClientMessages.JoinAddress](FiniteDuration(20, SECONDS))
//        println(s"Received message: $msg")
//      }
//    }
//  }
