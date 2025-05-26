import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import controller.Client
import controller.Client.{IWantToPlay, YouJoinedTheGame}
import model.Game.GameInConstruction
import model.{GameParameters, PlayerInLobby}
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.wordspec.AnyWordSpecLike
import utils.Message
import utils.ViewMessages.*

case class TestMessage(address: String) extends Message
case class ReplyTestMessage() extends Message

object TestReceiveMessage:
  def apply(): Behavior[Message] = Behaviors.setup[Message] { ctx =>
    Behaviors.receiveMessagePartial[Message] {
      case TestMessage(address) =>
        ctx.log.info(s"Received message from: $address")
        
        Behaviors.same
    }
  }

class ClientTest extends ScalaTestWithActorTestKit 
  with AnyWordSpecLike
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with Matchers:

  // test sequence of steps that user makes for a turn
  // instructions of test emulate messages from view actor to actor representing player
  // view expects messages from player actor
  // other players' actors expect messages from player actor in the end of the turn to know what happened

  var testProbe: TestProbe[Message] = _

  override def beforeEach(): Unit =
    testProbe = testKit.createTestProbe[Message]()

  override def beforeAll(): Unit = {
    super.beforeAll()
  }

  override def afterAll(): Unit = testKit.shutdownTestKit()

  "This test" must {
    "log information" in {
      val testActor = testKit.spawn(TestReceiveMessage())

      // Send a message to the actor
      testActor ! TestMessage(testProbe.ref.path.toSerializationFormat)
    }
  }

  "A client" should {
    "be able to join a game created by another player" in {
      val defaultName = "defaultCoolName"
      val hostUserID = "Player01"
      val probeClientHost = testKit.createTestProbe[Message]()
      val clientHost = testKit.spawn(Behaviors.monitor(probeClientHost.ref, Client(hostUserID, defaultName)))

      val probeClientJoiner = testKit.createTestProbe[Message]()
      val clientJoiner = testKit.spawn(Behaviors.monitor(probeClientJoiner.ref, Client("Player02", defaultName+"2")))

      val gameInConstruction = GameInConstruction(hostUserID + "game", GameParameters(false, 10, 5, 4), List(PlayerInLobby(hostUserID, defaultName, clientHost)))

      clientHost ! CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4)
      probeClientHost.expectMessage(CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4))

      clientJoiner ! JoinAGame()
      probeClientJoiner.expectMessage(JoinAGame())

      clientJoiner ! JoinGame(gameInConstruction)
      probeClientJoiner.expectMessage(JoinGame(gameInConstruction))

      probeClientHost.expectMessage(IWantToPlay(PlayerInLobby("Player02", defaultName+"2", clientJoiner), clientJoiner))

      probeClientJoiner.expectMessage(YouJoinedTheGame(gameInConstruction.copy(players = gameInConstruction.players :+ PlayerInLobby("Player02", defaultName+"2", clientJoiner))))
    }
  }