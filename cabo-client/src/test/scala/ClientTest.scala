import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import controller.Client
import controller.Client.{GameCancelled, IWantToLeaveTheGame, IWantToPlay, UpdateAboutGame, YouCanNotJoinTheGame, YouJoinedTheGame}
import model.Game.GameInConstruction
import model.{GameParameters, PlayerInLobby}
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.wordspec.AnyWordSpecLike
import utils.ClientMessages.{CreateNewGame, JoinAGame, JoinGame, LeaveTheGame}
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

    "not be able to join a game that is already full" in {
      val defaultName = "defaultCoolName"
      val hostUserID = "Player01"
      val probeClientHost = testKit.createTestProbe[Message]()
      val clientHost = testKit.spawn(Behaviors.monitor(probeClientHost.ref, Client(hostUserID, defaultName)))

      val probeClientJoiner = testKit.createTestProbe[Message]()
      val clientJoiner = testKit.spawn(Behaviors.monitor(probeClientJoiner.ref, Client("Player02", defaultName+"2")))

      val probeClientTooJoiner = testKit.createTestProbe[Message]()
      val clientTooJoiner = testKit.spawn(Behaviors.monitor(probeClientTooJoiner.ref, Client("Player03", defaultName + "3")))

      val gameInConstruction = GameInConstruction(hostUserID + "game", GameParameters(false, 10, 5, 2), List(PlayerInLobby(hostUserID, defaultName, clientHost)))

      clientHost ! CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 2)
      probeClientHost.expectMessage(CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 2))

      clientJoiner ! JoinAGame()
      probeClientJoiner.expectMessage(JoinAGame())

      clientJoiner ! JoinGame(gameInConstruction)
      probeClientJoiner.expectMessage(JoinGame(gameInConstruction))

      probeClientHost.expectMessage(IWantToPlay(PlayerInLobby("Player02", defaultName+"2", clientJoiner), clientJoiner))

      probeClientJoiner.expectMessage(YouJoinedTheGame(gameInConstruction.copy(players = gameInConstruction.players :+ PlayerInLobby("Player02", defaultName+"2", clientJoiner))))

      // Simulate the game being full
      clientTooJoiner ! JoinAGame()
      probeClientTooJoiner.expectMessage(JoinAGame())

      clientTooJoiner ! JoinGame(gameInConstruction)
      probeClientTooJoiner.expectMessage(JoinGame(gameInConstruction))

      probeClientTooJoiner.expectMessage(YouCanNotJoinTheGame())
    }

    "receive a notification when another player joins the game" in {
      val defaultName = "defaultCoolName"
      val hostUserID = "Player01"
      val probeClientHost = testKit.createTestProbe[Message]()
      val clientHost = testKit.spawn(Behaviors.monitor(probeClientHost.ref, Client(hostUserID, defaultName)))

      val probeClientJoiner = testKit.createTestProbe[Message]()
      val clientJoiner = testKit.spawn(Behaviors.monitor(probeClientJoiner.ref, Client("Player02", defaultName + "2")))

      val gameInConstruction = GameInConstruction(hostUserID + "game", GameParameters(false, 10, 5, 4), List(PlayerInLobby(hostUserID, defaultName, clientHost)))

      clientHost ! CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4)
      probeClientHost.expectMessage(CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4))

      clientJoiner ! JoinAGame()
      probeClientJoiner.expectMessage(JoinAGame())

      clientJoiner ! JoinGame(gameInConstruction)
      probeClientJoiner.expectMessage(JoinGame(gameInConstruction))

      probeClientHost.expectMessage(IWantToPlay(PlayerInLobby("Player02", defaultName + "2", clientJoiner), clientJoiner))

      val twoPlayers = gameInConstruction.players :+ PlayerInLobby("Player02", defaultName + "2", clientJoiner)
      probeClientJoiner.expectMessage(YouJoinedTheGame(gameInConstruction.copy(players = twoPlayers)))

      val probeClientTooJoiner = testKit.createTestProbe[Message]()
      val clientTooJoiner = testKit.spawn(Behaviors.monitor(probeClientTooJoiner.ref, Client("Player03", defaultName + "3")))

      clientTooJoiner ! JoinAGame()
      probeClientTooJoiner.expectMessage(JoinAGame())

      clientTooJoiner ! JoinGame(gameInConstruction)
      probeClientTooJoiner.expectMessage(JoinGame(gameInConstruction))

      probeClientHost.expectMessage(IWantToPlay(PlayerInLobby("Player03", defaultName + "3", clientTooJoiner), clientTooJoiner))

      val threePlayers = twoPlayers :+ PlayerInLobby("Player03", defaultName + "3", clientTooJoiner)
      probeClientTooJoiner.expectMessage(YouJoinedTheGame(gameInConstruction.copy(players = threePlayers)))

      probeClientJoiner.expectMessage(UpdateAboutGame(gameInConstruction.copy(players = threePlayers)))
      
      probeClientHost.expectNoMessage()
      probeClientTooJoiner.expectNoMessage()
    }

    "be able to leave a joined game" in {
      val defaultName = "defaultCoolName"
      val hostUserID = "Player01"
      val probeClientHost = testKit.createTestProbe[Message]()
      val clientHost = testKit.spawn(Behaviors.monitor(probeClientHost.ref, Client(hostUserID, defaultName)))

      val probeClientJoiner = testKit.createTestProbe[Message]()
      val clientJoiner = testKit.spawn(Behaviors.monitor(probeClientJoiner.ref, Client("Player02", defaultName + "2")))

      val gameInConstruction = GameInConstruction(hostUserID + "game", GameParameters(false, 10, 5, 4), List(PlayerInLobby(hostUserID, defaultName, clientHost)))

      clientHost ! CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4)
      probeClientHost.expectMessage(CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4))

      clientJoiner ! JoinAGame()
      probeClientJoiner.expectMessage(JoinAGame())

      clientJoiner ! JoinGame(gameInConstruction)
      probeClientJoiner.expectMessage(JoinGame(gameInConstruction))

      probeClientHost.expectMessage(IWantToPlay(PlayerInLobby("Player02", defaultName + "2", clientJoiner), clientJoiner))

      probeClientJoiner.expectMessage(YouJoinedTheGame(gameInConstruction.copy(players = gameInConstruction.players :+ PlayerInLobby("Player02", defaultName + "2", clientJoiner))))

      // Now the player leaves the game
      clientJoiner ! LeaveTheGame()
      probeClientJoiner.expectMessage(LeaveTheGame())

      // The host should receive a notification about the player leaving
      probeClientHost.expectMessage(IWantToLeaveTheGame(PlayerInLobby("Player02", defaultName + "2", clientJoiner)))
    }

    "be notified if someone leave the game" in {
      val defaultName = "defaultCoolName"
      val hostUserID = "Player01"
      val probeClientHost = testKit.createTestProbe[Message]()
      val clientHost = testKit.spawn(Behaviors.monitor(probeClientHost.ref, Client(hostUserID, defaultName)))

      val probeClientJoiner = testKit.createTestProbe[Message]()
      val clientJoiner = testKit.spawn(Behaviors.monitor(probeClientJoiner.ref, Client("Player02", defaultName + "2")))

      val probeClientTooJoiner = testKit.createTestProbe[Message]()
      val clientTooJoiner = testKit.spawn(Behaviors.monitor(probeClientTooJoiner.ref, Client("Player03", defaultName + "3")))

      val gameInConstruction = GameInConstruction(hostUserID + "game", GameParameters(false, 10, 5, 4), List(PlayerInLobby(hostUserID, defaultName, clientHost)))

      clientHost ! CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4)
      probeClientHost.expectMessage(CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4))

      clientJoiner ! JoinAGame()
      probeClientJoiner.expectMessage(JoinAGame())

      clientJoiner ! JoinGame(gameInConstruction)
      probeClientJoiner.expectMessage(JoinGame(gameInConstruction))

      clientTooJoiner ! JoinAGame()
      probeClientTooJoiner.expectMessage(JoinAGame())

      eventually(timeout(3.seconds), interval(100.millis)) {
        probeClientJoiner.receiveMessage() // Player02 expects confirmation of joining from the host, so we know he is the first to join
      }

      clientTooJoiner ! JoinGame(gameInConstruction)
      probeClientTooJoiner.expectMessage(JoinGame(gameInConstruction))

      probeClientHost.receiveMessages(2) // Expecting two messages: one for each player joining

      probeClientJoiner.receiveMessage() // Player02 expects the join message for Player03
      probeClientTooJoiner.receiveMessage() // Expecting the join message for Player03
      
      val gameToExpect = gameInConstruction.copy(players = gameInConstruction.players :+ PlayerInLobby("Player02", defaultName + "2", clientJoiner) :+ PlayerInLobby("Player03", defaultName + "3", clientTooJoiner))

      // Now the player leaves the game
      clientJoiner ! LeaveTheGame()
      probeClientJoiner.expectMessage(LeaveTheGame())

      probeClientTooJoiner.expectMessage(UpdateAboutGame(gameInConstruction.copy(players = gameToExpect.players.filterNot(_.userID == "Player02"))))
    }

    "should receive an abort notification if the host leaves the game" in {
      val defaultName = "defaultCoolName"
      val hostUserID = "Player01"
      val probeClientHost = testKit.createTestProbe[Message]()
      val clientHost = testKit.spawn(Behaviors.monitor(probeClientHost.ref, Client(hostUserID, defaultName)))

      val probeClientJoiner = testKit.createTestProbe[Message]()
      val clientJoiner = testKit.spawn(Behaviors.monitor(probeClientJoiner.ref, Client("Player02", defaultName + "2")))

      val gameInConstruction = GameInConstruction(hostUserID + "game", GameParameters(false, 10, 5, 4), List(PlayerInLobby(hostUserID, defaultName, clientHost)))

      clientHost ! CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4)
      probeClientHost.expectMessage(CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4))

      clientJoiner ! JoinAGame()
      probeClientJoiner.expectMessage(JoinAGame())

      clientJoiner ! JoinGame(gameInConstruction)
      probeClientJoiner.expectMessage(JoinGame(gameInConstruction))

      probeClientHost.expectMessage(IWantToPlay(PlayerInLobby("Player02", defaultName + "2", clientJoiner), clientJoiner))

      probeClientJoiner.expectMessage(YouJoinedTheGame(gameInConstruction.copy(players = gameInConstruction.players :+ PlayerInLobby("Player02", defaultName + "2", clientJoiner))))

      // Now the host leaves the game
      clientHost ! LeaveTheGame()
      probeClientHost.expectMessage(LeaveTheGame())

      // The joiner should receive an abort notification
      probeClientJoiner.expectMessage(GameCancelled())
    }
  }