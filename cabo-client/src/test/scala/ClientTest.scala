import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.Behavior
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.typed.{Cluster, Join}
import com.typesafe.config.ConfigFactory
import controller.Client
import controller.Client.{GameCancelled, IWantToLeaveTheGame, IWantToPlay, PlayerUnreachable, UpdateAboutGame, YouCanNotJoinTheGame, YouJoinedTheGame}
import model.Game.GameInConstruction
import model.{GameParameters, PlayerInLobby}
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.wordspec.AnyWordSpecLike
import utils.ClientMessages.{CreateNewGame, JoinAGame, JoinAddress, JoinGame, LeaveTheGame}
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

class ClientTest extends ScalaTestWithActorTestKit(ConfigFactory.parseString("""
    akka.actor.provider = "cluster"
    akka.remote.artery.canonical.port = 2579
  """))
  with AnyWordSpecLike
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with Matchers:

  // test sequence of steps that user makes for a turn
  // instructions of test emulate messages from view actor to actor representing player
  // view expects messages from player actor
  // other players' actors expect messages from player actor in the end of the turn to know what happened

  override def beforeAll(): Unit = {
    val cluster = Cluster.get(testKit.system)
    cluster.manager.tell(Join.create(cluster.selfMember.address))
  }

  override def afterAll(): Unit = testKit.shutdownTestKit()

  "A client" should {
    "be able to join a game created by another player" in {
      val defaultName = "defaultCoolName"
      val hostUserID = "Player01a"
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
      val hostUserID = "Player01b"
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

      val twoPlayersGame = gameInConstruction.copy(players = gameInConstruction.players :+ PlayerInLobby("Player02", defaultName + "2", clientJoiner))
      probeClientJoiner.expectMessage(YouJoinedTheGame(twoPlayersGame))

      // Simulate the game being full
      clientTooJoiner ! JoinAGame()
      probeClientTooJoiner.expectMessage(JoinAGame())

      // Attempt to join the full game, even if it is the old game reference
      clientTooJoiner ! JoinGame(gameInConstruction)
      probeClientTooJoiner.expectMessage(JoinGame(gameInConstruction))

      probeClientTooJoiner.expectMessage(YouCanNotJoinTheGame(twoPlayersGame))
    }

    "receive a notification when another player joins the game" in {
      val defaultName = "defaultCoolName"
      val hostUserID = "Player01c"
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
      val hostUserID = "Player01d"
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
      val hostUserID = "Player01e"
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
      val hostUserID = "Player01f"
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

    "should be able to enter a game using an 'address' (code)" in {
      val probeClientHost = testKit.createTestProbe[Message]()
      val clientHost = testKit.spawn(Behaviors.monitor(probeClientHost.ref, Client("Player01", "defaultCoolName")))

      val probeClientJoiner = testKit.createTestProbe[Message]()
      val clientJoiner = testKit.spawn(Behaviors.monitor(probeClientJoiner.ref, Client("Player02", "defaultCoolName2")))

      clientHost ! CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4)
      probeClientHost.expectMessage(CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4))

      val probe = TestProbe[Receptionist.Listing]()
      eventually(timeout(3.seconds), interval(100.millis)) {
        system.receptionist ! Receptionist.Find(ServiceKey[Message]("Player01game"), probe.ref)
        val listing = probe.receiveMessage()
        assert(listing.serviceInstances(ServiceKey[Message]("Player01game")).contains(clientHost))
      }

      clientJoiner ! JoinAGame()
      probeClientJoiner.expectMessage(JoinAGame())

      clientJoiner ! JoinAddress("Player01game")
      probeClientJoiner.expectMessage(JoinAddress("Player01game"))

      probeClientHost.expectMessage(IWantToPlay(PlayerInLobby("Player02", "defaultCoolName2", clientJoiner), clientJoiner))

      probeClientJoiner.expectMessage(YouJoinedTheGame(GameInConstruction("Player01game", GameParameters(false, 10, 5, 4), List(PlayerInLobby("Player01", "defaultCoolName", clientHost), PlayerInLobby("Player02", "defaultCoolName2", clientJoiner)))))
    }

//    "should receive a notification if a player 'crash'" in {
//      val defaultName = "defaultCoolName"
//      val hostUserID = "Player01"
//      val probeClientHost = testKit.createTestProbe[Message]()
//      val clientHost = testKit.spawn(Behaviors.monitor(probeClientHost.ref, Client(hostUserID, defaultName)))
//
//      val probeClientJoiner = testKit.createTestProbe[Message]()
//      val clientJoiner = testKit.spawn(Behaviors.monitor(probeClientJoiner.ref, Client("Player02", defaultName + "2")))
//
//      val gameInConstruction = GameInConstruction(hostUserID + "game", GameParameters(false, 10, 5, 4), List(PlayerInLobby(hostUserID, defaultName, clientHost)))
//
//      clientHost ! CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4)
//      probeClientHost.expectMessage(CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4))
//
//      clientJoiner ! JoinAGame()
//      probeClientJoiner.expectMessage(JoinAGame())
//
//      clientJoiner ! JoinGame(gameInConstruction)
//      probeClientJoiner.expectMessage(JoinGame(gameInConstruction))
//
//      probeClientHost.expectMessage(IWantToPlay(PlayerInLobby("Player02", defaultName + "2", clientJoiner), clientJoiner))
//
//      probeClientJoiner.expectMessage(YouJoinedTheGame(gameInConstruction.copy(players = gameInConstruction.players :+ PlayerInLobby("Player02", defaultName + "2", clientJoiner))))
//
//      testKit.stop(clientJoiner) // Simulate a crash by stopping the client actor
//
//      probeClientHost.expectMessage(10.seconds,PlayerUnreachable(PlayerInLobby("Player02", defaultName + "2", clientJoiner)))
//    }
  }