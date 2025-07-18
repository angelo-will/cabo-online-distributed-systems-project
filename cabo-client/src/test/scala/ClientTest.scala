import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.{ActorRef, Behavior}
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
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, color}
import org.scalatest.wordspec.AnyWordSpecLike
import utils.ClientMessages.{ChangePlayerName, CreateNewGame, GetPlayerInfo, JoinAGame, JoinAddress, JoinGame, LeaveTheGame, PlayerInfo}
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

  val hostId = "HostPlayer"
  val hostName = "HostName"

  val joinerId = "JoinerPlayer"
  val joinerName = "JoinerName"
  
  val joinerTooId = "JoinerTooPlayer"
  val joinerTooName = "JoinerTooName"

  // test sequence of steps that user makes for a turn
  // instructions of test emulate messages from view actor to actor representing player
  // view expects messages from player actor
  // other players' actors expect messages from player actor in the end of the turn to know what happened

  override def beforeAll(): Unit = {
    val cluster = Cluster.get(testKit.system)
    cluster.manager.tell(Join.create(cluster.selfMember.address))
  }

  override def afterAll(): Unit = testKit.shutdownTestKit()
  
  def correctPlayerID(name: String, ref: ActorRef[Message]): String = {
    name + ref.path.address.hashCode()
  }

  def retrieveClientID(client: ActorRef[Message], clientProbe: TestProbe[Message]): (String, String) = {

    val probe = testKit.createTestProbe[Message]()

    client ! GetPlayerInfo(probe.ref)
    clientProbe.expectMessage(GetPlayerInfo(probe.ref))

    probe.receiveMessage() match {
      case PlayerInfo(id, name) => (id, name)
      case _ => fail("Expected PlayerInfo message")
    }
  }

  def createClientAndProbe(id: String = "ClientID", name: String = "ClientName"): (ActorRef[Message], TestProbe[Message]) = {
    val probe = testKit.createTestProbe[Message]()
    val client = testKit.spawn(Behaviors.monitor(probe.ref, Client(id, name)))
    (client, probe)
  }

  def hostCreateGame(clientHost: ActorRef[Message], probeClientHost: TestProbe[Message],
                     makePublic: Boolean = false, maxTimeRound: Int = 10, maxNumRound: Int = 5, maxPlayers: Int = 4): Unit = {

    val (hostPlayerID, _) = retrieveClientID(clientHost, probeClientHost)

    clientHost ! CreateNewGame(makePublic, maxTimeRound, maxNumRound, maxPlayers)
    probeClientHost.expectMessage(CreateNewGame(makePublic, maxTimeRound, maxNumRound, maxPlayers))

    val listProbe = TestProbe[Receptionist.Listing]()
    eventually(timeout(3.seconds), interval(100.millis)) {
      system.receptionist ! Receptionist.Find(ServiceKey[Message](hostPlayerID + "game"), listProbe.ref)
      val listing = listProbe.receiveMessage()
      assert(listing.serviceInstances(ServiceKey[Message](hostPlayerID + "game")).contains(clientHost))
    }
  }

  def joinHostGame(clientHost: ActorRef[Message], probeClientHost: TestProbe[Message],
                   clientJoiner: ActorRef[Message], probeClientJoiner: TestProbe[Message]): Unit = {

    val (hostPlayerID, _) = retrieveClientID(clientHost, probeClientHost)

    val (joinerPlayerID, joinerName) = retrieveClientID(clientJoiner, probeClientJoiner)

    clientJoiner ! JoinAGame()
    probeClientJoiner.expectMessage(JoinAGame())

    clientJoiner ! JoinAddress(hostPlayerID + "game")
    probeClientJoiner.expectMessage(JoinAddress(hostPlayerID + "game"))

    probeClientHost.expectMessage(IWantToPlay(PlayerInLobby(joinerPlayerID, joinerName, clientJoiner), clientJoiner))

    probeClientJoiner.receiveMessage() match {
      case YouJoinedTheGame(game) =>
        assert(game.players.exists(p => p.userID == hostPlayerID))
      case _ => fail("Expected YouJoinedTheGame message")
    }

  }

  "A client" should {
    "be able to join a game created by another player" in {

      val (clientHost, probeClientHost) = createClientAndProbe(hostId, hostName)

      val (clientJoiner, probeClientJoiner) = createClientAndProbe(joinerId, joinerName)

      hostCreateGame(clientHost, probeClientHost)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner)

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()
    }

    "not be able to join a game that is already full" in {

      val (clientHost, probeClientHost) = createClientAndProbe(hostId, hostName)

      val (clientJoiner, probeClientJoiner) = createClientAndProbe(joinerId, joinerName)
      
      val (clientTooJoiner, probeClientTooJoiner) = createClientAndProbe(joinerTooId, joinerTooName)

      hostCreateGame(clientHost, probeClientHost, maxPlayers = 2)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner)

      // Simulate the game being full
      clientTooJoiner ! JoinAGame()
      probeClientTooJoiner.expectMessage(JoinAGame())

      clientTooJoiner ! JoinAddress(correctPlayerID(hostId, clientHost) + "game")
      probeClientTooJoiner.expectMessage(JoinAddress(correctPlayerID(hostId, clientHost) + "game"))

      probeClientTooJoiner.receiveMessage() match {
        case YouCanNotJoinTheGame(game) =>
          assert(game.players.size == 2)
          assert(game.players.exists(p => p.userID == correctPlayerID(hostId, clientHost)))
          assert(game.players.exists(p => p.userID == correctPlayerID(joinerId, clientJoiner)))
        case _ => fail("Expected YouCanNotJoinTheGame message")
      }

      clientHost ! LeaveTheGame()
    }

    "receive a notification when another player joins the game" in {

      val (clientHost, probeClientHost) = createClientAndProbe(hostId, hostName)

      val (clientJoiner, probeClientJoiner) = createClientAndProbe(joinerId, joinerName)

      val (clientTooJoiner, probeClientTooJoiner) = createClientAndProbe(joinerTooId, joinerTooName)

      hostCreateGame(clientHost, probeClientHost)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner)

      joinHostGame(clientHost, probeClientHost, clientTooJoiner, probeClientTooJoiner)

      probeClientJoiner.receiveMessage() match {
        case UpdateAboutGame(game) =>
          assert(game.players.size == 3)
          assert(game.players.exists(p => p.userID == correctPlayerID(hostId, clientHost)))
          assert(game.players.exists(p => p.userID == correctPlayerID(joinerId, clientJoiner)))
          assert(game.players.exists(p => p.userID == correctPlayerID(joinerTooId, clientTooJoiner)))
        case _ => fail("Expected UpdateAboutGame message")
      }

      probeClientHost.expectNoMessage()
      probeClientTooJoiner.expectNoMessage()

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()
    }

    "be able to leave a joined game" in {

      val (clientHost, probeClientHost) = createClientAndProbe(hostId, hostName)

      val (clientJoiner, probeClientJoiner) = createClientAndProbe(joinerId, joinerName)

      val (clientTooJoiner, probeClientTooJoiner) = createClientAndProbe(joinerTooId, joinerTooName)

      hostCreateGame(clientHost, probeClientHost)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner)

      // Now the player leaves the game
      clientJoiner ! LeaveTheGame()
      probeClientJoiner.expectMessage(LeaveTheGame())

      // The host should receive a notification about the player leaving
      probeClientHost.expectMessage(IWantToLeaveTheGame(PlayerInLobby(correctPlayerID(joinerId, clientJoiner), joinerName, clientJoiner)))

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()
    }

    "be notified if someone leave the game" in {

      val (clientHost, probeClientHost) = createClientAndProbe(hostId, hostName)

      val (clientJoiner, probeClientJoiner) = createClientAndProbe(joinerId, joinerName)

      val (clientTooJoiner, probeClientTooJoiner) = createClientAndProbe(joinerTooId, joinerTooName)

      hostCreateGame(clientHost, probeClientHost)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner)

      joinHostGame(clientHost, probeClientHost, clientTooJoiner, probeClientTooJoiner)

      // Player02 expects the join message for Player03
      probeClientJoiner.receiveMessage() match {
        case UpdateAboutGame(game) =>
          assert(game.players.size == 3)
          assert(game.players.exists(p => p.userID == correctPlayerID(hostId, clientHost)))
          assert(game.players.exists(p => p.userID == correctPlayerID(joinerId, clientJoiner)))
          assert(game.players.exists(p => p.userID == correctPlayerID(joinerTooId, clientTooJoiner)))
      }

      // Now the player leaves the game
      clientJoiner ! LeaveTheGame()
      probeClientJoiner.expectMessage(LeaveTheGame())

      probeClientTooJoiner.receiveMessage() match {
        case UpdateAboutGame(game) =>
          assert(game.players.size == 2)
          assert(game.players.exists(p => p.userID == correctPlayerID(hostId, clientHost)))
          assert(game.players.exists(p => p.userID == correctPlayerID(joinerTooId, clientTooJoiner)))
      }

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()
    }

    "should receive an abort notification if the host leaves the game" in {
      val (clientHost, probeClientHost) = createClientAndProbe(hostId, hostName)

      val (clientJoiner, probeClientJoiner) = createClientAndProbe(joinerId, joinerName)

      val (clientTooJoiner, probeClientTooJoiner) = createClientAndProbe(joinerTooId, joinerTooName)

      hostCreateGame(clientHost, probeClientHost)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner)

      // Now the host leaves the game
      clientHost ! LeaveTheGame()
      probeClientHost.expectMessage(LeaveTheGame())

      // The joiner should receive an abort notification
      probeClientJoiner.expectMessage(GameCancelled())

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()
    }

    "should be able to enter a game using an 'address' (code)" in {
      val probeClientHost = testKit.createTestProbe[Message]()
      val clientHost = testKit.spawn(Behaviors.monitor(probeClientHost.ref, Client(hostId, hostName)))

      val probeClientJoiner = testKit.createTestProbe[Message]()
      val clientJoiner = testKit.spawn(Behaviors.monitor(probeClientJoiner.ref, Client(joinerId, "defaultCoolName2")))

      clientHost ! CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4)
      probeClientHost.expectMessage(CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4))

      val probe = TestProbe[Receptionist.Listing]()
      eventually(timeout(3.seconds), interval(100.millis)) {
        system.receptionist ! Receptionist.Find(ServiceKey[Message](correctPlayerID(hostId, clientHost)+"game"), probe.ref)
        val listing = probe.receiveMessage()
        assert(listing.serviceInstances(ServiceKey[Message](correctPlayerID(hostId, clientHost)+"game")).contains(clientHost))
      }

      clientJoiner ! JoinAGame()
      probeClientJoiner.expectMessage(JoinAGame())

      clientJoiner ! JoinAddress(correctPlayerID(hostId, clientHost)+"game")
      probeClientJoiner.expectMessage(JoinAddress(correctPlayerID(hostId, clientHost)+"game"))

      probeClientHost.expectMessage(IWantToPlay(PlayerInLobby(correctPlayerID(joinerId, clientJoiner), "defaultCoolName2", clientJoiner), clientJoiner))

      probeClientJoiner.expectMessage(YouJoinedTheGame(GameInConstruction(correctPlayerID(hostId, clientHost)+"game", GameParameters(false, 10, 5, 4), List(PlayerInLobby(correctPlayerID(hostId, clientHost), hostName, clientHost), PlayerInLobby(correctPlayerID(joinerId, clientJoiner), "defaultCoolName2", clientJoiner)))))

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()

    }

    "should be able to change the name of the player" in {
//      val defaultName = "defaultCoolName"
//      val hostUserID = "Player01g"
      val probeClientHost = testKit.createTestProbe[Message]()
      val clientHost = testKit.spawn(Behaviors.monitor(probeClientHost.ref, Client(hostId, hostName)))

      val probe = testKit.createTestProbe[Message]()

      clientHost ! GetPlayerInfo(probe.ref)
      probeClientHost.expectMessage(GetPlayerInfo(probe.ref))

      probe.expectMessage(PlayerInfo(hostId+clientHost.path.address.hashCode(), hostName))

      val newCoolName = "NewCoolName"
      clientHost ! ChangePlayerName(newCoolName, probe.ref)
      probeClientHost.expectMessage(ChangePlayerName(newCoolName, probe.ref))

      probe.expectMessage(PlayerInfo(hostId + clientHost.path.address.hashCode(), newCoolName))

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()
    }

//    "should receive a notification if a player 'crash'" in {
//      val defaultName = "defaultCoolName"
//      val hostUserID = "Player01"
//      val probeClientHost = testKit.createTestProbe[Message]()
//      val clientHost = testKit.spawn(Behaviors.monitor(probeClientHost.ref, Client(hostUserID, defaultName)))
//
//      val probeClientJoiner = testKit.createTestProbe[Message]()
//      val clientJoiner = testKit.spawn(Behaviors.monitor(probeClientJoiner.ref, Client(joinerId, defaultName + "2")))
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
//      probeClientHost.expectMessage(IWantToPlay(PlayerInLobby(joinerId, defaultName + "2", clientJoiner), clientJoiner))
//
//      probeClientJoiner.expectMessage(YouJoinedTheGame(gameInConstruction.copy(players = gameInConstruction.players :+ PlayerInLobby(joinerId, defaultName + "2", clientJoiner))))
//
//      testKit.stop(clientJoiner) // Simulate a crash by stopping the client actor
//
//      probeClientHost.expectMessage(10.seconds,PlayerUnreachable(PlayerInLobby(joinerId, defaultName + "2", clientJoiner)))
//    }
  }