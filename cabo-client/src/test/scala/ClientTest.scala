import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.typed.{Cluster, Join}
import com.typesafe.config.ConfigFactory
import controller.Client
import controller.Client.*
import model.Game.GameInProgress
import model.GameStatus.InProgress
import model.{GameParameters, GameStatus, PlayerInLobby, PlayerPlaying}
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import utils.ClientMessages.*
import utils.Message
import utils.InitialViewMessages.*

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
  
  override def beforeAll(): Unit = {
    val cluster = Cluster.get(testKit.system)
    cluster.manager.tell(Join.create(cluster.selfMember.address))
  }

  override def afterAll(): Unit = testKit.shutdownTestKit()

  def correctPlayerID(name: String, ref: ActorRef[Message]): String = {
    name + ref.path.address.hashCode()
  }

  def retrieveClientIdAndName(client: ActorRef[Message], clientProbe: TestProbe[Message]): (String, String) = {

    val probe = testKit.createTestProbe[Message]()

    client ! GetPlayerInfo(probe.ref)
    clientProbe.expectMessage(GetPlayerInfo(probe.ref))

    probe.receiveMessage() match {
      case PlayerInfo(id, name) => (id, name)
      case _ => fail("Expected PlayerInfo message")
    }
  }

  def createClientAndProbeWithView(id: String = "ClientID", name: String = "ClientName"): (ActorRef[Message], TestProbe[Message], TestProbe[Message]) = {
    val probe = testKit.createTestProbe[Message]()
    val viewProbe = testKit.createTestProbe[Message]()
    val client = testKit.spawn(Behaviors.monitor(probe.ref, Client(id, name, viewProbe.ref)), id)
    (client, probe, viewProbe)
  }

  def createClientAndProbe(id: String = "ClientID", name: String = "ClientName"): (ActorRef[Message], TestProbe[Message]) = {
    val probe = testKit.createTestProbe[Message]()
    val client = testKit.spawn(Behaviors.monitor(probe.ref, Client(id, name)), id)
    (client, probe)
  }

  def hostCreateGame(clientHost: ActorRef[Message], probeClientHost: TestProbe[Message], clientHostView: TestProbe[Message] = null,
                     makePublic: Boolean = false, maxTimeRound: Int = 10, maxNumRound: Int = 5, maxPlayers: Int = 4): Unit = {

    val (hostPlayerID, _) = retrieveClientIdAndName(clientHost, probeClientHost)

    clientHost ! CreateNewGame(makePublic, maxTimeRound, maxNumRound, maxPlayers)
    probeClientHost.expectMessage(CreateNewGame(makePublic, maxTimeRound, maxNumRound, maxPlayers))

    clientHostView match {
      case null => // do nothing
      case vp =>
        vp.receiveMessage() match {
          case GameCreated(game) =>
            assert(game.players.exists(p => p.userID == hostPlayerID))
          case _ => fail("Expected GameCreated message")
        }
    }

    val listProbe = TestProbe[Receptionist.Listing]()
    eventually(timeout(3.seconds), interval(100.millis)) {
      system.receptionist ! Receptionist.Find(ServiceKey[Message](hostPlayerID + "game"), listProbe.ref)
      val listing = listProbe.receiveMessage()
      assert(listing.serviceInstances(ServiceKey[Message](hostPlayerID + "game")).contains(clientHost))
    }
  }

  def joinHostGame(clientHost: ActorRef[Message], probeClientHost: TestProbe[Message],
                   clientJoiner: ActorRef[Message], probeClientJoiner: TestProbe[Message],
                   clientHostView: TestProbe[Message] = null, clientJoinerView: TestProbe[Message] = null): Unit = {

    val (hostPlayerID, _) = retrieveClientIdAndName(clientHost, probeClientHost)

    val (joinerPlayerID, joinerName) = retrieveClientIdAndName(clientJoiner, probeClientJoiner)

    clientJoiner ! JoinAGame()
    probeClientJoiner.expectMessage(JoinAGame())

    clientJoiner ! JoinAddress(hostPlayerID + "game")
    probeClientJoiner.expectMessage(JoinAddress(hostPlayerID + "game"))

    probeClientHost.expectMessage(IWantToPlay(PlayerInLobby(joinerPlayerID, joinerName, clientJoiner), clientJoiner))

    clientHostView match {
      case null => // do nothing
      case vp =>
        vp.receiveMessage() match {
          case GameInfoUpdate(game) =>
            assert(game.players.exists(p => p.userID == joinerPlayerID))
            assert(game.players.exists(p => p.userID == hostPlayerID))
          case _ => fail("Expected GameInfoUpdate message")
        }
    }

    probeClientJoiner.receiveMessage() match {
      case YouJoinedTheGame(game) =>
        assert(game.players.exists(p => p.userID == hostPlayerID))
      case _ => fail("Expected YouJoinedTheGame message")
    }

    clientJoinerView match {
      case null => // do nothing
      case vp =>
        vp.receiveMessage() match {
          case FailedToPublishToServer() => // trying to find games
          case _ => fail("Joiner View expected ReadyToPlay message")
        }
    }

    clientJoinerView match {
      case null => // do nothing
      case vp =>
        vp.receiveMessage() match {
          case GameJoined(game) =>
            assert(game.players.exists(p => p.userID == joinerPlayerID))
            assert(game.players.exists(p => p.userID == hostPlayerID))
          case _ => fail("Expected GameJoined message")
        }
    }

  }

  def createStubGameInProgress(gameParameters: GameParameters = GameParameters(), gameStatus: GameStatus = InProgress(), round: Int = 0, players: List[PlayerPlaying]): GameInProgress = {
    GameInProgress(
      code = "XXXX",
      gameParameters = gameParameters,
      gameStatus = gameStatus,
      players = players,
      deckStack = null,
      discardDeckStack = null,
      currentRound = round
    )
  }

  def startGame(clientHost: ActorRef[Message], probeClientHost: TestProbe[Message], clientHostView: TestProbe[Message],
                joiners: List[(ActorRef[Message], TestProbe[Message], TestProbe[Message])],
                coordinatorProbe: TestProbe[Message], game: GameInProgress): Unit = {

    val coordinatorStub: () => Behavior[Message] = () => Behaviors.receiveMessage {
      m =>
        coordinatorProbe.ref ! m
        Behaviors.same
    }

    clientHost ! StartGameBehavior(coordinatorStub, clientHost)
    probeClientHost.expectMessage(StartGameBehavior(coordinatorStub, clientHost))

    clientHost ! TakeGetInProgressGame(game)
    probeClientHost.expectMessage(TakeGetInProgressGame(game))

    joiners.foreach { case (clientJoiner, probeClientJoiner, clientJoinerView) =>

      val msg = probeClientJoiner.expectMessageType[GameHasStarted]
      assert(
        msg.gameInProgress.players.exists(p => p.userID == correctPlayerID(hostId, clientHost)),
        s"GameHasStarted does not contains the correct player: $msg"
      )

      clientJoinerView match {
        case null => // do nothing
        case vp =>

          val msg = vp.expectMessageType[ReadyToPlay]
      }
    }

    probeClientHost.receiveMessages(joiners.size).foreach {
      case SynchronizationAck(id) => // ok
      case _ => fail("Host probe expected SynchronizationAck message")
    }

    clientHostView match {
      case null => // do nothing
      case vp =>
        vp.receiveMessage() match {
          case ReadyToPlay(gameCoordinator) => // ok
          case _ => fail("Host View expected ReadyToPlay message")
        }
    }

  }

//  def hostStartGame(clientHost: ActorRef[Message], probeClientHost: TestProbe[Message],
//                    clientJoiner: ActorRef[Message], probeClientJoiner: TestProbe[Message],
//                    clientHostView: TestProbe[Message] = null, clientJoinerView: TestProbe[Message] = null,
//                    coordinatorProbe: TestProbe[Message]): Unit = {
//
//    val coordinatorStub: Behavior[Message] = Behaviors.receiveMessage {
//      m =>
//        coordinatorProbe.ref ! m
//        Behaviors.same
//    }
//
//    val hostTrueId = correctPlayerID(hostId, clientHost)
//    val joinerTrueId = correctPlayerID(joinerId, clientJoiner)
//
//    val gameInProgress = createStubGameInProgress(round = 0, List(PlayerPlaying(hostTrueId, hostName, 0, null), PlayerPlaying(joinerTrueId, joinerName, 1, null)))
//
//    clientHost ! StartGameBehavior(coordinatorStub, clientHost)
//    probeClientHost.expectMessage(StartGameBehavior(coordinatorStub, clientHost))
//
//    clientHost ! TakeGetInProgressGame(gameInProgress)
//    probeClientHost.expectMessage(TakeGetInProgressGame(gameInProgress))
//
//    probeClientJoiner.receiveMessage() match {
//      case GameHasStarted(host, game) =>
//        assert(game.players.exists(p => p.userID == hostTrueId))
//        assert(game.players.exists(p => p.userID == joinerTrueId))
//      case _ => fail("Expected GameHasStarted message")
//    }
//
//    clientJoinerView match {
//      case null => // do nothing
//      case vp =>
//        vp.receiveMessage() match {
//          case ReadyToPlay(gameCoordinator) => // ok
//          case _ => fail("Joiner View expected ReadyToPlay message")
//        }
//    }
//
//    probeClientHost.receiveMessage() match {
//      case SynchronizationAck(id) if id.contains(joinerId) => // ok
//      case _ => fail("Host probe expected SynchronizationAck message")
//    }
//
//    clientHostView match {
//      case null => // do nothing
//      case vp =>
//        vp.receiveMessage() match {
//          case ReadyToPlay(gameCoordinator) => // ok
//          case _ => fail("Host View expected ReadyToPlay message")
//        }
//    }
//
//  }

  "A client" should {
    "be able to join a game created by another player" in {

      val (clientHost, probeClientHost) = createClientAndProbe(hostId, hostName)

      val (clientJoiner, probeClientJoiner) = createClientAndProbe(joinerId, joinerName)

      hostCreateGame(clientHost, probeClientHost)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner)

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()

      testKit.stop(clientHost)
      testKit.stop(clientJoiner)
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

      testKit.stop(clientHost)
      testKit.stop(clientJoiner)
      testKit.stop(clientTooJoiner)
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

      testKit.stop(clientHost)
      testKit.stop(clientJoiner)
      testKit.stop(clientTooJoiner)
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

      testKit.stop(clientHost)
      testKit.stop(clientJoiner)
      testKit.stop(clientTooJoiner)
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

      testKit.stop(clientHost)
      testKit.stop(clientJoiner)
      testKit.stop(clientTooJoiner)
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

      testKit.stop(clientHost)
      testKit.stop(clientJoiner)
      testKit.stop(clientTooJoiner)
    }

    "should be able to enter a game using an 'address' (code)" in {

      val (clientHost, probeClientHost) = createClientAndProbe(hostId, hostName)

      val (clientJoiner, probeClientJoiner) = createClientAndProbe(joinerId, joinerName)

      hostCreateGame(clientHost, probeClientHost)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner)

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()

      testKit.stop(clientHost)
      testKit.stop(clientJoiner)
    }

    "should be able to change the name of the player" in {

      val (clientHost, probeClientHost) = createClientAndProbe(hostId, hostName)

      val probe = testKit.createTestProbe[Message]()

      clientHost ! GetPlayerInfo(probe.ref)
      probeClientHost.expectMessage(GetPlayerInfo(probe.ref))

      probe.expectMessage(PlayerInfo(hostId+clientHost.path.address.hashCode(), hostName))

      val newCoolName = "NewCoolName"
      clientHost ! ChangePlayerName(newCoolName, probe.ref)
      probeClientHost.expectMessage(ChangePlayerName(newCoolName, probe.ref))

      probe.expectMessage(PlayerInfo(hostId + clientHost.path.address.hashCode(), newCoolName))

      // should be able to retrieve player information after creating a game
      hostCreateGame(clientHost, probeClientHost)

      clientHost ! GetPlayerInfo(probe.ref)
      probeClientHost.expectMessage(GetPlayerInfo(probe.ref))

      probe.expectMessage(PlayerInfo(hostId + clientHost.path.address.hashCode(), newCoolName))

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()
      probeClientHost.expectMessage(LeaveTheGame())

      clientHost ! JoinAGame()
      probeClientHost.expectMessage(JoinAGame())

      clientHost ! GetPlayerInfo(probe.ref)
      probeClientHost.expectMessage(GetPlayerInfo(probe.ref))

      probe.expectMessage(PlayerInfo(hostId + clientHost.path.address.hashCode(), newCoolName))

      testKit.stop(clientHost)
    }

    "should be able to start a game with a false coordinator" in {

      val (clientHost, probeClientHost, clientHostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, clientJoinerView) = createClientAndProbeWithView(joinerId, joinerName)

      hostCreateGame(clientHost, probeClientHost, clientHostView = clientHostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, clientHostView, clientJoinerView)

      var gameInProgress = createStubGameInProgress(round = 0, List(
        PlayerPlaying(correctPlayerID(hostId, clientHost), hostName, 0, null),
        PlayerPlaying(correctPlayerID(joinerId, clientJoiner), joinerName, 1, null)
      ))

      val coordinatorProbe = testKit.createTestProbe[Message]()

      startGame(clientHost, probeClientHost, clientHostView, List((clientJoiner, probeClientJoiner, clientJoinerView)), coordinatorProbe, gameInProgress)

      testKit.stop(clientHost)
      testKit.stop(clientJoiner)
    }

    "should be able to start a game with a true coordinator" in {
      val (clientHost, probeClientHost, clientHostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, clientJoinerView) = createClientAndProbeWithView(joinerId, joinerName)

      hostCreateGame(clientHost, probeClientHost, clientHostView = clientHostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, clientHostView, clientJoinerView)

      clientHost ! StartTheGame()
      probeClientHost.expectMessage(StartTheGame())

      probeClientHost.expectMessageType[StartGameBehavior]

      probeClientHost.expectMessageType[TakeGetInProgressGame]

      probeClientJoiner.expectMessageType[GameHasStarted]

      testKit.stop(clientHost)
      testKit.stop(clientJoiner)
    }

    "should be able to pass round around" in {

      val (clientHost, probeClientHost, clientHostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, clientJoinerView) = createClientAndProbeWithView(joinerId, joinerName)

      hostCreateGame(clientHost, probeClientHost, clientHostView = clientHostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, clientHostView, clientJoinerView)

//      val coordinatorProbe = testKit.createTestProbe[Message]()
//      hostStartGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, clientHostView, clientJoinerView, coordinatorProbe)

      var gameInProgress = createStubGameInProgress(round = 0, List(
        PlayerPlaying(correctPlayerID(hostId, clientHost), hostName, 0, null),
        PlayerPlaying(correctPlayerID(joinerId, clientJoiner), joinerName, 1, null)
      ))

      val coordinatorProbe = testKit.createTestProbe[Message]()

      startGame(clientHost, probeClientHost, null, List((clientJoiner, probeClientJoiner, clientJoinerView)), coordinatorProbe, gameInProgress)

      clientHost ! TurnEnded(gameInProgress, null)
      probeClientHost.expectMessage(TurnEnded(gameInProgress, null))

      probeClientJoiner.expectMessageType[GameInProgressUpdate]
//      clientJoinerView.expectMessageType[GameInfoUpdate]

      // simulate gameCoordinator sending TurnUpdated
      clientJoiner ! TurnUpdated()
      probeClientJoiner.expectMessage(TurnUpdated())

      probeClientHost.receiveMessage(10.seconds) match {
        case SynchronizationAck(id) if id.contains(joinerId) => // ok
        case _ => fail("Host probe expected SynchronizationAck message")
      }

      gameInProgress = gameInProgress.copy(currentRound = 1)

      clientJoiner ! TurnEnded(gameInProgress, null)
      probeClientJoiner.expectMessage(TurnEnded(gameInProgress, null))

      probeClientHost.expectMessage(GameInProgressUpdate(clientJoiner, gameInProgress, null))

      // simulate gameCoordinator sending TurnUpdated
      clientHost ! TurnUpdated()
      probeClientHost.expectMessage(TurnUpdated())

      val ack = probeClientJoiner.expectMessageType[SynchronizationAck](10.seconds)

      assert(
        ack.fromWho.contains(hostId),
        s"Expected SynchronizationAck containing hostId '$hostId', but got '${ack.fromWho}'"
      )

      testKit.stop(clientHost)
      testKit.stop(clientJoiner)

    }

//    "should elect a new host if the previous leaves" in {
////      val (host, probeHost, hostView) = createClientAndProbeWithView(hostId, hostName)
//      val (host, probeHost) = createClientAndProbe(hostId, hostName)
//
////      val (joiner, probeJoiner, joinerView) = createClientAndProbeWithView(joinerId, joinerName)
//      val (joiner, probeJoiner) = createClientAndProbe(joinerId, joinerName)
//
////      val (joinerToo, probeJoinerToo, joinerTooView) = createClientAndProbeWithView(joinerTooId, joinerTooName)
//      val (joinerToo, probeJoinerToo) = createClientAndProbe(joinerTooId, joinerTooName)
//
////      hostCreateGame(host, probeHost, clientHostView = hostView)
//      hostCreateGame(host, probeHost)
//
////      joinHostGame(host, probeHost, joiner, probeJoiner, hostView, joinerView)
//      joinHostGame(host, probeHost, joiner, probeJoiner)
//
//      //      joinHostGame(host, probeHost, joinerToo, probeJoinerToo, hostView, joinerTooView)
//      joinHostGame(host, probeHost, joinerToo, probeJoinerToo)
//
//      // joiner should receive a notification about joinerToo joining
//      probeJoiner.expectMessageType[UpdateAboutGame]
//
//      var game = createStubGameInProgress(round = 0, List(
//        PlayerPlaying(correctPlayerID(hostId, host), hostName, 0, null),
//        PlayerPlaying(correctPlayerID(joinerId, joiner), joinerName, 1, null),
//        PlayerPlaying(correctPlayerID(joinerTooId, joinerToo), joinerTooName, 2, null)
//      ))
//
//      val coordinatorProbe = hostStartGame2(host, probeHost, null, List((joiner, probeJoiner, null), (joinerToo, probeJoinerToo, null)), game)
//
//      // Now the host leaves the game
//      host ! LeaveTheGame()
//      probeHost.expectMessage(LeaveTheGame())
//
//      probeJoiner.expectMessageType[PlayerUnreachable]
//      probeJoinerToo.expectMessageType[PlayerUnreachable]
//
//      // The new host should be joiner
//      probeJoinerToo.expectMessageType[ElectionStarted]
//
//      probeJoiner.expectMessageType[ElectionWon]
//
//      probeJoinerToo.expectMessageType[NewHostElected]
//
//    }
}