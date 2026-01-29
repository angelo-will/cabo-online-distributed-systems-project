import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.cluster.typed.{Cluster, Join}
import com.typesafe.config.ConfigFactory
import controller.Client
import controller.Client.*
import controller.ViewsProxyActor.SwitchToGameView
import messages.ClientMessages.*
import messages.GameCoordinatorMessage.*
import messages.GameViewMessages.*
import messages.PreGameViewMessages.*
import messages.{GameCoordinatorMessage, GameViewMessages, IGameCoordinatorMessage, Message}
import model.Game.GameInProgress
import model.GameStatus.InProgress
import model.{GameParameters, GameStatus, PlayerInLobby, PlayerPlaying}
import org.scalatest.concurrent.Eventually.eventually
import org.scalatest.concurrent.Futures.{interval, timeout}
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

class ClientTestCommons extends ScalaTestWithActorTestKit(ConfigFactory.parseString(
  """
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
    val client = testKit.spawn(Behaviors.monitor(probe.ref, Client(id, name, viewProbe.ref)))
    (client, probe, viewProbe)
  }

  def createClientAndProbe(id: String = "ClientID", name: String = "ClientName"): (ActorRef[Message], TestProbe[Message]) = {
    val probe = testKit.createTestProbe[Message]()
    val client = testKit.spawn(Behaviors.monitor(probe.ref, Client(id, name)))
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

    clientJoinerView match {
      case null => // do nothing
      case vp =>
        val m = vp.expectMessageType[FailedToPublishToServer]
    }

    clientJoiner ! JoinWithGameCode(hostPlayerID + "game")
    probeClientJoiner.expectMessage(JoinWithGameCode(hostPlayerID + "game"))

    probeClientHost.expectMessage(IWantToPlay(PlayerInLobby(joinerPlayerID, joinerName, clientJoiner), clientJoiner))

    clientHostView match {
      case null => // do nothing
      case vp =>
        val m = vp.expectMessageType[GameInfoUpdate]
        assert(m.game.players.exists(p => p.userID == joinerPlayerID || p.userID == hostPlayerID))
    }

    probeClientJoiner.receiveMessage() match {
      case YouJoinedTheGame(game) =>
        assert(game.players.exists(p => p.userID == hostPlayerID))
      case _ => fail("Expected YouJoinedTheGame message")
    }


    clientJoinerView match {
      case null => // do nothing
      case vp =>
        val m = vp.expectMessageType[GameJoined]
        assert(m.game.players.exists(p => p.userID == joinerPlayerID || p.userID == hostPlayerID))
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

    val (hostId, _) = retrieveClientIdAndName(clientHost, probeClientHost)

    val coordinatorStub: () => Behavior[IGameCoordinatorMessage] = () => Behaviors.receiveMessage {
      m =>
        coordinatorProbe.ref ! m
        Behaviors.same
    }

    clientHost ! StartGameBehavior(coordinatorStub, clientHost)
    probeClientHost.expectMessage(StartGameBehavior(coordinatorStub, clientHost))

    clientHost ! TakeGetInProgressGame(game)
    probeClientHost.expectMessage(TakeGetInProgressGame(game))

    coordinatorProbe.expectMessageType[StartPrePlayCycleSection]

    joiners.foreach { case (clientJoiner, probeClientJoiner, clientJoinerView) =>

      val msg = probeClientJoiner.expectMessageType[GameHasStarted]
      assert(
        msg.gameInProgress.players.exists(p => p.userID == hostId),
        s"GameHasStarted does not contains the correct player: $msg"
      )

      clientJoinerView match {
        case null => // do nothing
        case vp =>
          vp.expectMessageType[GameStarted]
          vp.expectMessageType[SwitchToGameView]
      }
    }

    probeClientHost.receiveMessages(joiners.size).foreach {
      case SynchronizationAck(id) => // ok
      case _ => fail("Host probe expected SynchronizationAck message")
    }

    //In preGame phase

    // simulate gameCoordinator sending RevealingCardsPhaseLog in host
    clientHost ! InitialPhaseCompleted(null)
    probeClientHost.expectMessage(InitialPhaseCompleted(null))

    joiners.foreach { case (clientJoiner, probeClientJoiner, clientJoinerView) =>
      // simulate gameCoordinator sending RevealingCardsPhaseLog in joiners
      clientJoiner ! InitialPhaseCompleted(null)
      probeClientJoiner.expectMessageType[InitialPhaseCompleted]
    }

    // host receives the log of the other clients
    probeClientHost.receiveMessages(joiners.size).foreach {
      case AdversaryLogInfo(_) => // ok
      case _ => fail("Host probe expected logs message")
    }

    joiners.foreach { case (clientJoiner, probeClientJoiner, clientJoinerView) =>
      probeClientJoiner.expectMessageType[AllTheLogs]
    }

    probeClientHost.receiveMessages(joiners.size).foreach {
      case SynchronizationAck(id) => // ok
      case _ => fail("Host probe expected SynchronizationAck message")
    }

    coordinatorProbe.expectMessageType[StartPlayCycle]

  }

  def stopAndWait(ref: ActorRef[Message]): Unit = {
    val probe = testKit.createTestProbe[Message]()
    testKit.stop(ref)
    probe.expectTerminated(ref)
  }

  def startGameTrue(host: ActorRef[Message], probeHost: TestProbe[Message], hostView: TestProbe[Message],
                    joiners: List[(ActorRef[Message], TestProbe[Message], TestProbe[Message])]): List[ActorRef[IGameCoordinatorMessage]] = {

    var coordinators: List[ActorRef[IGameCoordinatorMessage]] = List.empty

    host ! StartTheGame()
    probeHost.expectMessage(StartTheGame())

    hostView.expectMessageType[SwitchToGameView]

    probeHost.expectMessageType[StartGameBehavior]

    probeHost.expectMessageType[TakeGetInProgressGame]

    joiners.foreach { case (joiner, probeJoiner, joinerView) => probeJoiner.expectMessageType[GameHasStarted] }

    // ack from joiner to enter prePreGame
    probeHost.receiveMessages(joiners.size).foreach(m => assert(m.isInstanceOf[SynchronizationAck], "Host probe expected SynchronizationAck message"))

    joiners.foreach { case (joiner, probeJoiner, joinerView) =>
      joinerView.expectMessageType[GameStarted]
      joinerView.expectMessageType[SwitchToGameView]
    }

    val h = hostView.expectMessageType[GameViewMessages.StartGame]
    val hostCoo = h.gameCoordinatorRef

    coordinators = coordinators :+ hostCoo

    hostCoo ! GameCoordinatorMessage.ShowOwnNthCard(0)
    hostView.expectMessageType[CardSeen]
    hostCoo ! GameCoordinatorMessage.ShowOwnNthCard(0)
    hostView.expectMessageType[CardSeen]

    probeHost.expectMessageType[InitialPhaseCompleted]
    hostView.expectMessageType[WaitAfterPreCycleSection]

    joiners.foreach { case (joiner, probeJoiner, joinerView) =>
      val j = joinerView.expectMessageType[GameViewMessages.StartGame]
      val joinerCoo = j.gameCoordinatorRef

      joinerCoo ! GameCoordinatorMessage.ShowOwnNthCard(0)
      joinerView.expectMessageType[CardSeen]
      joinerCoo ! GameCoordinatorMessage.ShowOwnNthCard(0)
      joinerView.expectMessageType[CardSeen]

      probeJoiner.expectMessageType[InitialPhaseCompleted]
      joinerView.expectMessageType[WaitAfterPreCycleSection]

      coordinators = coordinators :+ joinerCoo
    }

    //host receives the log of the other clients
    probeHost.receiveMessages(joiners.size).foreach(m => assert(m.isInstanceOf[AdversaryLogInfo], "Host expected InitialPhaseCompleted message"))

    //+1 for own log
    hostView.receiveMessages(joiners.size + 1).foreach(m => assert(m.isInstanceOf[PreCyclePhaseAdversaryLog], "Host View expected PreCyclePhaseAdversaryLog message"))

    //host respond with all the logs and joiner receives them
    joiners.foreach { case (joiner, probeJoiner, joinerView) =>
      probeJoiner.expectMessageType[AllTheLogs]
      //+1 for own log
      joinerView.receiveMessages(joiners.size + 1).foreach(m => assert(m.isInstanceOf[PreCyclePhaseAdversaryLog], "Host View expected PreCyclePhaseAdversaryLog message"))
    }

    //joiner ack the reception of all the logs
    probeHost.receiveMessages(joiners.size).foreach(m => assert(m.isInstanceOf[SynchronizationAck], "Host expected Sync message"))

    //host and join send StartPlayCycle to coordinator

    hostView.expectMessageType[StartTurnPlayer]
    joiners.foreach { case (_, _, joinerView) =>
      joinerView.expectMessageType[StartTurnPlayer]
    }

    //END PREPHASE_______________________________________________________

    coordinators
  }
