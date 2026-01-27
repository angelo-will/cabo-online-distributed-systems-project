import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.typed.{Cluster, Join}
import com.typesafe.config.ConfigFactory
import controller.Client
import controller.Client.*
import controller.ViewsProxyActor.SwitchToGameView
import model.Game.GameInProgress
import model.GameStatus.InProgress
import model.{GameParameters, GameStatus, PlayerInLobby, PlayerPlaying}
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import messages.ClientMessages.*
import messages.GameCoordinatorMessage.*
import messages.GameViewMessages.*
import messages.{GameCoordinatorMessage, GameViewMessages, IGameCoordinatorMessage, Message}
import messages.PreGameViewMessages.*
import model.TurnEvent.JumpTurnForDisconnection
import org.scalatest.concurrent.Eventually.eventually
import org.scalatest.concurrent.Futures.{interval, timeout}

class ClientTest extends ScalaTestWithActorTestKit(ConfigFactory.parseString(
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

    clientJoiner ! JoinWithGameCode(hostPlayerID + "game")
    probeClientJoiner.expectMessage(JoinWithGameCode(hostPlayerID + "game"))

    probeClientHost.expectMessage(IWantToPlay(PlayerInLobby(joinerPlayerID, joinerName, clientJoiner), clientJoiner))

    clientHostView match {
      case null => // do nothing
      case vp =>
        val m = vp.expectMessageType[GameInfoUpdate]
        assert(m.game.players.exists(p => p.userID == joinerPlayerID || p.userID == hostPlayerID))
      //        vp.receiveMessage() match {
      //          case GameInfoUpdate(game) =>
      //            assert(game.players.exists(p => p.userID == joinerPlayerID))
      //            assert(game.players.exists(p => p.userID == hostPlayerID))
      //          case _ => fail("Expected GameInfoUpdate message")
      //        }
    }

    probeClientJoiner.receiveMessage() match {
      case YouJoinedTheGame(game) =>
        assert(game.players.exists(p => p.userID == hostPlayerID))
      case _ => fail("Expected YouJoinedTheGame message")
    }

    clientJoinerView match {
      case null => // do nothing
      case vp =>
        val m = vp.expectMessageType[FailedToPublishToServer]
      //        vp.receiveMessage() match {
      //          case FailedToPublishToServer() => // trying to find games
      //          case _ => fail("Joiner View expected message about server publishing")
      //        }
    }

    clientJoinerView match {
      case null => // do nothing
      case vp =>
        val m = vp.expectMessageType[GameJoined]
        assert(m.game.players.exists(p => p.userID == joinerPlayerID || p.userID == hostPlayerID))
      //        vp.receiveMessage() match {
      //          case GameJoined(game) =>
      //            assert(game.players.exists(p => p.userID == joinerPlayerID))
      //            assert(game.players.exists(p => p.userID == hostPlayerID))
      //          case _ => fail("Expected GameJoined message")
      //        }
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
      case InitialPhaseCompleted(_) => // ok
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
    hostView.expectMessageType[WaitAfterRevealingSection]

    joiners.foreach { case (joiner, probeJoiner, joinerView) =>
      val j = joinerView.expectMessageType[GameViewMessages.StartGame]
      val joinerCoo = j.gameCoordinatorRef

      joinerCoo ! GameCoordinatorMessage.ShowOwnNthCard(0)
      joinerView.expectMessageType[CardSeen]
      joinerCoo ! GameCoordinatorMessage.ShowOwnNthCard(0)
      joinerView.expectMessageType[CardSeen]

      probeJoiner.expectMessageType[InitialPhaseCompleted]
      joinerView.expectMessageType[WaitAfterRevealingSection]

      coordinators = coordinators :+ joinerCoo
    }

    //host receives the log of the other clients
    probeHost.receiveMessages(joiners.size).foreach(m => assert(m.isInstanceOf[InitialPhaseCompleted], "Host expected InitialPhaseCompleted message"))

    //+1 for own log
    hostView.receiveMessages(joiners.size+1).foreach(m => assert(m.isInstanceOf[PreCyclePhaseAdversaryLog], "Host View expected PreCyclePhaseAdversaryLog message"))

    //host respond with all the logs and joiner receives them
    joiners.foreach { case (joiner, probeJoiner, joinerView) =>
      probeJoiner.expectMessageType[AllTheLogs]
      //+1 for own log
      joinerView.receiveMessages(joiners.size+1).foreach(m => assert(m.isInstanceOf[PreCyclePhaseAdversaryLog], "Host View expected PreCyclePhaseAdversaryLog message"))
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

  "A client" should {

    "be able to leave while playing if it is the host" in {

      val (clientHost, probeClientHost, hostView) = createClientAndProbeWithView(hostId, hostName)
      val (clientJoiner, probeClientJoiner, joinerView) = createClientAndProbeWithView(joinerId, joinerName)
      val (clientTooJoiner, probeClientTooJoiner, joinerTooView) = createClientAndProbeWithView(joinerTooId, joinerTooName)

      hostCreateGame(clientHost, probeClientHost, hostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, hostView, joinerView)
      joinHostGame(clientHost, probeClientHost, clientTooJoiner, probeClientTooJoiner, hostView, joinerTooView)

      probeClientJoiner.expectMessageType[UpdateAboutGame]
      joinerView.expectMessageType[GameInfoUpdate]

      startGameTrue(clientHost, probeClientHost, hostView,
        List((clientJoiner, probeClientJoiner, joinerView), (clientTooJoiner, probeClientTooJoiner, joinerTooView))
      )

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()
      probeClientHost.expectMessageType[LeaveTheGame]

      var m = probeClientJoiner.expectMessageType[IWantToLeaveTheGame]
      assert(m.player.userID.contains(hostId))

      m = probeClientTooJoiner.expectMessageType[IWantToLeaveTheGame]
      assert(m.player.userID.contains(hostId))

      eventually(timeout(10.seconds), interval(500.millis)) {
        val m = probeClientJoiner.receiveMessage()
        assert(m.isInstanceOf[ElectionWon])
      }

      eventually(timeout(10.seconds), interval(500.millis)) {
        val m = probeClientTooJoiner.receiveMessage()
        assert(m.isInstanceOf[NewHostElected])
      }

      var m_w = probeClientJoiner.expectMessageType[WhoIsPlaying]
      assert(m_w.playerID.contains(hostId))

      var m_te = probeClientJoiner.expectMessageType[TurnEnded]
      assert(m_te.turnLog.events.contains(JumpTurnForDisconnection()))

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
      stopAndWait(clientTooJoiner)
    }

    "be able to leave while playing if it is a joiner" in {

      val (clientHost, probeClientHost, hostView) = createClientAndProbeWithView(hostId, hostName)
      val (clientJoiner, probeClientJoiner, joinerView) = createClientAndProbeWithView(joinerId, joinerName)
      val (clientTooJoiner, probeClientTooJoiner, joinerTooView) = createClientAndProbeWithView(joinerTooId, joinerTooName)

      hostCreateGame(clientHost, probeClientHost, hostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, hostView, joinerView)
      joinHostGame(clientHost, probeClientHost, clientTooJoiner, probeClientTooJoiner, hostView, joinerTooView)

      probeClientJoiner.expectMessageType[UpdateAboutGame]
      joinerView.expectMessageType[GameInfoUpdate]

      val List(hostCoo, _, _) = startGameTrue(clientHost, probeClientHost, hostView,
        List((clientJoiner, probeClientJoiner, joinerView), (clientTooJoiner, probeClientTooJoiner, joinerTooView))
      )

      // Now the player leaves the game
      clientJoiner ! LeaveTheGame()
      probeClientJoiner.expectMessage(LeaveTheGame())

      var m = probeClientHost.expectMessageType[IWantToLeaveTheGame]
      assert(m.player.userID.contains(joinerId))
      hostView.expectMessageType[OpponentDisconnected]

      m = probeClientTooJoiner.expectMessageType[IWantToLeaveTheGame]
      assert(m.player.userID.contains(joinerId))
      joinerTooView.expectMessageType[OpponentDisconnected]

      var m_w = probeClientHost.expectMessageType[WhoIsPlaying]
      assert(m_w.playerID.contains(hostId))

      hostCoo ! GameCoordinatorMessage.TurnTimeEnded()
      hostView.expectMessageType[GameViewMessages.EndTurnByTimeEnded](30.seconds)
      probeClientHost.expectMessageType[TurnEnded](30.seconds)

      // has left, so no messages expected
      probeClientJoiner.expectNoMessage()

      probeClientTooJoiner.expectMessageType[GameInProgressUpdate]
      probeClientTooJoiner.expectMessageType[TurnUpdated]

      probeClientHost.expectMessageType[SynchronizationAck]

      m_w = probeClientHost.expectMessageType[WhoIsPlaying]
      assert(m_w.playerID.contains(joinerId))

      var m_te = probeClientHost.expectMessageType[TurnEnded]
      assert(m_te.turnLog.events.contains(JumpTurnForDisconnection()))

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
      stopAndWait(clientTooJoiner)
    }

    "be able to join a game created by another player" in {

      val (clientHost, probeClientHost, hostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, joinerView) = createClientAndProbeWithView(joinerId, joinerName)

      hostCreateGame(clientHost, probeClientHost, hostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, hostView, joinerView)

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
    }

    "not be able to join a game that is already full" in {

      val (clientHost, probeClientHost, hostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, joinerView) = createClientAndProbeWithView(joinerId, joinerName)

      val (clientTooJoiner, probeClientTooJoiner, joinerTooView) = createClientAndProbeWithView(joinerTooId, joinerTooName)

      hostCreateGame(clientHost, probeClientHost, hostView, maxPlayers = 2)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, hostView, joinerView)

      // Simulate the game being full
      clientTooJoiner ! JoinAGame()
      probeClientTooJoiner.expectMessage(JoinAGame())

      val (hostPlayerID, _) = retrieveClientIdAndName(clientHost, probeClientHost)

      clientTooJoiner ! JoinWithGameCode(hostPlayerID + "game")
      probeClientTooJoiner.expectMessageType[JoinWithGameCode]

      probeClientTooJoiner.receiveMessage() match {
        case YouCanNotJoinTheGame(game) =>
          assert(game.players.size == 2)
          assert(game.players.exists(p => p.userID.contains(hostId)))
          assert(game.players.exists(p => p.userID.contains(joinerId)))
        case _ => fail("Expected YouCanNotJoinTheGame message")
      }

      clientHost ! LeaveTheGame()

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
      stopAndWait(clientTooJoiner)
    }

    "receive a notification when another player joins the game" in {

      val (clientHost, probeClientHost, hostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, joinerView) = createClientAndProbeWithView(joinerId, joinerName)

      val (clientTooJoiner, probeClientTooJoiner, joinerTooView) = createClientAndProbeWithView(joinerTooId, joinerTooName)

      hostCreateGame(clientHost, probeClientHost, hostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, hostView, joinerView)

      joinHostGame(clientHost, probeClientHost, clientTooJoiner, probeClientTooJoiner, hostView, joinerTooView)

      probeClientJoiner.receiveMessage() match {
        case UpdateAboutGame(game) =>
          assert(game.players.size == 3)
          assert(game.players.exists(p => p.userID.contains(hostId)))
          assert(game.players.exists(p => p.userID.contains(joinerId)))
          assert(game.players.exists(p => p.userID.contains(joinerTooId)))
        case _ => fail("Expected UpdateAboutGame message")
      }

      probeClientHost.expectNoMessage()
      probeClientTooJoiner.expectNoMessage()

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
      stopAndWait(clientTooJoiner)
    }

    "be able to leave a joined game" in {

      val (clientHost, probeClientHost, hostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, joinerView) = createClientAndProbeWithView(joinerId, joinerName)

      val (clientTooJoiner, probeClientTooJoiner, joinerTooView) = createClientAndProbeWithView(joinerTooId, joinerTooName)

      hostCreateGame(clientHost, probeClientHost, hostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, hostView, joinerView)

      // Now the player leaves the game
      clientJoiner ! LeaveTheGame()
      probeClientJoiner.expectMessage(LeaveTheGame())

      // The host should receive a notification about the player leaving
      val m = probeClientHost.expectMessageType[IWantToLeaveTheGame]

      assert(m.player.userID.contains(joinerId))

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
      stopAndWait(clientTooJoiner)
    }

    "be notified if someone leave the game" in {

      val (clientHost, probeClientHost, hostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, joinerView) = createClientAndProbeWithView(joinerId, joinerName)

      val (clientTooJoiner, probeClientTooJoiner, joinerTooView) = createClientAndProbeWithView(joinerTooId, joinerTooName)

      hostCreateGame(clientHost, probeClientHost, hostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, hostView, joinerView)

      joinHostGame(clientHost, probeClientHost, clientTooJoiner, probeClientTooJoiner, hostView, joinerTooView)

      // Player02 expects the join message for Player03
      probeClientJoiner.receiveMessage() match {
        case UpdateAboutGame(game) =>
          assert(game.players.size == 3)
          assert(game.players.exists(p => p.userID.contains(hostId)))
          assert(game.players.exists(p => p.userID.contains(joinerId)))
          assert(game.players.exists(p => p.userID.contains(joinerTooId)))
      }

      // Now the player leaves the game
      clientJoiner ! LeaveTheGame()
      probeClientJoiner.expectMessage(LeaveTheGame())

      probeClientTooJoiner.receiveMessage() match {
        case UpdateAboutGame(game) =>
          assert(game.players.size == 2)
          assert(game.players.exists(p => p.userID.contains(hostId)))
          assert(game.players.exists(p => p.userID.contains(joinerTooId)))
      }

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
      stopAndWait(clientTooJoiner)
    }

    "should receive an abort notification if the host leaves the game" in {

      val (clientHost, probeClientHost, hostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, joinerView) = createClientAndProbeWithView(joinerId, joinerName)

      val (clientTooJoiner, probeClientTooJoiner, joinerTooView) = createClientAndProbeWithView(joinerTooId, joinerTooName)

      hostCreateGame(clientHost, probeClientHost, hostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, hostView, joinerView)

      // Now the host leaves the game
      clientHost ! LeaveTheGame()
      probeClientHost.expectMessage(LeaveTheGame())

      // The joiner should receive an abort notification
      probeClientJoiner.expectMessage(GameCancelled())

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
      stopAndWait(clientTooJoiner)
    }

    "should be able to enter a game using an 'address' (code)" in {

      val (clientHost, probeClientHost, hostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, joinerView) = createClientAndProbeWithView(joinerId, joinerName)

      hostCreateGame(clientHost, probeClientHost, hostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, hostView, joinerView)

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
    }

    "should be able to change the name of the player" in {

      val (clientHost, probeClientHost, hostView) = createClientAndProbeWithView(hostId, hostName)

      val probe = testKit.createTestProbe[Message]()

      clientHost ! GetPlayerInfo(probe.ref)
      probeClientHost.expectMessage(GetPlayerInfo(probe.ref))

      var m = probe.expectMessageType[PlayerInfo]

      assert(m.userID.contains(hostId) && (m.name equals hostName))

      val newCoolName = "NewCoolName"
      clientHost ! ChangePlayerName(newCoolName, probe.ref)
      probeClientHost.expectMessage(ChangePlayerName(newCoolName, probe.ref))

      m = probe.expectMessageType[PlayerInfo]

      assert(m.userID.contains(hostId) && (m.name equals newCoolName))

      // should be able to retrieve player information after creating a game
      hostCreateGame(clientHost, probeClientHost)

      clientHost ! GetPlayerInfo(probe.ref)
      probeClientHost.expectMessage(GetPlayerInfo(probe.ref))

      m = probe.expectMessageType[PlayerInfo]

      assert(m.userID.contains(hostId) && (m.name equals newCoolName))

      // Remove the game from the receptionist
      clientHost ! LeaveTheGame()
      probeClientHost.expectMessage(LeaveTheGame())

      clientHost ! JoinAGame()
      probeClientHost.expectMessage(JoinAGame())

      clientHost ! GetPlayerInfo(probe.ref)
      probeClientHost.expectMessage(GetPlayerInfo(probe.ref))

      m = probe.expectMessageType[PlayerInfo]

      assert(m.userID.contains(hostId) && (m.name equals newCoolName))

      stopAndWait(clientHost)
    }

    "should be able to start a game with a false coordinator" in {

      val (clientHost, probeClientHost, clientHostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, clientJoinerView) = createClientAndProbeWithView(joinerId, joinerName)

      hostCreateGame(clientHost, probeClientHost, clientHostView = clientHostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, clientHostView, clientJoinerView)

      val (h_id, _) = retrieveClientIdAndName(clientHost, probeClientHost)
      val (j_id, _) = retrieveClientIdAndName(clientJoiner, probeClientJoiner)

      val gameInProgress = createStubGameInProgress(round = 0, List(
        PlayerPlaying(h_id, hostName, 0, null),
        PlayerPlaying(j_id, joinerName, 1, null)
      ))

      val coordinatorProbe = testKit.createTestProbe[Message]()

      startGame(clientHost, probeClientHost, clientHostView, List((clientJoiner, probeClientJoiner, clientJoinerView)), coordinatorProbe, gameInProgress)

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
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

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
    }

    "should be able to pass round around" in {

      val (clientHost, probeClientHost, clientHostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, clientJoinerView) = createClientAndProbeWithView(joinerId, joinerName)

      hostCreateGame(clientHost, probeClientHost, clientHostView = clientHostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, clientHostView, clientJoinerView)

      val (h_id, _) = retrieveClientIdAndName(clientHost, probeClientHost)
      val (j_id, _) = retrieveClientIdAndName(clientJoiner, probeClientJoiner)

      var gameInProgress = createStubGameInProgress(round = 0, List(
        PlayerPlaying(h_id, hostName, 0, null),
        PlayerPlaying(j_id, joinerName, 1, null)
      ))

      val coordinatorProbe = testKit.createTestProbe[Message]()

      startGame(clientHost, probeClientHost, clientHostView, List((clientJoiner, probeClientJoiner, clientJoinerView)), coordinatorProbe, gameInProgress)

      clientHost ! TurnEnded(gameInProgress, null)
      probeClientHost.expectMessage(TurnEnded(gameInProgress, null))

      probeClientJoiner.expectMessageType[GameInProgressUpdate]

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

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
    }

    "should be able to play with a true coordinator" in {
      val (clientHost, probeClientHost, clientHostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, clientJoinerView) = createClientAndProbeWithView(joinerId, joinerName)

      hostCreateGame(clientHost, probeClientHost, maxTimeRound = 1, clientHostView = clientHostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, clientHostView, clientJoinerView)

      val List(hostCoo, joinerCoo) = startGameTrue(clientHost, probeClientHost, clientHostView,
        List((clientJoiner, probeClientJoiner, clientJoinerView))
      )

      //simulate first turn played by host, for test purposes we just end the turn by time ended
      hostCoo ! GameCoordinatorMessage.TurnTimeEnded()
      clientHostView.expectMessageType[GameViewMessages.EndTurnByTimeEnded](30.seconds)
      probeClientHost.expectMessageType[TurnEnded](30.seconds)
      probeClientJoiner.expectMessageType[GameInProgressUpdate]
      probeClientJoiner.expectMessageType[TurnUpdated]
      probeClientHost.expectMessageType[SynchronizationAck]

      //host receive who is the next player
      var m_id = probeClientHost.expectMessageType[WhoIsPlaying](10.seconds)
      assert(
        m_id.playerID.contains(joinerId),
        s"Expected WhoIsPlaying containing joinerId '$joinerId', but got '${m_id.playerID}'"
      )

      //simulate first turn played by joiner, for test purposes we just end the turn by time ended
      joinerCoo ! GameCoordinatorMessage.TurnTimeEnded()
      probeClientJoiner.expectMessageType[TurnEnded](30.seconds)

      probeClientJoiner.expectMessageType[SynchronizationAck]

      //host receives who is the next player
      m_id = probeClientJoiner.expectMessageType[WhoIsPlaying](10.seconds)
      assert(
        m_id.playerID.contains(hostId),
        s"Expected WhoIsPlaying containing hostId '$hostId', but got '${m_id.playerID}'"
      )

      probeClientHost.expectMessageType[GameInProgressUpdate]
      probeClientHost.expectMessageType[TurnUpdated]

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
    }
  }