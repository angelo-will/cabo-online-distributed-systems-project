import akka.actor.testkit.typed.scaladsl.TestProbe
import akka.actor.typed.ActorRef
import controller.Client
import controller.Client.*
import messages.ClientMessages.*
import messages.Message
import model.Game.GameInProgress
import model.{GameParameters, GameStatus, PlayerInLobby, PlayerPlaying}
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

class ClientTest extends ClientTestCommons:

  "A client" should {

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

    "should be able to enter a game using the code of the game" in {

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
      probeClientJoiner.expectMessageType[GameCancelled]

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
      stopAndWait(clientTooJoiner)
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
  }
