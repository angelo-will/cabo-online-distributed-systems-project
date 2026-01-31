import controller.Client
import controller.Client.*
import messages.ClientMessages.*
import messages.GameViewMessages.*
import messages.PreGameViewMessages.*
import messages.{GameCoordinatorMessage, GameViewMessages}
import model.TurnEvent.JumpTurnForDisconnection
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

class ClientAndGameCoordinatorTest extends ClientTestCommons {

  "A client, using a GameCoordinator, " should {

    "be able to start a game with a true coordinator" in {
      val (clientHost, probeClientHost, clientHostView) = createClientAndProbeWithView(hostId, hostName)

      val (clientJoiner, probeClientJoiner, clientJoinerView) = createClientAndProbeWithView(joinerId, joinerName)

      hostCreateGame(clientHost, probeClientHost, clientHostView = clientHostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, clientHostView, clientJoinerView)

      clientHost ! StartTheGame()
      probeClientHost.expectMessage(StartTheGame())

      probeClientHost.expectMessageType[StartGameBehavior]

      probeClientHost.expectMessageType[TakeGameInProgress]

      probeClientJoiner.expectMessageType[GameHasStarted]

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
    }

    "be able to play with a true coordinator" in {
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

    "be able to leave while playing if it is a joinee" in {

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
      
      // message resent by each other
      probeClientHost.expectMessageType[IWantToLeaveTheGame]
      probeClientTooJoiner.expectMessageType[IWantToLeaveTheGame]

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

    "leave if remains alone in a game" in {

      val (clientHost, probeClientHost, hostView) = createClientAndProbeWithView(hostId, hostName)
      val (clientJoiner, probeClientJoiner, joinerView) = createClientAndProbeWithView(joinerId, joinerName)

      hostCreateGame(clientHost, probeClientHost, hostView)

      joinHostGame(clientHost, probeClientHost, clientJoiner, probeClientJoiner, hostView, joinerView)

      startGameTrue(clientHost, probeClientHost, hostView,
        List((clientJoiner, probeClientJoiner, joinerView))
      )

      // Now the player leaves the game
      clientJoiner ! LeaveTheGame()
      probeClientJoiner.expectMessage(LeaveTheGame())

      // The host should receive a notification about the player leaving
      val m = probeClientHost.expectMessageType[IWantToLeaveTheGame]
      assert(m.player.userID.contains(joinerId))

      // The host should automatically leave the game as it is the only one left
      hostView.expectMessageType[AllOpponentsDisconnected]

      stopAndWait(clientHost)
      stopAndWait(clientJoiner)
    }
  }

}
