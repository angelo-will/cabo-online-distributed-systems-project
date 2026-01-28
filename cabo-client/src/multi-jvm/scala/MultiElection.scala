import akka.actor.testkit.typed.scaladsl.TestProbe
import akka.actor.typed.ActorSystem
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter.*
import akka.cluster.Cluster
import akka.cluster.ClusterEvent.{CurrentClusterState, MemberUp}
import akka.remote.testkit.MultiNodeSpec
import akka.testkit.ImplicitSender
import controller.Client
import controller.Client.*
import messages.{ClientMessages, Message}
import org.scalatest.concurrent.Eventually.eventually
import org.scalatest.concurrent.Futures.{interval, timeout}
import messages.ClientMessages.{InitialPhaseCompleted, JoinWithGameCode, StartTheGame, TakeGetInProgressGame}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

class MultiElectionMultiJvmNode1 extends MultiElection

class MultiElectionMultiJvmNode2 extends MultiElection

class MultiElectionMultiJvmNode3 extends MultiElection

abstract class MultiElection extends MultiNodeSpec(MultiNodeConfig) with STMultiNodeSpec with ImplicitSender {

  import MultiNodeConfig.*

  override def initialParticipants: Int = roles.size

  implicit val typedSystem: ActorSystem[Nothing] = system.toTyped

  "Client on different nodes" should {

    "illustrate how to startup cluster" in within(15.seconds) {
      Cluster(system).subscribe(testActor, classOf[MemberUp])
      expectMsgClass(classOf[CurrentClusterState])

      val firstAddress = node(node1).address
      val secondAddress = node(node2).address
      val thirdAddress = node(node3).address
      Cluster(system).join(firstAddress)

      receiveN(3).collect { case MemberUp(m) => m.address }.toSet should be(
        Set(firstAddress, secondAddress, thirdAddress))

      Cluster(system).unsubscribe(testActor)

      testConductor.enter("all-up")
    }

    "change host if the host disconnects with multiple elections" in {
      runOn(node1) {
        // Client code
        val probeClient = TestProbe[Message]()
        val viewProbe = TestProbe[Message]()
        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client4-1/", "Gino", viewProbe.ref)), "Client4-1")

        enterBarrier("game-created")

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host4game"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("host4game")).map(_.path.name).contains("Host4"))
        }

        client ! ClientMessages.JoinAGame()
        probeClient.expectMessage(ClientMessages.JoinAGame())

        client ! JoinWithGameCode("host4game")
        probeClient.expectMessage(ClientMessages.JoinWithGameCode("host4game"))

        probeClient.expectMessageType[YouJoinedTheGame]

        enterBarrier("player2-joined")

        enterBarrier("player3-joined")

        probeClient.expectMessageType[UpdateAboutGame]

        enterBarrier("game-started")

        probeClient.expectMessageType[GameHasStarted]

        //preGamePhase
        enterBarrier("pre-game-phase")

        client ! InitialPhaseCompleted(null)
        probeClient.expectMessageType[InitialPhaseCompleted]

        enterBarrier("all-the-logs-sent")

        probeClient.expectMessageType[AllTheLogs]

        enterBarrier("pre-game-phase-completed")

        client ! RemoveCheckPlayerStatus()
        probeClient.expectMessageType[RemoveCheckPlayerStatus]

        enterBarrier("removed-host-check")

        val exitFuture = testConductor.shutdown(node2)
        Await.result(exitFuture, 30.seconds)

        enterBarrier("host4-removed")

        val m = probeClient.expectMessageType[ElectionStarted](10.seconds)
        assert((m.replyTo.toString.toLowerCase contains "client4-2") && m.candidateRank == 3)

        probeClient.expectMessageType[ElectionWon](10.seconds)

      }

      runOn(node2) {
        // Host code
        val probeHost = TestProbe[Message]()
        val viewProbe = TestProbe[Message]()
        val host = system.spawn(Behaviors.monitor(probeHost.ref, Client("host4", "Gino", viewProbe.ref)), "Host4")

        host ! ClientMessages.CreateNewGame(gameCode = Some("host4game"))
        probeHost.receiveMessages(1)

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host4game"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("host4game")).contains(host))
        }

        enterBarrier("game-created")

        probeHost.expectMessageType[IWantToPlay]

        enterBarrier("player2-joined")

        probeHost.expectMessageType[IWantToPlay]

        enterBarrier("player3-joined")

        host ! StartTheGame()
        probeHost.expectMessageType[StartTheGame]

        probeHost.expectMessageType[StartGameBehavior]

        probeHost.expectMessageType[TakeGetInProgressGame]

        probeHost.expectMessageType[SynchronizationAck]
        probeHost.expectMessageType[SynchronizationAck]

        enterBarrier("game-started")

        enterBarrier("pre-game-phase")

        //preGamePhase

        //simulate revealing cards phase for host player
        host ! InitialPhaseCompleted(null)
        probeHost.expectMessageType[InitialPhaseCompleted]

        //receive log of other clients
        probeHost.expectMessageType[AdversaryLogInfo]
        probeHost.expectMessageType[AdversaryLogInfo]

        enterBarrier("all-the-logs-sent")

        probeHost.expectMessageType[SynchronizationAck]
        probeHost.expectMessageType[SynchronizationAck]

        enterBarrier("pre-game-phase-completed")

        // theoretically not necessary but to keep the barriers aligned
        enterBarrier("removed-host-check")
      }

      runOn(node3) {
        // Another Client code
        val probeClient = TestProbe[Message]()
        val viewProbe = TestProbe[Message]()
        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client4-2/", "Gino", viewProbe.ref)), "Client4-2")

        enterBarrier("game-created")

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host4game"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("host4game")).map(_.path.name).contains("Host4"))
        }

        enterBarrier("player2-joined")

        client ! ClientMessages.JoinAGame()
        probeClient.expectMessage(ClientMessages.JoinAGame())

        client ! JoinWithGameCode("host4game")
        probeClient.expectMessage(ClientMessages.JoinWithGameCode("host4game"))

        probeClient.expectMessageType[YouJoinedTheGame]

        enterBarrier("player3-joined")

        enterBarrier("game-started")

        probeClient.expectMessageType[GameHasStarted]

        //preGamePhase
        enterBarrier("pre-game-phase")

        client ! InitialPhaseCompleted(null)
        probeClient.expectMessageType[InitialPhaseCompleted]

        enterBarrier("all-the-logs-sent")

        probeClient.expectMessageType[AllTheLogs]

        enterBarrier("pre-game-phase-completed")

        enterBarrier("removed-host-check")

        enterBarrier("host4-removed")

        val m = probeClient.expectMessageType[PlayerUnreachable](10.seconds)

        assert(m.playerInLobby.userID contains "host4")

        val refuse = probeClient.expectMessageType[NoYouCanNot](10.seconds)

        val hostElected = probeClient.expectMessageType[NewHostElected](10.seconds)
        assert(hostElected.replyTo.toString.toLowerCase contains "client4-1")

      }
    }
    enterBarrier("test-completed")
  }
}
