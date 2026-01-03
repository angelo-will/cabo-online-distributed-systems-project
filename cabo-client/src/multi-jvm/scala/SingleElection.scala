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
import messages.ClientMessages.{JoinAddress, IntialPhaseCompleted, StartTheGame, TakeGetInProgressGame}

import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

class SingleElectionMultiJvmNode1 extends SingleElection
class SingleElectionMultiJvmNode2 extends SingleElection
class SingleElectionMultiJvmNode3 extends SingleElection

abstract class SingleElection extends MultiNodeSpec(MultiNodeConfig) with STMultiNodeSpec with ImplicitSender{

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

    "change host if the host disconnects with a single election" in {
      // This test can be implemented similarly by having the host disconnect and verifying that another player is promoted to host.

      // node1 is a joinee because node2 will be the host and disconnect
      runOn(node1) {
        // Client code
        val probeClient = TestProbe[Message]()
        val viewProbe = TestProbe[Message]()
        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client3-1", "Gino", viewProbe.ref)), "Client3-1")

        enterBarrier("game-created")

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host3game"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("host3game")).map(_.path.name).contains("Host3"))
        }

        client ! ClientMessages.JoinAGame()
        probeClient.expectMessage(ClientMessages.JoinAGame())

        client ! JoinAddress("host3game")
        probeClient.expectMessage(ClientMessages.JoinAddress("host3game"))

        probeClient.expectMessageType[YouJoinedTheGame]

        enterBarrier("player2-joined")

        enterBarrier("player3-joined")

        probeClient.expectMessageType[UpdateAboutGame]

        enterBarrier("game-started")

        probeClient.expectMessageType[GameHasStarted]

        //preGamePhase
        enterBarrier("pre-game-phase")

        client ! IntialPhaseCompleted(null)
        probeClient.expectMessageType[IntialPhaseCompleted]

        enterBarrier("all-the-logs-sent")

        probeClient.expectMessageType[AllTheLogs]

        enterBarrier("pre-game-phase-completed")

        enterBarrier("removed-host-check")

        testConductor.exit(node2, 0)

        val m = probeClient.expectMessageType[PlayerUnreachable]

        assert(m.playerInLobby.userID contains "host3")

        probeClient.expectMessageType[ElectionWon](10.seconds)

      }

      runOn(node2) {
        // Host code
        val probeHost = TestProbe[Message]()
        val viewProbe = TestProbe[Message]()
        val host = system.spawn(Behaviors.monitor(probeHost.ref, Client("host3", "Gino", viewProbe.ref)), "Host3")

        host ! ClientMessages.CreateNewGame(gameCode = Some("host3game"))
        probeHost.receiveMessages(1)

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host3game"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("host3game")).contains(host))
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
        host ! IntialPhaseCompleted(null)
        probeHost.expectMessageType[IntialPhaseCompleted]

        //receive log of other clients
        probeHost.expectMessageType[InitialGamePhaseLog]
        probeHost.expectMessageType[InitialGamePhaseLog]

        enterBarrier("all-the-logs-sent")

        probeHost.expectMessageType[SynchronizationAck]
        probeHost.expectMessageType[SynchronizationAck]

        enterBarrier("pre-game-phase-completed")

        // theoretically not necessary but to keep the barriers aligned
        enterBarrier("removed-host-check")

        // now die to simulate host failure

      }


      runOn(node3) {
        // Another Client code
        val probeClient = TestProbe[Message]()
        val viewProbe = TestProbe[Message]()
        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client3-2", "Gino", viewProbe.ref)), "Client3-2")

        enterBarrier("game-created")

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host3game"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("host3game")).map(_.path.name).contains("Host3"))
        }

        enterBarrier("player2-joined")

        client ! ClientMessages.JoinAGame()
        probeClient.expectMessage(ClientMessages.JoinAGame())

        client ! JoinAddress("host3game")
        probeClient.expectMessage(ClientMessages.JoinAddress("host3game"))

        probeClient.expectMessageType[YouJoinedTheGame]

        enterBarrier("player3-joined")

        enterBarrier("game-started")

        probeClient.expectMessageType[GameHasStarted]

        //preGamePhase
        enterBarrier("pre-game-phase")

        client ! IntialPhaseCompleted(null)
        probeClient.expectMessageType[IntialPhaseCompleted]

        enterBarrier("all-the-logs-sent")

        probeClient.expectMessageType[AllTheLogs]

        enterBarrier("pre-game-phase-completed")

        client ! RemoveCheckPlayerStatus()
        probeClient.expectMessageType[RemoveCheckPlayerStatus]

        enterBarrier("removed-host-check")

        val m = probeClient.expectMessageType[NewHostElected](10.seconds)

        assert(m.replyTo.toString.toLowerCase contains "client3-1")
      }
    }
    enterBarrier("test-completed")
  }
}
