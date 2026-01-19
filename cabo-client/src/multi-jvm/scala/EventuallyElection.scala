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
import messages.ClientMessages.{JoinWithGameCode, IntialPhaseCompleted, StartTheGame, TakeGetInProgressGame}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

class EventuallyElectionMultiJvmNode1 extends EventuallyElection

class EventuallyElectionMultiJvmNode2 extends EventuallyElection

class EventuallyElectionMultiJvmNode3 extends EventuallyElection

abstract class EventuallyElection extends MultiNodeSpec(MultiNodeConfig) with STMultiNodeSpec with ImplicitSender {

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
        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client5-1/", "Gino", viewProbe.ref)), "client5-1")

        enterBarrier("game-created")

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host5game"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("host5game")).map(_.path.name).contains("Host4"))
        }

        client ! ClientMessages.JoinAGame()
        probeClient.expectMessage(ClientMessages.JoinAGame())

        client ! JoinWithGameCode("host5game")
        probeClient.expectMessage(ClientMessages.JoinWithGameCode("host5game"))

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

        //simulate crash
        val exitFuture = testConductor.exit(node2, 0)
        Await.result(exitFuture, 30.seconds)

        enterBarrier("host5-removed")

        eventually(timeout(10.seconds), interval(500.millis)) {
          val m = probeClient.receiveMessage()
          assert(m.isInstanceOf[ElectionWon])
        }

      }

      runOn(node2) {
        // Host code
        val probeHost = TestProbe[Message]()
        val viewProbe = TestProbe[Message]()
        val host = system.spawn(Behaviors.monitor(probeHost.ref, Client("host5", "Gino", viewProbe.ref)), "Host4")

        host ! ClientMessages.CreateNewGame(gameCode = Some("host5game"))
        probeHost.receiveMessages(1)

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host5game"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("host5game")).contains(host))
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
        probeHost.expectMessageType[IntialPhaseCompleted]
        probeHost.expectMessageType[IntialPhaseCompleted]

        enterBarrier("all-the-logs-sent")

        probeHost.expectMessageType[SynchronizationAck]
        probeHost.expectMessageType[SynchronizationAck]

        enterBarrier("pre-game-phase-completed")


      }

      runOn(node3) {
        // Another Client code
        val probeClient = TestProbe[Message]()
        val viewProbe = TestProbe[Message]()
        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client5-2/", "Gino", viewProbe.ref)), "client5-2")

        enterBarrier("game-created")

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host5game"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("host5game")).map(_.path.name).contains("Host4"))
        }

        enterBarrier("player2-joined")

        client ! ClientMessages.JoinAGame()
        probeClient.expectMessage(ClientMessages.JoinAGame())

        client ! JoinWithGameCode("host5game")
        probeClient.expectMessage(ClientMessages.JoinWithGameCode("host5game"))

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

        enterBarrier("host5-removed")

        eventually(timeout(10.seconds), interval(500.millis)) {
          val m = probeClient.receiveMessage()
          assert(m.isInstanceOf[NewHostElected])
          assert(m.asInstanceOf[NewHostElected].replyTo.toString.toLowerCase contains "client5-1")
        }

      }
    }
    enterBarrier("test-completed")
  }

}