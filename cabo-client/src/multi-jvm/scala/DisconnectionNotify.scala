import akka.actor.testkit.typed.scaladsl.TestProbe
import akka.actor.typed.ActorSystem
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter.*
import akka.cluster.Cluster
import akka.cluster.ClusterEvent.{CurrentClusterState, MemberUp}
import akka.remote.testconductor.RoleName
import akka.remote.testkit.MultiNodeSpec
import akka.testkit.ImplicitSender
import controller.Client
import controller.Client.*
import messages.{ClientMessages, Message}
import org.scalatest.concurrent.Eventually.eventually
import org.scalatest.concurrent.Futures.{interval, timeout}
import messages.ClientMessages.JoinWithGameCode

import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

class DiscNotifySpecClientMultiJvmNode1 extends DisconnectionNotify

class DiscNotifySpecClientMultiJvmNode2 extends DisconnectionNotify

class DiscNotifySpecClientMultiJvmNode3 extends DisconnectionNotify

abstract class DisconnectionNotify extends MultiNodeSpec(MultiNodeConfig) with STMultiNodeSpec with ImplicitSender {

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

    "be notified if a player disconnect" in {
      runOn(node1) {
        val probeHost = TestProbe[Message]()
        val viewProbe = TestProbe[Message]()
        val host = system.spawn(Behaviors.monitor(probeHost.ref, Client("host2", "Gino", viewProbe.ref)), "Host2")

        host ! ClientMessages.CreateNewGame(gameCode = Some("host2game"))
        probeHost.receiveMessages(1)

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host2game"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("host2game")).contains(host))
        }

        enterBarrier("host-game-created")

        enterBarrier("join-message-sent")

        probeHost.expectMessageType[IWantToPlay]

        enterBarrier("player-joined")

        testConductor.exit(node2, 0)

        probeHost.expectMessageType[PlayerUnreachable]
      }

      runOn(node2) {
        val probeClient = TestProbe[Message]()
        val viewProbe = TestProbe[Message]()
        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client", "Gino", viewProbe.ref)), "Client2")

        enterBarrier("host-game-created")

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host2game"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("host2game")).map(_.path.name).contains("Host2"))
        }

        client ! ClientMessages.JoinAGame()
        probeClient.expectMessage(ClientMessages.JoinAGame())

        client ! JoinWithGameCode("host2game")
        probeClient.expectMessage(ClientMessages.JoinWithGameCode("host2game"))

        enterBarrier("join-message-sent")

        probeClient.expectMessageType[YouJoinedTheGame]

        enterBarrier("player-joined")
      }

      runOn(node3) {
        enterBarrier("host-game-created")
        enterBarrier("join-message-sent")
        enterBarrier("player-joined")
      }
    }
    enterBarrier("test-completed")
  }
}
