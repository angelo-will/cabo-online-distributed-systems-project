import akka.actor.testkit.typed.scaladsl.TestProbe
import akka.actor.typed.ActorSystem
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import akka.remote.testkit.{MultiNodeConfig, MultiNodeSpec, MultiNodeSpecCallbacks}
import akka.testkit.ImplicitSender
import com.typesafe.config.ConfigFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import akka.actor.typed.scaladsl.adapter.*
import akka.cluster.Cluster
import akka.cluster.ClusterEvent.{CurrentClusterState, MemberUp}
import akka.remote.testconductor.{RoleName, TestConductor}
import controller.Client
import controller.Client.{IWantToPlay, PlayerUnreachable, YouJoinedTheGame}
import org.scalatest.concurrent.Eventually.eventually
import org.scalatest.concurrent.Futures.{interval, timeout}
import utils.ClientMessages.{JoinAddress, JoinGame}
import utils.{ClientMessages, Message}

import java.net.InetSocketAddress
import scala.language.implicitConversions
import scala.concurrent.duration.DurationInt

/**
 * Hooks up MultiNodeSpec with ScalaTest
 */
trait STMultiNodeSpec extends MultiNodeSpecCallbacks with AnyWordSpecLike with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {
  self: MultiNodeSpec =>

  override def beforeAll() = multiNodeSpecBeforeAll()

  override def afterAll() = multiNodeSpecAfterAll()

  // Might not be needed anymore if we find a nice way to tag all logging from a node
  override implicit def convertToWordSpecStringWrapper(s: String): WordSpecStringWrapper =
    new WordSpecStringWrapper(s"$s (on node '${self.myself.name}', $getClass)")
}

object MultiNodeConfig extends MultiNodeConfig {
  val node1 = role("node1")
  val node2 = role("node2")

  commonConfig(ConfigFactory.parseString("""
    akka.actor.provider = cluster
    """).withFallback(ConfigFactory.load()))
}

class MultiNodeSpecClientMultiJvmNode1 extends ClientMultiNode
class MultiNodeSpecClientMultiJvmNode2 extends ClientMultiNode

abstract class ClientMultiNode extends MultiNodeSpec(MultiNodeConfig) with STMultiNodeSpec with ImplicitSender {

  import MultiNodeConfig._

  override def initialParticipants: Int = roles.size

  implicit val typedSystem: ActorSystem[Nothing] = system.toTyped

  "A Client on a node" should {

    "illustrate how to startup cluster" in within(15.seconds) {
      Cluster(system).subscribe(testActor, classOf[MemberUp])
      expectMsgClass(classOf[CurrentClusterState])

      val firstAddress = node(node1).address
      val secondAddress = node(node2).address
      Cluster(system).join(firstAddress)

      receiveN(2).collect { case MemberUp(m) => m.address }.toSet should be(
        Set(firstAddress, secondAddress))

      Cluster(system).unsubscribe(testActor)

      testConductor.enter("all-up")
    }

    "be able to join a game create on another node" in {
      runOn(node1) {
        val probeHost = TestProbe[Message]()
        val host = system.spawn(Behaviors.monitor(probeHost.ref, Client("host", "Gino")), "Host")

        host ! ClientMessages.CreateNewGame()
        probeHost.receiveMessages(1)

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("hostgame"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("hostgame")).contains(host))
        }

        enterBarrier("host-game-created")

        enterBarrier("join-message-sent")

        probeHost.expectMessageType[IWantToPlay]

      }

      runOn(node2) {
        val probeClient = TestProbe[Message]()
        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client", "Gino")), "Client")

        enterBarrier("host-game-created")

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("hostgame"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("hostgame")).map(_.path.name).contains("Host"))
        }

        client ! ClientMessages.JoinAGame()
        probeClient.expectMessage(ClientMessages.JoinAGame())

        client ! JoinAddress("hostgame")
        probeClient.expectMessage(ClientMessages.JoinAddress("hostgame"))

        enterBarrier("join-message-sent")

        probeClient.expectMessageType[YouJoinedTheGame]
      }
    }

    "be notified if a player disconnect" in {
      runOn(node1) {
        val probeHost = TestProbe[Message]()
        val host = system.spawn(Behaviors.monitor(probeHost.ref, Client("host2", "Gino")), "Host2")

        host ! ClientMessages.CreateNewGame()
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

        enterBarrier("player-disconnected")


      }

      runOn(node2) {
        val probeClient = TestProbe[Message]()
        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client", "Gino")), "Client2")

        enterBarrier("host-game-created")

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host2game"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("host2game")).map(_.path.name).contains("Host2"))
        }

        client ! ClientMessages.JoinAGame()
        probeClient.expectMessage(ClientMessages.JoinAGame())

        client ! JoinAddress("host2game")
        probeClient.expectMessage(ClientMessages.JoinAddress("host2game"))

        enterBarrier("join-message-sent")

        probeClient.expectMessageType[YouJoinedTheGame]

        enterBarrier("player-joined")

        enterBarrier("player-disconnected")

      }

    }

    enterBarrier("test-completed")

  }
}