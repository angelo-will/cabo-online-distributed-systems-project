import MultiNodeConfig.{commonConfig, role}
import akka.actor.testkit.typed.scaladsl.TestProbe
import akka.actor.typed.ActorSystem
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter.*
import akka.cluster.Cluster
import akka.cluster.ClusterEvent.{CurrentClusterState, MemberUp}
import akka.remote.testkit.{MultiNodeConfig, MultiNodeSpec, MultiNodeSpecCallbacks}
import akka.testkit.ImplicitSender
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import controller.Client
import messages.{ClientMessages, Message, ServerMessages}
import org.scalatest.concurrent.Eventually.eventually
import org.scalatest.concurrent.Futures.{interval, timeout}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import messages.ClientMessages.CreateNewGame
import messages.ServerMessages.{RegisterGame, ServerKey}

import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

trait STMultiNodeSpec extends MultiNodeSpecCallbacks with AnyWordSpecLike with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {
  self: MultiNodeSpec =>

  override def beforeAll() = multiNodeSpecBeforeAll()

  override def afterAll() = multiNodeSpecAfterAll()

  // Might not be needed anymore if we find a nice way to tag all logging from a node
  override implicit def convertToWordSpecStringWrapper(s: String): WordSpecStringWrapper =
    new WordSpecStringWrapper(s"$s (on node '${self.myself.name}', $getClass)")
}

object MultiNodeConfig extends MultiNodeConfig {
  val serverNode = role("serverNode")
  val clientNode = role("clientNode")

  val commonConfig: Config = ConfigFactory.parseString(s"""
    akka.actor.provider = cluster
    """).withFallback(ConfigFactory.load())

  commonConfig(commonConfig)

  nodeConfig(serverNode)(ConfigFactory.parseString(s"""
    akka.cluster.roles = [server]
    """))
}

class SystemNodeSpecMultiJvmNode1 extends MultiNodeSystemTest
class SystemNodeSpecMultiJvmNode2 extends MultiNodeSystemTest


abstract class MultiNodeSystemTest extends MultiNodeSpec(MultiNodeConfig) with STMultiNodeSpec with ImplicitSender {

  import MultiNodeConfig.*

  override def initialParticipants: Int = roles.size

  implicit val typedSystem: ActorSystem[Nothing] = system.toTyped

  "The system" should {

    "illustrate how to startup cluster" in within(15.seconds) {
      Cluster(system).subscribe(testActor, classOf[MemberUp])
      expectMsgClass(classOf[CurrentClusterState])

      val firstAddress = node(serverNode).address
      val secondAddress = node(clientNode).address
      Cluster(system).join(firstAddress)

      receiveN(2).collect { case MemberUp(m) => m.address }.toSet should be(
        Set(firstAddress, secondAddress))

      Cluster(system).unsubscribe(testActor)

      testConductor.enter("all-up")
    }

    "Server node should have the 'server' role" in {
      runOn(serverNode) {
        Cluster(system).selfRoles should contain("server")
      }

      runOn(clientNode) {
        Cluster(system).selfRoles shouldNot contain("server")
      }

      enterBarrier("checked-roles")
    }

    "allow a client on a node to send a game to the server, which is in another node" in {

      runOn(serverNode) {
        val probeServer = TestProbe[Message]()
        val server = system.spawn(Behaviors.monitor(probeServer.ref, Server()), "Server")

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServerKey, probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServerKey).contains(server))
        }

        enterBarrier("server-started")

        probeServer.expectMessageType[RegisterGame]
      }

      runOn(clientNode) {
        val probeClient = TestProbe[Message]()
        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client", "Gino")), "Client")

        enterBarrier("server-started")

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServerKey, probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServerKey).size == 1)
        }

        client ! ClientMessages.CreateNewGame(makePublic = true)
        probeClient.expectMessage(CreateNewGame(makePublic = true))

        // Wait for the server to receive the game registration
        probeClient.expectMessageType[ServerMessages.GameRegistered]
      }

      enterBarrier("test-completed")

    }
  }
}
