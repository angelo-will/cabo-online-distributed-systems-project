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

/**
 * Hooks up MultiNodeSpec with ScalaTest
 */
//trait STMultiNodeSpec extends MultiNodeSpecCallbacks with AnyWordSpecLike with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {
//  self: MultiNodeSpec =>
//
//  override def beforeAll() = multiNodeSpecBeforeAll()
//
//  override def afterAll() = multiNodeSpecAfterAll()
//
//  // Might not be needed anymore if we find a nice way to tag all logging from a node
//  override implicit def convertToWordSpecStringWrapper(s: String): WordSpecStringWrapper =
//    new WordSpecStringWrapper(s"$s (on node '${self.myself.name}', $getClass)")
//}
//
//object MultiNodeConfig extends MultiNodeConfig {
//  val node1 = role("node1")
//  val node2 = role("node2")
//  val node3 = role("node3")
//
//  commonConfig(ConfigFactory.parseString("""
//    akka.actor.provider = cluster
//    akka.loglevel = "OFF"
//    akka.testconductor.query-timeout = 30s
//    akka.testconductor.controller-startup-timeout = 30s
//    akka.testconductor.barriertimeout = 60s
//    akka.test.single-expect-default = 10s
//    """).withFallback(ConfigFactory.load()))
//}

class MultiNodeSpecClientMultiJvmNode1 extends ClientMultiNode

class MultiNodeSpecClientMultiJvmNode2 extends ClientMultiNode

class MultiNodeSpecClientMultiJvmNode3 extends ClientMultiNode

abstract class ClientMultiNode extends MultiNodeSpec(MultiNodeConfig) with STMultiNodeSpec with ImplicitSender {

  import MultiNodeConfig.*

  override def initialParticipants: Int = roles.size

  implicit val typedSystem: ActorSystem[Nothing] = system.toTyped

  "A Client on a node" should {

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

    "be able to join a game create on another node" in {
      runOn(node1) {
        val probeHost = TestProbe[Message]()
        val viewProbe = TestProbe[Message]()
        val host = system.spawn(Behaviors.monitor(probeHost.ref, Client("host", "Gino", viewProbe.ref)), "Host")

        host ! ClientMessages.CreateNewGame(gameCode = Some("hostgame"))
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
        val viewProbe = TestProbe[Message]()
        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client", "Gino", viewProbe.ref)), "Client")

        enterBarrier("host-game-created")

        val probe = TestProbe[Receptionist.Listing]()
        eventually(timeout(3.seconds), interval(100.millis)) {
          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("hostgame"), probe.ref)
          val listing = probe.receiveMessage()
          assert(listing.serviceInstances(ServiceKey[Message]("hostgame")).map(_.path.name).contains("Host"))
        }

        client ! ClientMessages.JoinAGame()
        probeClient.expectMessage(ClientMessages.JoinAGame())

        client ! JoinWithGameCode("hostgame")
        probeClient.expectMessage(ClientMessages.JoinWithGameCode("hostgame"))

        enterBarrier("join-message-sent")

        probeClient.expectMessageType[YouJoinedTheGame]
      }

      runOn(node3) {
        // Just a third node to make the cluster more realistic
        enterBarrier("host-game-created")
        enterBarrier("join-message-sent")
      }
    }

    //    "be notified if a player disconnect" in {
    //      runOn(node1) {
    //        val probeHost = TestProbe[Message]()
    //        val host = system.spawn(Behaviors.monitor(probeHost.ref, Client("host2", "Gino")), "Host2")
    //
    //        host ! ClientMessages.CreateNewGame(gameCode = Some("host2game"))
    //        probeHost.receiveMessages(1)
    //
    //        val probe = TestProbe[Receptionist.Listing]()
    //        eventually(timeout(3.seconds), interval(100.millis)) {
    //          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host2game"), probe.ref)
    //          val listing = probe.receiveMessage()
    //          assert(listing.serviceInstances(ServiceKey[Message]("host2game")).contains(host))
    //        }
    //
    //        enterBarrier("host-game-created")
    //
    //        enterBarrier("join-message-sent")
    //
    //        probeHost.expectMessageType[IWantToPlay]
    //
    //        enterBarrier("player-joined")
    //
    //        testConductor.exit(node2, 0)
    //
    //        probeHost.expectMessageType[PlayerUnreachable]
    //
    //        enterBarrier("player-disconnected")
    //      }
    //
    //      runOn(node2) {
    //        val probeClient = TestProbe[Message]()
    //        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client", "Gino")), "Client2")
    //
    //        enterBarrier("host-game-created")
    //
    //        val probe = TestProbe[Receptionist.Listing]()
    //        eventually(timeout(3.seconds), interval(100.millis)) {
    //          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host2game"), probe.ref)
    //          val listing = probe.receiveMessage()
    //          assert(listing.serviceInstances(ServiceKey[Message]("host2game")).map(_.path.name).contains("Host2"))
    //        }
    //
    //        client ! ClientMessages.JoinAGame()
    //        probeClient.expectMessage(ClientMessages.JoinAGame())
    //
    //        client ! JoinAddress("host2game")
    //        probeClient.expectMessage(ClientMessages.JoinAddress("host2game"))
    //
    //        enterBarrier("join-message-sent")
    //
    //        probeClient.expectMessageType[YouJoinedTheGame]
    //
    //        enterBarrier("player-joined")
    //
    //        enterBarrier("player-disconnected")
    //      }
    //
    //      runOn(node3) {
    //        enterBarrier("host-game-created")
    //        enterBarrier("join-message-sent")
    //        enterBarrier("player-joined")
    //        enterBarrier("player-disconnected")
    //      }
    //
    //    }
    //
    //    "change host if the host disconnects with a single election" in {
    //      // This test can be implemented similarly by having the host disconnect and verifying that another player is promoted to host.
    //
    //      // node1 is a joinee because node2 will be the host and disconnect
    //      runOn(node1) {
    //        // Client code
    //        val probeClient = TestProbe[Message]()
    //        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client3-1", "Gino")), "Client3-1")
    //
    //        enterBarrier("game-created")
    //
    //        val probe = TestProbe[Receptionist.Listing]()
    //        eventually(timeout(3.seconds), interval(100.millis)) {
    //          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host3game"), probe.ref)
    //          val listing = probe.receiveMessage()
    //          assert(listing.serviceInstances(ServiceKey[Message]("host3game")).map(_.path.name).contains("Host3"))
    //        }
    //
    //        client ! ClientMessages.JoinAGame()
    //        probeClient.expectMessage(ClientMessages.JoinAGame())
    //
    //        client ! JoinAddress("host3game")
    //        probeClient.expectMessage(ClientMessages.JoinAddress("host3game"))
    //
    //        probeClient.expectMessageType[YouJoinedTheGame]
    //
    //        enterBarrier("player2-joined")
    //
    //        enterBarrier("player3-joined")
    //
    //        probeClient.expectMessageType[UpdateAboutGame]
    //
    //        enterBarrier("game-started")
    //
    //        probeClient.expectMessageType[GameHasStarted]
    //
    //        enterBarrier("removed-host-check")
    //
    //        testConductor.exit(node2, 0)
    //
    //        val m = probeClient.expectMessageType[PlayerUnreachable]
    //
    //        assert(m.playerInLobby.userID contains "host3")
    //
    //        probeClient.expectMessageType[ElectionWon](10.seconds)
    //
    //      }
    //
    //      runOn(node2) {
    //        // Host code
    //        val probeHost = TestProbe[Message]()
    //        val host = system.spawn(Behaviors.monitor(probeHost.ref, Client("host3", "Gino")), "Host3")
    //
    //        host ! ClientMessages.CreateNewGame(gameCode = Some("host3game"))
    //        probeHost.receiveMessages(1)
    //
    //        val probe = TestProbe[Receptionist.Listing]()
    //        eventually(timeout(3.seconds), interval(100.millis)) {
    //          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host3game"), probe.ref)
    //          val listing = probe.receiveMessage()
    //          assert(listing.serviceInstances(ServiceKey[Message]("host3game")).contains(host))
    //        }
    //
    //        enterBarrier("game-created")
    //
    //        probeHost.expectMessageType[IWantToPlay]
    //
    //        enterBarrier("player2-joined")
    //
    //        probeHost.expectMessageType[IWantToPlay]
    //
    //        enterBarrier("player3-joined")
    //
    //        host ! StartTheGame()
    //        probeHost.expectMessageType[StartTheGame]
    //
    //        probeHost.expectMessageType[StartGameBehavior]
    //
    //        probeHost.expectMessageType[TakeGetInProgressGame]
    //
    //        enterBarrier("game-started")
    //
    //        // theoretically not necessary but to keep the barriers aligned
    //        enterBarrier("removed-host-check")
    //
    //        // now die to simulate host failure
    //
    //      }
    //
    //
    //      runOn(node3) {
    //        // Another Client code
    //        val probeClient = TestProbe[Message]()
    //        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client3-2", "Gino")), "Client3-2")
    //
    //        enterBarrier("game-created")
    //
    //        val probe = TestProbe[Receptionist.Listing]()
    //        eventually(timeout(3.seconds), interval(100.millis)) {
    //          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host3game"), probe.ref)
    //          val listing = probe.receiveMessage()
    //          assert(listing.serviceInstances(ServiceKey[Message]("host3game")).map(_.path.name).contains("Host3"))
    //        }
    //
    //        enterBarrier("player2-joined")
    //
    //        client ! ClientMessages.JoinAGame()
    //        probeClient.expectMessage(ClientMessages.JoinAGame())
    //
    //        client ! JoinAddress("host3game")
    //        probeClient.expectMessage(ClientMessages.JoinAddress("host3game"))
    //
    //        probeClient.expectMessageType[YouJoinedTheGame]
    //
    //        enterBarrier("player3-joined")
    //
    //        enterBarrier("game-started")
    //
    //        probeClient.expectMessageType[GameHasStarted]
    //
    //        client ! RemoveCheckPlayerStatus()
    //        probeClient.expectMessageType[RemoveCheckPlayerStatus]
    //
    //        enterBarrier("removed-host-check")
    //
    //        val m = probeClient.expectMessageType[NewHostElected](10.seconds)
    //
    //        assert(m.replyTo.toString.toLowerCase contains "client3-1")
    //
    //      }
    //    }
    //
    //    "change host if the host disconnects with multiple elections" in {
    //
    //      runOn(node1) {
    //        // Client code
    //        val probeClient = TestProbe[Message]()
    //        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client4-1/", "Gino")), "Client4-1")
    //
    //        enterBarrier("game-created")
    //
    //        val probe = TestProbe[Receptionist.Listing]()
    //        eventually(timeout(3.seconds), interval(100.millis)) {
    //          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host4game"), probe.ref)
    //          val listing = probe.receiveMessage()
    //          assert(listing.serviceInstances(ServiceKey[Message]("host4game")).map(_.path.name).contains("Host4"))
    //        }
    //
    //        client ! ClientMessages.JoinAGame()
    //        probeClient.expectMessage(ClientMessages.JoinAGame())
    //
    //        client ! JoinAddress("host4game")
    //        probeClient.expectMessage(ClientMessages.JoinAddress("host4game"))
    //
    //        probeClient.expectMessageType[YouJoinedTheGame]
    //
    //        enterBarrier("player2-joined")
    //
    //        enterBarrier("player3-joined")
    //
    //        probeClient.expectMessageType[UpdateAboutGame]
    //
    //        enterBarrier("game-started")
    //
    //        probeClient.expectMessageType[GameHasStarted]
    //
    //        client ! RemoveCheckPlayerStatus()
    //        probeClient.expectMessageType[RemoveCheckPlayerStatus]
    //
    //        enterBarrier("removed-host-check")
    //
    //        val exitFuture = testConductor.exit(node2, 0)
    //        Await.result(exitFuture, 30.seconds)
    //
    //        enterBarrier("host4-removed")
    //
    //        val m = probeClient.expectMessageType[ElectionStarted]
    //        assert((m.replyTo.toString.toLowerCase contains "client4-2") && m.candidateRank == 3)
    //
    //        probeClient.expectMessageType[ElectionWon](10.seconds)
    //
    //      }
    //
    //      runOn(node2) {
    //        // Host code
    //        val probeHost = TestProbe[Message]()
    //        val host = system.spawn(Behaviors.monitor(probeHost.ref, Client("host4", "Gino")), "Host4")
    //
    //        host ! ClientMessages.CreateNewGame(gameCode = Some("host4game"))
    //        probeHost.receiveMessages(1)
    //
    //        val probe = TestProbe[Receptionist.Listing]()
    //        eventually(timeout(3.seconds), interval(100.millis)) {
    //          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host4game"), probe.ref)
    //          val listing = probe.receiveMessage()
    //          assert(listing.serviceInstances(ServiceKey[Message]("host4game")).contains(host))
    //        }
    //
    //        enterBarrier("game-created")
    //
    //        probeHost.expectMessageType[IWantToPlay]
    //
    //        enterBarrier("player2-joined")
    //
    //        probeHost.expectMessageType[IWantToPlay]
    //
    //        enterBarrier("player3-joined")
    //
    //        host ! StartTheGame()
    //        probeHost.expectMessageType[StartTheGame]
    //
    //        probeHost.expectMessageType[StartGameBehavior]
    //
    //        probeHost.expectMessageType[TakeGetInProgressGame]
    //
    //        enterBarrier("game-started")
    //
    //        // theoretically not necessary but to keep the barriers aligned
    //        enterBarrier("removed-host-check")
    //
    //        // now die to simulate host failure
    ////        System.exit(0)
    //
    //        //        host ! LeaveTheGame()
    //        //
    //        //        enterBarrier("host4-removed")
    //
    //      }
    //
    //      runOn(node3) {
    //        // Another Client code
    //        val probeClient = TestProbe[Message]()
    //        val client = system.spawn(Behaviors.monitor(probeClient.ref, Client("client4-2/", "Gino")), "Client4-2")
    //
    //        enterBarrier("game-created")
    //
    //        val probe = TestProbe[Receptionist.Listing]()
    //        eventually(timeout(3.seconds), interval(100.millis)) {
    //          typedSystem.receptionist ! Receptionist.Find(ServiceKey[Message]("host4game"), probe.ref)
    //          val listing = probe.receiveMessage()
    //          assert(listing.serviceInstances(ServiceKey[Message]("host4game")).map(_.path.name).contains("Host4"))
    //        }
    //
    //        enterBarrier("player2-joined")
    //
    //        client ! ClientMessages.JoinAGame()
    //        probeClient.expectMessage(ClientMessages.JoinAGame())
    //
    //        client ! JoinAddress("host4game")
    //        probeClient.expectMessage(ClientMessages.JoinAddress("host4game"))
    //
    //        probeClient.expectMessageType[YouJoinedTheGame]
    //
    //        enterBarrier("player3-joined")
    //
    //        enterBarrier("game-started")
    //
    //        probeClient.expectMessageType[GameHasStarted]
    //
    //        enterBarrier("removed-host-check")
    //
    //        enterBarrier("host4-removed")
    //
    //        val m = probeClient.expectMessageType[PlayerUnreachable]
    //
    //        assert(m.playerInLobby.userID contains "host4")
    //
    //        val refuse = probeClient.expectMessageType[NoYouCanNot](10.seconds)
    //
    //        val hostElected = probeClient.expectMessageType[NewHostElected](10.seconds)
    //        assert(hostElected.replyTo.toString.toLowerCase contains "client4-1")
    //
    //      }
    //
    //    }

    enterBarrier("test-completed")

  }
}