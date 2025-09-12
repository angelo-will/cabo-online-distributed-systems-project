import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.typed.{Cluster, Join}
import controller.Client
import model.Game.GameInConstruction
import model.{GameParameters, PlayerInLobby}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import utils.ClientMessages.{CreateNewGame, StartTheGame}
import utils.Message
import utils.ServerMessages.{GamesList, GetGames, RegisterGame, ServerKey, StartGame}
import utils.InitialViewMessages.*

import scala.concurrent.duration.DurationInt

class SystemTest extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with Matchers:

  var probeServer: TestProbe[Message] = _
  var server: ActorRef[Message] = _

  override def beforeAll(): Unit =
    val cluster = Cluster.get(testKit.system)
    cluster.manager.tell(Join.create(cluster.selfMember.address))

    probeServer = testKit.createTestProbe[Message]()
    server = testKit.spawn(Behaviors.monitor(probeServer.ref, Server()))

    val probe = TestProbe[Receptionist.Listing]()
    eventually(timeout(3.seconds), interval(100.millis)) {
      system.receptionist ! Receptionist.Find(ServerKey, probe.ref)
      val listing = probe.receiveMessage()
      assert(listing.serviceInstances(ServerKey).contains(server))
    }

  override def afterAll(): Unit = testKit.shutdownTestKit()

  def correctPlayerID(name: String, ref: ActorRef[Message]): String =
    name + ref.path.address.hashCode()

  "Client" should {
    "create game correctly and send it to the server" in {
      val defaultName = "defaultCoolName"

      val hostUserID = "Player01"
      val probeClientHost = testKit.createTestProbe[Message]()
      val clientHost = testKit.spawn(Behaviors.monitor(probeClientHost.ref, Client(hostUserID)))

      val hostPlayerID = correctPlayerID(hostUserID, clientHost)

      //      val probeClientJoiner = testKit.createTestProbe[Message]()
      //      val clientJoiner = testKit.spawn(Behaviors.monitor(probeClientJoiner.ref, Client("Player02")))

      clientHost ! CreateNewGame(makePublic = true, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4)

      val gameInConstruction = GameInConstruction(hostPlayerID + "game", GameParameters(true, 10, 5, 4), List(PlayerInLobby(hostPlayerID, defaultName, clientHost)))

      probeServer.expectMessage(RegisterGame(gameInConstruction, clientHost))

      clientHost ! StartTheGame()

      val probe = testKit.createTestProbe[Message]()

      eventually(timeout(3.seconds), interval(100.millis)) {
        server ! GetGames(probe.ref)
        probe.expectMessage(GamesList(Set()))
      }
    }
  }