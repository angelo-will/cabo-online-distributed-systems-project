import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import akka.cluster.typed.{Cluster, Join}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*
import model.Game.GameInConstruction
import model.{GameParameters, PlayerInLobby}
import utils.{Message, ServerMessages}
import utils.ServerMessages.*

class ServerTest extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with Matchers:

  import org.scalatest.matchers.must.Matchers.mustBe

  val serverCode = "TestServer"
  var server: ActorRef[Message] = _
  var testProbe: TestProbe[Message] = _

  override def beforeAll(): Unit =
    val cluster = Cluster.get(testKit.system)
    cluster.manager.tell(Join.create(cluster.selfMember.address))

  override def beforeEach(): Unit =
    server = testKit.spawn(Server())
    testProbe = testKit.createTestProbe[Message]()

  override def afterEach(): Unit =
    // Clear the server's game list after each test to avoid state leakage

    eventually(timeout(3.seconds), interval(100.millis)) {
      server ! ClearGames(testProbe.ref)
      testProbe.expectMessage(GamesCleared(server))
      server ! GetGames(testProbe.ref)
      testProbe.expectMessage(GamesList(Set()))
    }

  override def afterAll(): Unit =
    testKit.shutdownTestKit()

  "Server" must {
    "send empty games list" when {
      "someone requests games but nobody has registered one of them" in {
        server ! GetGames(testProbe.ref)
        testProbe.expectMessage(GamesList(Set()))
      }
    }
    "send a GameRegistered message" when {
      "someone sends to Server a RegisterGame message" in {
        val game = GameInConstruction("codeGame", GameParameters(maxTimeRound = 10), List.empty)
        server ! RegisterGame(game, testProbe.ref)
        testProbe.expectMessage(GameRegistered(game, server))
      }
    }
    "send a GamesList message with games not started" when {
      "someone requests games" in {
        val game1 = GameInConstruction("codeGame1", GameParameters(maxTimeRound = 10), List.empty)
        val game2 = GameInConstruction("codeGame2", GameParameters(maxTimeRound = 10), List.empty)
        server ! RegisterGame(game1, testProbe.ref)
        server ! RegisterGame(game2, testProbe.ref)
        server ! GetGames(testProbe.ref)

        val message = testProbe.receiveMessages(3, 5.seconds).filter(_.isInstanceOf[GamesList]).head

        message mustBe GamesList(Set(game1, game2))
      }
    }
    "send a GameList message without a game" when {
      "the game has been already started" in {
        val game1 = GameInConstruction("codeGame1", GameParameters(maxTimeRound = 10), List.empty)
        server ! RegisterGame(game1, testProbe.ref)
        val registerGame = testProbe.receiveMessage()
        registerGame match {
          case GameRegistered(game, ref) =>
            server ! StartGame(game, testProbe.ref)
            server ! GetGames(testProbe.ref)
            testProbe.expectMessage(GamesList(Set()))
          case _ =>
            fail("Expected GameRegistered message")
        }
      }
    }
    "send a GameList message without a game" when {
      "the game has been aborted previously" in {
        val game1 = GameInConstruction("codeGame1", GameParameters(maxTimeRound = 10), List.empty)
        server ! RegisterGame(game1, testProbe.ref)
        val registerGame = testProbe.receiveMessage()
        registerGame match {
          case GameRegistered(game, ref) =>
            server ! AbortGame(game, testProbe.ref)
            server ! GetGames(testProbe.ref)
            testProbe.expectMessage(GamesList(Set()))
          case _ =>
            fail("Expected GameRegistered message")
        }
      }
    }
    "send a GameList message with games not started" when {
      "even if they were add in other server instance" in {
        val server2 = testKit.spawn(Server())
        val game = GameInConstruction("codeGame", GameParameters(maxTimeRound = 10), List.empty)
        server2 ! RegisterGame(game, testProbe.ref)

        testProbe.expectMessage(GameRegistered(game, server2))

        server ! GetGames(testProbe.ref)

        testProbe.expectMessage(GamesList(Set(game)))
      }
    }
    "must not modify other games" when {
      "update the information about a game" in {
        val gameToUpdate = GameInConstruction("codeGameToUpdate", GameParameters(maxTimeRound = 10), List.empty)
        val game = GameInConstruction("codeGame", GameParameters(maxTimeRound = 10), List.empty)
        server ! RegisterGame(gameToUpdate, testProbe.ref)
        testProbe.expectMessage(GameRegistered(gameToUpdate, server))

        server ! RegisterGame(game, testProbe.ref)
        testProbe.expectMessage(GameRegistered(game, server))

        val gameUpdated = gameToUpdate.copy(players = List(PlayerInLobby("id1", "Io", testProbe.ref)))

        server ! UpdateGame(gameUpdated, testProbe.ref)
        testProbe.expectMessage(GameRegistered(gameUpdated, server))

        server ! GetGames(testProbe.ref)
        testProbe.expectMessage(GamesList(Set(game, gameUpdated)))
      }
    }
  }
