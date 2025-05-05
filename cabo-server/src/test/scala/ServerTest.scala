import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import akka.cluster.typed.{Cluster, Join}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*
import model.GameInConstruction
import model.GameParameters
import utils.{Message, ServerMessages}

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
    server = testKit.spawn(Server(serverCode))
    testProbe = testKit.createTestProbe[Message]()

  override def afterAll(): Unit =
    testKit.shutdownTestKit()

  "Server" must {
    "send empty games list" when {
      "someone requests games but nobody has registered one of them" in {
        server ! ServerMessages.GetGames(testProbe.ref)
        testProbe.expectMessage(ServerMessages.GamesList(Seq()))
      }
    }
    "send a GameRegistered message" when {
      "someone sends to Server a RegisterGame message" in {
        val game = GameInConstruction("codeGame", GameParameters(maxTimeRound = 10), List.empty)
        server ! ServerMessages.RegisterGame(game, testProbe.ref)
        testProbe.expectMessage(ServerMessages.GameRegistered(game, server))

        //Remove the game from the sever because the list is persistent between test
        server ! ServerMessages.StartGame(game, testProbe.ref)
      }
    }
    "send a GamesList message with games not started" when {
      "someone requests games" in {
        val game1 = GameInConstruction("codeGame1", GameParameters(maxTimeRound = 10), List.empty)
        val game2 = GameInConstruction("codeGame2", GameParameters(maxTimeRound = 10), List.empty)
        server ! ServerMessages.RegisterGame(game1, testProbe.ref)
        server ! ServerMessages.RegisterGame(game2, testProbe.ref)
        server ! ServerMessages.GetGames(testProbe.ref)

        val message = testProbe.receiveMessages(3, 5.seconds).filter(_.isInstanceOf[ServerMessages.GamesList]).head

        message mustBe ServerMessages.GamesList(List(game1, game2))

        //Remove the games from the sever because the list is persistent between test
        server ! ServerMessages.StartGame(game1, testProbe.ref)
        server ! ServerMessages.StartGame(game2, testProbe.ref)
      }
    }
    "send a GameList message without a game" when {
      "the game has been already started" in {
        val game1 = GameInConstruction("codeGame1", GameParameters(maxTimeRound = 10), List.empty)
        server ! ServerMessages.RegisterGame(game1, testProbe.ref)
        val registerGame = testProbe.receiveMessage()
        registerGame match {
          case ServerMessages.GameRegistered(game, ref) =>
            server ! ServerMessages.StartGame(game, testProbe.ref)
            server ! ServerMessages.GetGames(testProbe.ref)
            testProbe.expectMessage(ServerMessages.GamesList(Seq()))
          case _ =>
            fail("Expected GameRegistered message")
        }
      }
    }
    "send a GameList message without a game" when {
      "the game has been aborted previously" in {
        val game1 = GameInConstruction("codeGame1", GameParameters(maxTimeRound = 10), List.empty)
        server ! ServerMessages.RegisterGame(game1, testProbe.ref)
        val registerGame = testProbe.receiveMessage()
        registerGame match {
          case ServerMessages.GameRegistered(game, ref) =>
            server ! ServerMessages.AbortGame(game, testProbe.ref)
            server ! ServerMessages.GetGames(testProbe.ref)
            testProbe.expectMessage(ServerMessages.GamesList(Seq()))
          case _ =>
            fail("Expected GameRegistered message")
        }
      }
    }
  }
