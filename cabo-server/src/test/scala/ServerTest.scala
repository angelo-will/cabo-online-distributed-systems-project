import akka.actor.testkit.typed.scaladsl.{ActorTestKit, ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import model.GameInConstruction
import model.GameParameters
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.wordspec.AnyWordSpecLike
import utils.{AbortGame, GameRegistered, GamesList, GetGames, Message, RegisterGame, StartGame}

import scala.concurrent.duration.*
import org.scalatest.matchers.should.Matchers

class ServerTest extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with Matchers:

  import org.scalatest.matchers.must.Matchers.mustBe

  val serverCode = "TestServer"
  var server: ActorRef[Message] = _
  var testProbe: TestProbe[Message] = _

  override def beforeEach(): Unit =
    server = testKit.spawn(Server(serverCode))
    testProbe = testKit.createTestProbe[Message]()

  "Server" must {
    "send empty games list" when {
      "someone requests games but nobody has registered one of them" in {
        server ! GetGames(testProbe.ref)
        testProbe.expectMessage(GamesList(Seq()))
      }
    }
    "send a GameRegistered messagge" when {
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

        message mustBe GamesList(List(game1, game2))
      }
    }
    "send a GameList message without a game" when {
      "the game has been already started" in {
        val game1 = GameInConstruction("codeGame1", GameParameters(maxTimeRound = 10), List.empty)
        server ! RegisterGame(game1, testProbe.ref)
        val registerGame = testProbe.receiveMessage()
        registerGame match {
          case _ =>
            fail("Expected GameRegistered message")
          case GameRegistered(game, ref) =>
            server ! StartGame(game, testProbe.ref)
            server ! GetGames(testProbe.ref)
            testProbe.expectMessage(GamesList(Seq()))
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
            testProbe.expectMessage(GamesList(Seq(game1)))
          case _ =>
            fail("Expected GameRegistered message")
        }
      }
    }
  }
