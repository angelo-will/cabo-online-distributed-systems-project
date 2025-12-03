import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import messages.{ClientMessages, PreGameViewMessages}
import model.{Game, GameParameters, PlayerInLobby}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers.mustBe
import utils.Message
import view.lobbyphase.ViewApplication
import view.lobbyphase.actors.InitialPhaseViewActor.ViewCreated
import view.lobbyphase.actors.{InitialPhaseViewActor, ViewActorListener}

import scala.swing.{BoxPanel, Label, MainFrame, Orientation, Swing}
import scala.swing.MenuBar.NoMenuBar.border


class InitialPhaseViewActorSpec extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with BeforeAndAfterEach
  with Matchers:

  import scala.concurrent.duration.{FiniteDuration, SECONDS}

  private var probe: TestProbe[Message] = _

  override def beforeEach(): Unit =
    super.beforeEach()
    probe = testKit.createTestProbe[Message]()

  private val tab = "&nbsp;"

  private case class Passed() extends Message

  private case class Failed() extends Message

  "Initial Phase View Actor" must {
    "create view" when {
      "it's spawned" in {
        val actorView = testKit.spawn(InitialPhaseViewActor(probe.ref, "player01"))
        actorView ! PreGameViewMessages.WhoToSendResponse(probe.ref)
        createCheckFrame("Initial Phase View Actor must create the view when it's spawned.", probe.ref).open()
        probe.expectMessageType[ViewCreated]
        probe.expectMessage(FiniteDuration(20, SECONDS), Passed())
      }
    }
    "close the waiting creation game view" when {
      "receive the message the game is created" in {
        val actorView = testKit.spawn(InitialPhaseViewActor(probe.ref, "player01"))
        actorView ! PreGameViewMessages.WhoToSendResponse(probe.ref)
        createCheckFrame("<html>" +
          "Initial Phase View Actor<br>" +
          "- must close the waiting creation game view<br>" +
          s"    when the game is created (public or private)." +
          "</html>", probe.ref).open()
        probe.expectMessageType[ViewCreated]
        val newGame = probe.expectMessageType[ClientMessages.CreateNewGame](FiniteDuration(20, SECONDS))
        Thread.sleep(3000)
        actorView ! PreGameViewMessages.GameCreated(Game.GameInConstruction(
          code = "testGame",
          GameParameters(!newGame.makePublic, newGame.maxTimeRound, newGame.maxNumRound, newGame.maxPlayers),
          players = List(PlayerInLobby("user1", "User One", probe.ref))
        ))
        probe.expectMessage(FiniteDuration(40, SECONDS), Passed())
      }
    }
    "close the waiting join game view" when {
      "receive the message the game is joined" in {
        val actorView = testKit.spawn(InitialPhaseViewActor(probe.ref, "player01"))
        actorView ! PreGameViewMessages.WhoToSendResponse(probe.ref)
        createCheckFrame("<html>" +
          "Initial Phase View Actor<br>" +
          "- must close the waiting join game view<br>" +
          s"    when the game is joined." +
          "</html>", probe.ref).open()
        probe.expectMessageType[ViewCreated]
        probe.expectMessageType[ClientMessages.JoinAGame](FiniteDuration(5, SECONDS))
        // Simulate the game list being sent after some delay
        Thread.sleep(3000)
        actorView ! PreGameViewMessages.GameList(List(
          Game.GameInConstruction(
            code = "testGame001",
            GameParameters(maxTimeRound = 60, roundLimitation = 10, maxPlayers = 4),
            players = List(PlayerInLobby("user1", "User One", probe.ref))
          ),
          Game.GameInConstruction(
            code = "anotherGame002",
            GameParameters(maxTimeRound = 30, roundLimitation = 5, maxPlayers = 2),
            players = List(PlayerInLobby("user2", "User Two", probe.ref))
          ),
          Game.GameInConstruction(
            code = "publicGame003",
            GameParameters(maxTimeRound = 45, roundLimitation = 7, maxPlayers = 3),
            players = List(PlayerInLobby("user3", "User Three", probe.ref))
          )
        ))
        val gameToJoin = probe.expectMessageType[ClientMessages.JoinGame](FiniteDuration(20, SECONDS))
        // Simulate the game being joined after some delay
        Thread.sleep(3000)
        actorView ! PreGameViewMessages.GameJoined(gameToJoin.game)
        probe.expectMessage(FiniteDuration(30, SECONDS), Passed())
      }
    }
//    "show to host request to partecipate dialog" when {
//      "another player ask to join the game" in {
//        val actorView = testKit.spawn(InitialPhaseViewActor(probe.ref, "player01"))
//        createCheckFrame("<html>" +
//          "Initial Phase View Actor<br>" +
//          "- make the host waiting view startable<br>" +
//          s"    there is at least another player waiting to start." +
//          "</html>", probe.ref).open()
//        probe.expectMessageType[ViewCreated]
//        val newGame = probe.expectMessageType[ClientMessages.CreateNewGame](FiniteDuration(20, SECONDS))
//        val gameInConstruction = Game.GameInConstruction(
//          code = "testGame",
//          GameParameters(!newGame.isPublic, newGame.maxTimeRound, newGame.maxNumRound, newGame.maxPlayers),
//          players = List(PlayerInLobby("user1", "User One", probe.ref))
//        )
//
//        Thread.sleep(3000)
//
//        // send the game created message
//        actorView ! InitialViewMessages.GameCreated(gameInConstruction)
//
//        Thread.sleep(3000)
//
//        // send a request to join the game
//        val playerWhoWantToJoin = PlayerInLobby("user2", "User Two", probe.ref)
//        actorView ! InitialViewMessages.PlayerRequestedToJoinGame(playerWhoWantToJoin)
//
//        val playerWhoJoined = probe.expectMessageType[InitialViewMessages.PlayerCanJoinGame](FiniteDuration(10, SECONDS)).player
//
//        playerWhoJoined mustBe playerWhoWantToJoin
//
//        // Check the adding to list of players
//
//        probe.expectMessage(FiniteDuration(40, SECONDS), Passed())
//      }
//    }
    "show the waiting lobby view to player who requested to enter" when {
      "the host accepted the player" in {
        val actorViewPlayerJoiner = testKit.spawn(InitialPhaseViewActor(probe.ref, "player01"))
        actorViewPlayerJoiner ! PreGameViewMessages.WhoToSendResponse(probe.ref)
        val hostRef = probe.ref
        createCheckFrame("<html>" +
          "Initial Phase View Actor<br>" +
          "- must show the waiting lobby view to player who requested to enter<br>" +
          s"    when the host accepted the player." +
          "</html>", probe.ref).open()
        probe.expectMessageType[ViewCreated]

        val playerJoiner = PlayerInLobby("playerJoiner", "Player Joiner", probe.ref)
        val playerHost = PlayerInLobby("host", "Host Player", hostRef)

        // Player insert the link in gui
        val address = probe.expectMessageType[ClientMessages.JoinAddress](FiniteDuration(20, SECONDS))

        Thread.sleep(3000)

        // host accepted and integrate the player in the game in construction
        val gameInConstruction = Game.GameInConstruction(
          code = "testGame",
          GameParameters(true, 60, 30, 4),
          players = List(playerHost, playerJoiner)
        )
        actorViewPlayerJoiner ! PreGameViewMessages.GameJoined(gameInConstruction)

        probe.expectMessage(FiniteDuration(40, SECONDS), Passed())
      }
    }
    "allow a normal game simulation" in {
      val actorViewHost = testKit.spawn(InitialPhaseViewActor(probe.ref, "player01"))
      actorViewHost ! PreGameViewMessages.WhoToSendResponse(probe.ref)
      Thread.sleep(3000)
      val actorViewJoiner = testKit.spawn(InitialPhaseViewActor(probe.ref, "player02"))
      actorViewJoiner ! PreGameViewMessages.WhoToSendResponse(probe.ref)

      probe.expectMessageType[ViewCreated]
      probe.expectMessageType[ViewCreated]

      // host build the game

      val newGame = probe.expectMessageType[ClientMessages.CreateNewGame](FiniteDuration(20, SECONDS))
      val gameBuilt = Game.GameInConstruction(
        code = "testGame",
        GameParameters(newGame.makePublic, newGame.maxTimeRound, newGame.maxNumRound, newGame.maxPlayers),
        players = List(PlayerInLobby("host", "Host Player", probe.ref))
      )
      actorViewHost ! PreGameViewMessages.GameCreated(gameBuilt)

      // joiner request to join the game
      val _ = probe.expectMessageType[ClientMessages.JoinAGame](FiniteDuration(20,SECONDS))
      Thread.sleep(3000)
      actorViewJoiner ! PreGameViewMessages.GameList(List(gameBuilt))

      val gameToJoin = probe.expectMessageType[ClientMessages.JoinGame](FiniteDuration(20, SECONDS))

      Thread.sleep(2000)

      // the host see changing of the players in list
      val gameAfterJoin = gameBuilt.copy(
        players = gameBuilt.players :+ PlayerInLobby("joiner", "Joiner Player", probe.ref))
      actorViewHost ! PreGameViewMessages.GameInfoUpdate(gameAfterJoin)
      actorViewJoiner ! PreGameViewMessages.GameJoined(gameAfterJoin)
      Thread.sleep(20000)
    }
  }

  "open error to publish on server dialog" when {
    "server send error" in {
      val actorView = testKit.spawn(InitialPhaseViewActor(probe.ref, "player01"))
      actorView ! PreGameViewMessages.WhoToSendResponse(probe.ref)
      createCheckFrame("<html>" +
        "Initial Phase View Actor<br>" +
        "- must open error to publish on server dialog<br>" +
        s"    when the server send error." +
        "</html>", probe.ref).open()
      probe.expectMessageType[ViewCreated]
      val newGame = probe.expectMessageType[ClientMessages.CreateNewGame](FiniteDuration(20, SECONDS))
      // Simulate the game creation
      Thread.sleep(3000)
      actorView ! PreGameViewMessages.GameCreated(Game.GameInConstruction(
        code = "testGame",
        GameParameters(newGame.makePublic, newGame.maxTimeRound, newGame.maxNumRound, newGame.maxPlayers),
        players = List(PlayerInLobby("user1", "User One", probe.ref))
      ))
      Thread.sleep(2000)
      // Simulate the error from server
      actorView ! PreGameViewMessages.FailedToPublishToServer()
      probe.expectMessage(FiniteDuration(20, SECONDS), Passed())
    }
  }

  def createCheckFrame(test: String, ref: ActorRef[Message]): MainFrame = new MainFrame {
    title = "Test Frame"
    preferredSize = new java.awt.Dimension(500, 400)
    peer.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    val panel = new BoxPanel(Orientation.Vertical) {
      border = Swing.EmptyBorder(30, 30, 30, 30)
      val yesButton = new scala.swing.Button("Yes")
      val noButton = new scala.swing.Button("No")
      listenTo(yesButton, noButton)
      reactions += {
        case scala.swing.event.ButtonClicked(`yesButton`) =>
          ref ! Passed()
          dispose()
        case scala.swing.event.ButtonClicked(`noButton`) =>
          ref ! Failed()
          dispose()
      }
      contents += new Label("Click yes if view has showed the correct behavior.")
      contents += new Label(test)
      contents += yesButton
      contents += noButton
    }
    contents = panel
  }
