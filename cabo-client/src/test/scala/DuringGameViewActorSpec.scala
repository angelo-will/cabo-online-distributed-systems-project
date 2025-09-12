import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import model.Game.GameInProgress
import model.{CardStack, Game, GameParameters, GameStatus, Hand, IGameParameters, PlayerPlaying}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers.mustBe
import utils.{ClientMessages, DuringGameViewMessages, InitialViewMessages, GameCoordinatorMessage, Message}
import view.lobbyphase.ViewApplication
import view.lobbyphase.actors.InitialPhaseViewActor.ViewCreated
import view.lobbyphase.actors.{InitialPhaseViewActor, ViewActorListener}

import scala.swing.{BoxPanel, Label, MainFrame, Orientation, Swing}
import scala.swing.MenuBar.NoMenuBar.border

class DuringGameViewActorSpec extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with BeforeAndAfterEach
  with Matchers:

  import scala.concurrent.duration.{FiniteDuration, SECONDS}

  private var probeAsClient: TestProbe[Message] = _
  private var probeAsMainMenu: TestProbe[Message] = _
  private var probeAsGameCoordinator: TestProbe[Message] = _

  val userID = "Protagonista"

  override def beforeEach(): Unit =
    super.beforeEach()
    probeAsClient = testKit.createTestProbe[Message]()
    probeAsMainMenu = testKit.createTestProbe[Message]()
    probeAsGameCoordinator = testKit.createTestProbe[Message]()

  private val tab = "&nbsp;"

  "DuringGameViewActor" must {
    "start and send DuringGameViewReady message to clientRef" when {
      "spawned" in {
        val duringGameViewActor = testKit.spawn(view.gamephase.DuringGameViewActor("user1", probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.expectMessageType[ClientMessages.DuringGameViewReady](FiniteDuration(3, SECONDS))
      }
    }
    "change view from waiting to game started" when {
      "receive StartGame message" in {
        val duringGameViewActor = testKit.spawn(view.gamephase.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.expectMessageType[ClientMessages.DuringGameViewReady](FiniteDuration(3, SECONDS))
        Thread.sleep(2000) // wait for the view to update
        val game = generateGameInProgress(userID)
        duringGameViewActor ! DuringGameViewMessages.StartGame(game, probeAsGameCoordinator.ref)
        Thread.sleep(5000) // wait for the view to update
      }
    }
    "send to GameCoordinator showYourCard request" when {
      "user click on own card during revealing at start own card phase" in {
        val duringGameViewActor = testKit.spawn(view.gamephase.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.receiveMessages(1)
        Thread.sleep(2000) // wait for the view to update
        val game = generateGameInProgress(userID)
        duringGameViewActor ! DuringGameViewMessages.StartGame(game, probeAsGameCoordinator.ref)
        val card = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.ShowYourNthCard](FiniteDuration(5, SECONDS))
      }
    }
    "show own card after receiving CardSeen message from GameCoordinator" when {
      "user click on own card during revealing at start own card phase and receive CardSeen message from GameCoordinator" in {
        val duringGameViewActor = testKit.spawn(view.gamephase.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.receiveMessages(1)
        val game = generateGameInProgress(userID)
        val player = game.players.filter(_.userID.equals(userID)).head
        duringGameViewActor ! DuringGameViewMessages.StartGame(game, probeAsGameCoordinator.ref)
        val showYourNCardRequest = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.ShowYourNthCard](FiniteDuration(5, SECONDS))
        val cardRequested = player.hand.cards(showYourNCardRequest.index)
        duringGameViewActor ! DuringGameViewMessages.CardSeen(cardRequested)

//        duringGameViewActor ! GameCoordinatorMessage.CardSeen(cardRequest.index, game.players.head.hand.cards(cardRequest.index))
        Thread.sleep(10000) // wait for the view to update
      }
    }

  }

  def generateGameInProgress(userID: String): GameInProgress = {
    // 1. Creazione dei giocatori e delle loro mani
    val fullDeck = CardStack.buildShuffledFullDeck
    val (hand1Cards, deckAfterHand1) = fullDeck.drawNCards(4)
    val (hand2Cards, deckAfterHand2) = deckAfterHand1.drawNCards(4)
    //    val (hand3Cards, deckAfterHand3) = deckAfterHand2.drawNCards(4)
    //    val (hand4Cards, deckAfterHand4) = deckAfterHand3.drawNCards(4)

    val player1 = PlayerPlaying(userID, "Alice", 1, Hand(hand1Cards))
    val player2 = PlayerPlaying("user2", "Bob", 2, Hand(hand2Cards))
    //    val player3 = PlayerPlaying("user3", "Charlie", Hand(hand3Cards))
    //    val player4 = PlayerPlaying("user4", "Diana", Hand(hand4Cards))


    val playersList = List(
      player1,
      player2,
      //      player3,
      //      player4
    )

    // 2. Creazione del mazzo e del mazzo degli scarti
    val gameDeck = deckAfterHand2
    //    val gameDeck = deckAfterHand4
    val discardDeck = CardStack.buildEmptyDeck

    // 3. Creazione dei parametri del gioco e dello stato
    val gameParameters: IGameParameters = GameParameters()
    val gameStatus = GameStatus.InProgress()
    val currentRound = 1

    GameInProgress(
      code = "game-1234",
      gameParameters = gameParameters,
      gameStatus = gameStatus,
      players = playersList,
      deckStack = gameDeck,
      discardDeckStack = discardDeck,
      currentRound = currentRound
    )
  }
