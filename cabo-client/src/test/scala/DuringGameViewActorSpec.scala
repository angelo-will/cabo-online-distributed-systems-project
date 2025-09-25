import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import model.Game.GameInProgress
import model.TurnEvent.CardDiscarded
import model.{CardStack, DuringGameTurnLog, Game, GameParameters, GameStatus, Hand, IGameParameters, InvalidTurnEventException, PlayerPlaying, Power, TurnEvent, TurnLog, TurnPhase}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers.mustBe
import utils.{ClientMessages, DuringGameViewMessages, GameCoordinatorMessage, InitialViewMessages, Message}
import view.lobbyphase.ViewApplication
import view.lobbyphase.actors.InitialPhaseViewActor.ViewCreated
import view.lobbyphase.actors.{InitialPhaseViewActor, ViewActorListener}

import scala.swing.{BoxPanel, Label, MainFrame, Orientation, Swing}
import scala.swing.MenuBar.NoMenuBar.border

class DuringGameViewActorSpec extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with BeforeAndAfterEach
  with Matchers {

  import scala.concurrent.duration.{FiniteDuration, SECONDS}

  val userID = "Protagonista"

  private var probeAsClient: TestProbe[Message] = _
  private var probeAsMainMenu: TestProbe[Message] = _
  private var probeAsGameCoordinator: TestProbe[Message] = _
  private var game: GameInProgress = _


  override def beforeEach(): Unit =
    super.beforeEach()
    probeAsClient = testKit.createTestProbe[Message]()
    probeAsMainMenu = testKit.createTestProbe[Message]()
    probeAsGameCoordinator = testKit.createTestProbe[Message]()
    game = generateGameInProgress(userID, true)

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
        duringGameViewActor ! DuringGameViewMessages.StartGame(game, probeAsGameCoordinator.ref)
        Thread.sleep(5000) // wait for the view to update
      }
    }
    "send to GameCoordinator showYourCard request" when {
      "user click on own card during revealing at start own card phase" in {
        val duringGameViewActor = testKit.spawn(view.gamephase.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.receiveMessages(1)
        Thread.sleep(2000) // wait for the view to update
        duringGameViewActor ! DuringGameViewMessages.StartGame(game, probeAsGameCoordinator.ref)
        val card = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.ShowYourNthCard](FiniteDuration(5, SECONDS))
      }
    }
    "show own card after receiving CardSeen message from GameCoordinator" when {
      "user click on own card during revealing at start own card phase and receive CardSeen message from GameCoordinator" in {
        val duringGameViewActor = testKit.spawn(view.gamephase.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.receiveMessages(1)
        val player = game.players.filter(_.userID.equals(userID)).head
        duringGameViewActor ! DuringGameViewMessages.StartGame(game, probeAsGameCoordinator.ref)
        val showYourNCardRequest = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.ShowYourNthCard](FiniteDuration(5, SECONDS))
        val cardRequested = player.hand.cards(showYourNCardRequest.index)
        duringGameViewActor ! DuringGameViewMessages.CardSeen(cardRequested)

        //        duringGameViewActor ! GameCoordinatorMessage.CardSeen(cardRequest.index, game.players.head.hand.cards(cardRequest.index))
        Thread.sleep(10000) // wait for the view to update
      }
    }
    "notify end view card phase" when {
      "quantity of cards visible has been seen" in {
        val duringGameViewActor = testKit.spawn(view.gamephase.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.receiveMessages(1)
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! DuringGameViewMessages.StartPlayPhase()
        Thread.sleep(10000) // wait for the view to update
      }
    }
    "let the player start his turn" when {
      "receive StartPlayPhase message and then FirstTurn" in {
        val duringGameViewActor = testKit.spawn(view.gamephase.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.receiveMessages(1)
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! DuringGameViewMessages.StartPlayPhase()
        duringGameViewActor ! DuringGameViewMessages.FirstTurn()
        Thread.sleep(10000) // wait for the view to update
      }
      "receive LastTurnPlayed and myTurn is true" in {
        val game = generateGameInProgress(userID, false)
        val duringGameViewActor = testKit.spawn(view.gamephase.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        val playerWhoPlayTurnBefore = game.players.filter(_.userID != userID).head
        probeAsClient.receiveMessages(1)
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! DuringGameViewMessages.StartPlayPhase()
        val (newGameState, turn) = generateTurnWithDrawFromDeck(playerWhoPlayTurnBefore.userID, game)
        Thread.sleep(2000)
        duringGameViewActor ! DuringGameViewMessages.LastTurnPlayed(turn, newGameState, true)
        Thread.sleep(10000) // wait for the view to update
      }
    }
    "let the player draw a card at turn start" when {
      "choose to draw from deck" in {
        val duringGameViewActor = testKit.spawn(view.gamephase.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        val playerWhoPlayTurnBefore = game.players.filter(_.userID != userID).head
        probeAsClient.receiveMessages(1)
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! DuringGameViewMessages.StartPlayPhase()
        Thread.sleep(1000)
        duringGameViewActor ! DuringGameViewMessages.FirstTurn()
        val msg = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.DrawCardFromDeck](FiniteDuration(5, SECONDS))
        val (cardDrawn, newDeck) = game.deckStack.drawFirstCard
        duringGameViewActor ! DuringGameViewMessages.CardDrawn(cardDrawn)
        // Check the correct visualization of the card drawn from deck
        Thread.sleep(10000)
      }
      "choose to draw from discard stack" in {

      }
    }
  }

  private def revealingFirstTwoCardsPhase(duringGameViewActor: ActorRef[Message], game: GameInProgress): Unit = {
    val player = game.players.filter(_.userID.equals(userID)).head
    duringGameViewActor ! DuringGameViewMessages.StartGame(game, probeAsGameCoordinator.ref)
    val showYourFirstNthCard = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.ShowYourNthCard](FiniteDuration(5, SECONDS))
    val firstCardRequested = player.hand.cards(showYourFirstNthCard.index)
    duringGameViewActor ! DuringGameViewMessages.CardSeen(firstCardRequested)
    val showYourSecondNthCard = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.ShowYourNthCard](FiniteDuration(5, SECONDS))
    val secondCardRequested = player.hand.cards(showYourSecondNthCard.index)
    duringGameViewActor ! DuringGameViewMessages.CardSeen(secondCardRequested)
  }

  def generateGameInProgress(userID: String, shuffleDeck: Boolean): GameInProgress = {
    // 1. Creazione dei giocatori e delle loro mani
    val fullDeck = if shuffleDeck then CardStack.buildShuffledFullDeck else CardStack.buildSortedFullDeck
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

  private def generateTurnWithDrawFromDeck(userID: String, game: GameInProgress): (GameInProgress, TurnLog) = {
    val turnLog = new DuringGameTurnLog(userID)
    val (cardDrawn, newDeck) = game.deckStack.drawFirstCard
    turnLog.addEvent(TurnEvent.DrawCardFromDeck(cardDrawn))
    turnLog.currentPhase match {
      case TurnPhase.AwaitDiscardCard() =>
      case TurnPhase.AwaitUsePower() =>
        // Here you would implement the logic for using the power of the drawn card
        // For simplicity, let's assume the player sees their own card (if applicable) and then discards the drawn card
        cardDrawn.power match {
          case Power.SeeYourCard() =>
            turnLog.addEvent(TurnEvent.SeeSelfCard(0)) // Assuming the player sees their first card
          case Power.SeeYourOpponentCard() =>
            val adversaryID = game.players.filter(_.userID == this.userID).head.userID
            turnLog.addEvent(TurnEvent.SeeAdversaryCard(adversaryID, 0)) // Assuming the player sees the first card of an adversary
          case Power.ChangeOneOfYourCardWithOpponent() =>
            val adversaryID = game.players.filter(_.userID == this.userID).head.userID
            turnLog.addEvent(TurnEvent.ReplaceOwnCardWithAdversaryCard(0, adversaryID, 0)) // Assuming the player swaps their first card with the first card of an adversary
          case Power.NoPower() =>
            throw new Error("This case should not happen as NoPower is handled in AwaitDrawCard phase")
        }
      case _ =>
        throw new Error("Test not implemented for this case")
      //
      //        val player = game.players.filter(_.userID.equals(userID)).head
      //        val cardToDiscard = player.hand.cards.head
      //        turnLog.addEvent(TurnEvent.CardDiscarded(cardToDiscard))
      //        val newHand = Hand(player.hand.cards.tail :+ cardDrawn)
      //        val newPlayer = player.copy(hand = newHand)
      //        val newPlayers = game.players.map(p => if (p.userID == userID) newPlayer else p)
      //        val newDiscardDeck = game.discardDeckStack.addCardOnTop(cardToDiscard)
      //        game = game.copy(
      //          players = newPlayers,
      //          deckStack = newDeck,
      //          discardDeckStack = newDiscardDeck
      //        )
    }
    turnLog.addEvent(TurnEvent.CardDiscarded(cardDrawn))

    val newGameState = game.copy(
      deckStack = newDeck,
      discardDeckStack = game.discardDeckStack.addTopCard(cardDrawn),
      currentRound = game.currentRound + 1
    )
    (newGameState, turnLog)
  }
}
