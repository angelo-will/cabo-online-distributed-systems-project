import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import messages.{ClientMessages, GameCoordinatorMessage, GameViewMessages}
import model.Game.GameInProgress
import model.{Card, CardStack, DuringGameTurnLog, Game, GameParameters, GameStatus, Hand, IGameParameters, PlayerPlaying, Power, TurnEvent, TurnLog, TurnPhase}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.BeforeAndAfterEach
import utils.Message
import view.gamephase
import view.gamephase.actors
import view.gamephase.actors.DuringGameViewActor
import view.lobbyphase.actors.InitialPhaseViewActor

import scala.swing.*
import scala.swing.event.ButtonClicked

class DuringGameViewActorSpec extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with BeforeAndAfterEach
  with Matchers {

  import scala.concurrent.duration.{FiniteDuration, SECONDS}

  val userID = "Protagonista"
  val adversary01ID = "player01"
  val adversary02ID = "player02"
  val adversary03ID = "player03"

  private var probeAsClient: TestProbe[Message] = _
  private var probeAsMainMenu: TestProbe[Message] = _
  private var probeAsGameCoordinator: TestProbe[Message] = _
  private var probeCheck: TestProbe[Message] = _
  private var game: GameInProgress = _


  override def beforeEach(): Unit =
    super.beforeEach()
    probeAsClient = testKit.createTestProbe[Message]()
    probeAsMainMenu = testKit.createTestProbe[Message]()
    probeAsGameCoordinator = testKit.createTestProbe[Message]()
    probeCheck = testKit.createTestProbe[Message]()
    game = generateGameInProgress(userID, true)

  private val tab = "&nbsp;"

  private case class Passed() extends Message

  private case class Failed() extends Message

  "DuringGameViewActor" must {
    "start and send DuringGameViewReady message to clientRef" when {
      "spawned" in {
        val duringGameViewActor = testKit.spawn(actors.DuringGameViewActor("user1", probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.expectMessageType[ClientMessages.DuringGameViewReady](FiniteDuration(3, SECONDS))
      }
    }
    "change view from waiting to game started" when {
      "receive StartGame message" in {
        val duringGameViewActor = testKit.spawn(actors.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.expectMessageType[ClientMessages.DuringGameViewReady](FiniteDuration(3, SECONDS))
        Thread.sleep(2000) // wait for the view to update
        duringGameViewActor ! GameViewMessages.StartGame(game, probeAsGameCoordinator.ref)
        Thread.sleep(5000) // wait for the view to update
      }
    }
    "send to GameCoordinator showYourCard request" when {
      "user click on own card during revealing at start own card phase" in {
        val duringGameViewActor = testKit.spawn(actors.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.receiveMessages(1)
        Thread.sleep(2000) // wait for the view to update
        duringGameViewActor ! GameViewMessages.StartGame(game, probeAsGameCoordinator.ref)
        val card = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.ShowYourNthCard](FiniteDuration(5, SECONDS))
      }
    }
    "show own card after receiving CardSeen message from GameCoordinator" when {
      "user click on own card during revealing at start own card phase and receive CardSeen message from GameCoordinator" in {
        val duringGameViewActor = testKit.spawn(actors.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.receiveMessages(1)
        val player = game.players.filter(_.userID.equals(userID)).head
        duringGameViewActor ! GameViewMessages.StartGame(game, probeAsGameCoordinator.ref)
        val showYourNCardRequest = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.ShowYourNthCard](FiniteDuration(5, SECONDS))
        val cardRequested = player.hand.cards(showYourNCardRequest.index)
        duringGameViewActor ! GameViewMessages.CardSeen(cardRequested)

        //        duringGameViewActor ! GameCoordinatorMessage.CardSeen(cardRequest.index, game.players.head.hand.cards(cardRequest.index))
        Thread.sleep(10000) // wait for the view to update
      }
    }
    "notify end view card phase" when {
      "quantity of cards visible has been seen" in {
        val duringGameViewActor = testKit.spawn(actors.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.receiveMessages(1)
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! GameViewMessages.WaitAfterRevealingSection()
        Thread.sleep(10000) // wait for the view to update
      }
    }
    "let the player start his turn" when {
      "receive StartPlayPhase message and then FirstTurn" in {
        val duringGameViewActor = testKit.spawn(actors.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.receiveMessages(1)
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! GameViewMessages.WaitAfterRevealingSection()
        duringGameViewActor ! GameViewMessages.StartTurnPlayer(userID)
        Thread.sleep(10000) // wait for the view to update
      }
      "receive LastTurnPlayed and myTurn is true" in {
        game = generateGameInProgress(userID, false)
        val duringGameViewActor = startApp()
        val playerWhoPlayTurnBefore = game.players.filter(_.userID != userID).head
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! GameViewMessages.WaitAfterRevealingSection()
        val (newGameState, turn) = generateTurnWithDrawFromDeck(playerWhoPlayTurnBefore.userID, game)
        Thread.sleep(2000)
        duringGameViewActor ! GameViewMessages.LastTurnPlayed(turn, newGameState)
        duringGameViewActor ! GameViewMessages.StartTurnPlayer(userID)
        Thread.sleep(10000) // wait for the view to update
      }
    }
    "let the player draw a card at turn start" when {
      "choose to draw from deck" in {
        val duringGameViewActor = testKit.spawn(actors.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        val playerWhoPlayTurnBefore = game.players.filter(_.userID != userID).head
        probeAsClient.receiveMessages(1)
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! GameViewMessages.WaitAfterRevealingSection()
        Thread.sleep(1000)
        duringGameViewActor ! GameViewMessages.StartTurnPlayer(userID)
        val msg = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.DrawCardFromDeck](FiniteDuration(5, SECONDS))
        val (cardDrawn, newDeck) = game.deckStack.drawFirstCard
        duringGameViewActor ! GameViewMessages.CardDrawn(cardDrawn)
        // Check the correct visualization of the card drawn from deck
        Thread.sleep(10000)
      }
      "choose to draw from discard stack" in {
        val duringGameViewActor = testKit.spawn(actors.DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
        probeAsClient.receiveMessages(1)
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! GameViewMessages.WaitAfterRevealingSection()
        Thread.sleep(1000)
        duringGameViewActor ! GameViewMessages.StartTurnPlayer(userID)
        val msg = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.DrawCardFromDiscardStack](FiniteDuration(5, SECONDS))
        val (cardDrawn, newDiscardStack) = game.discardDeckStack.drawFirstCard
        duringGameViewActor ! GameViewMessages.CardDrawn(cardDrawn)
        if newDiscardStack.isEmpty then
          duringGameViewActor ! GameViewMessages.EmptyDiscardStack()
        else
          duringGameViewActor ! GameViewMessages.NewTopCardDiscardStack(newDiscardStack.cards.head)
        // Check the correct visualization of the card drawn from deck
        Thread.sleep(10000)
      }
    }
    "let the player discard card drawn" in {
      val duringGameViewActor = startApp()
      revealingFirstTwoCardsPhase(duringGameViewActor, game)
      duringGameViewActor ! GameViewMessages.WaitAfterRevealingSection()
      Thread.sleep(1000)
      duringGameViewActor ! GameViewMessages.StartTurnPlayer(userID)
      val (cardDrawn, newDeck) = drawFromDeckExpectation(duringGameViewActor.ref)
      probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.DiscardCardDrawn](FiniteDuration(5, SECONDS))
      Thread.sleep(10000)
    }
    "let update discard stack" when {
      "player discard card" in {
        val duringGameViewActor = startApp()
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! GameViewMessages.WaitAfterRevealingSection()
        Thread.sleep(1000)
        duringGameViewActor ! GameViewMessages.StartTurnPlayer(userID)
        val (cardDrawn, newDeck) = drawFromDeckExpectation(duringGameViewActor.ref)
        probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.DiscardCardDrawn](FiniteDuration(5, SECONDS))
        duringGameViewActor ! GameViewMessages.NewTopCardDiscardStack(cardDrawn)
        Thread.sleep(10000)
      }
    }
    "let the player change one of his card" when {
      "choose to keep drawn card" in {
        val duringGameViewActor = startApp()
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! GameViewMessages.WaitAfterRevealingSection()
        Thread.sleep(1000)
        duringGameViewActor ! GameViewMessages.StartTurnPlayer(userID)
        val (cardDrawn, newDeck) = drawFromDeckExpectation(duringGameViewActor.ref)
        val msg = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.DiscardYourNthCard](FiniteDuration(5, SECONDS))
        val oldCard = game.getPlayerWithID(userID).hand.cards(msg.index)
        game = game.replaceNthCardOfPlayerWithID(userID, cardDrawn, msg.index)
        duringGameViewActor ! GameViewMessages.NewTopCardDiscardStack(oldCard)
        Thread.sleep(10000)
      }
    }
    "let the player end turn" in {
      val duringGameViewActor = startApp()
      revealingFirstTwoCardsPhase(duringGameViewActor, game)
      duringGameViewActor ! GameViewMessages.WaitAfterRevealingSection()
      Thread.sleep(1000)
      duringGameViewActor ! GameViewMessages.StartTurnPlayer(userID)
      val (cardDrawn, newDeck) = drawFromDeckExpectation(duringGameViewActor.ref)
      probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.DiscardCardDrawn](FiniteDuration(5, SECONDS))
      duringGameViewActor ! GameViewMessages.NewTopCardDiscardStack(cardDrawn)
      probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.EndTurn](FiniteDuration(5, SECONDS))
      createCheckFrame().open()
    }
    "let the player use power" when {
      "draw power to see one of his cards" in {
        game = generateGameInProgress(userID, false)
        // With two players skipping first card reach Jack (in not shuffled deck)
        val (_, deck) = game.deckStack.drawFirstCard
        game = game.copy(deckStack = deck)
        val duringGameViewActor = startApp()
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! GameViewMessages.WaitAfterRevealingSection()
        Thread.sleep(1000)
        duringGameViewActor ! GameViewMessages.StartTurnPlayer(userID)
        val (cardDrawn, newDeck) = drawFromDeckExpectation(duringGameViewActor.ref)
        val cardIndex = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.ShowYourNthCard](FiniteDuration(5, SECONDS)).index
        duringGameViewActor ! GameViewMessages.CardSeen(game.getCardOfPlayerWithID(userID, cardIndex))
        probeAsGameCoordinator.receiveMessages(1, FiniteDuration(10, SECONDS)).head match
          case GameCoordinatorMessage.DiscardYourNthCard(index) =>
            val oldCard = game.getCardOfPlayerWithID(userID, index)
            game = game.replaceNthCardOfPlayerWithID(userID, cardDrawn, index)
            duringGameViewActor ! GameViewMessages.NewTopCardDiscardStack(oldCard)
          case GameCoordinatorMessage.DiscardCardDrawn() =>
            duringGameViewActor ! GameViewMessages.NewTopCardDiscardStack(cardDrawn)
          case msg =>
            fail(s"Expected DiscardYourNthCard or DiscardCardDrawn message, received instead $msg")
        probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.EndTurn](FiniteDuration(10, SECONDS))
        createCheckFrame().open()
      }
      "draw power to see one of adversary cards" in {
        game = generateGameInProgress(userID, false)
        val (_, deck) = game.deckStack.drawNCards(2)
        game = game.copy(deckStack = deck)
        val duringGameViewActor = startApp()
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! GameViewMessages.WaitAfterRevealingSection()
        Thread.sleep(1000)
        duringGameViewActor ! GameViewMessages.StartTurnPlayer(userID)
        val (cardDrawn, newDeck) = drawFromDeckExpectation(duringGameViewActor.ref)
        val msg = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.ShowAdversaryNthCard](FiniteDuration(5, SECONDS))
        val card = game.getCardOfPlayerWithID(msg.playerID, msg.cardIndex)
        duringGameViewActor ! GameViewMessages.CardSeen(card)
        createCheckFrame().open()
      }
      "draw power to change card with adversary one" in {
        game = generateGameInProgress(userID, false)
        val (_, deck) = game.deckStack.drawNCards(3)
        game = game.copy(deckStack = deck)
        val duringGameViewActor = startApp()
        revealingFirstTwoCardsPhase(duringGameViewActor, game)
        duringGameViewActor ! GameViewMessages.WaitAfterRevealingSection()
        Thread.sleep(1000)
        duringGameViewActor ! GameViewMessages.StartTurnPlayer(userID)
        val (cardDrawn, newDeck) = drawFromDeckExpectation(duringGameViewActor.ref)
        val msg = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.ReplaceOwnNthCardWithAdversaryNthOne](FiniteDuration(15, SECONDS))
        val playerOldCard = game.getCardOfPlayerWithID(userID, msg.ownCardIndex)
        val adversaryOldCard = game.getCardOfPlayerWithID(msg.adversaryID, msg.adversaryCardIndex)
        game = game.replaceNthCardOfPlayerWithID(userID, adversaryOldCard, msg.ownCardIndex)
        game = game.replaceNthCardOfPlayerWithID(msg.adversaryID, playerOldCard, msg.adversaryCardIndex)
        duringGameViewActor ! GameViewMessages.ChangeCardWithAdversaryAck()
        createCheckFrame().open()
      }
    }
  }

  private def startApp(): ActorRef[Message] = {
    val ref = testKit.spawn(DuringGameViewActor(userID, probeAsClient.ref, probeAsMainMenu.ref))
    probeAsClient.receiveMessages(1)
    ref ! GameViewMessages.StartGame(game, probeAsGameCoordinator.ref)
    ref
  }

  private def revealingFirstTwoCardsPhase(duringGameViewActor: ActorRef[Message], game: GameInProgress): Unit = {
    val player = game.players.filter(_.userID.equals(userID)).head
    //    duringGameViewActor ! DuringGameViewMessages.StartGame(game, probeAsGameCoordinator.ref)
    val showYourFirstNthCard = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.ShowYourNthCard](FiniteDuration(5, SECONDS))
    val firstCardRequested = player.hand.cards(showYourFirstNthCard.index)
    duringGameViewActor ! GameViewMessages.CardSeen(firstCardRequested)
    val showYourSecondNthCard = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.ShowYourNthCard](FiniteDuration(5, SECONDS))
    val secondCardRequested = player.hand.cards(showYourSecondNthCard.index)
    duringGameViewActor ! GameViewMessages.CardSeen(secondCardRequested)
  }

  private def drawFromDeckExpectation(viewActor: ActorRef[Message]): (Card, CardStack) = {
    val msg = probeAsGameCoordinator.expectMessageType[GameCoordinatorMessage.DrawCardFromDeck](FiniteDuration(5, SECONDS))
    println(s"DuringGameViewActorSpec: received message $msg TO GameCoordinator")
    val (cardDrawn, newDeck) = game.deckStack.drawFirstCard
    viewActor ! GameViewMessages.CardDrawn(cardDrawn)
    (cardDrawn, newDeck)
  }

  private def generateGameInProgress(userID: String, shuffleDeck: Boolean): GameInProgress = {
    val fullDeck = if shuffleDeck then CardStack.buildShuffledFullDeck else CardStack.buildSortedFullDeck
    val (hand1Cards, deckAfterHand1) = fullDeck.drawNCards(4)
    val (hand2Cards, deckAfterHand2) = deckAfterHand1.drawNCards(4)
    //    val (hand3Cards, deckAfterHand3) = deckAfterHand2.drawNCards(4)
    //    val (hand4Cards, deckAfterHand4) = deckAfterHand3.drawNCards(4)

    val (firstCardDiscardStack, finalDeck) = deckAfterHand2.drawFirstCard

    val player1 = PlayerPlaying(userID, userID, 1, Hand(hand1Cards))
    val adversary01 = PlayerPlaying(adversary01ID, adversary01ID, 2, Hand(hand2Cards))
    //    val player3 = PlayerPlaying("user3", "Charlie", Hand(hand3Cards))
    //    val player4 = PlayerPlaying("user4", "Diana", Hand(hand4Cards))


    val playersList = List(
      player1,
      adversary01,
      //      player3,
      //      player4
    )

    val discardDeck = CardStack.buildEmptyDeck.addTopCard(firstCardDiscardStack)


    val gameParameters: IGameParameters = GameParameters()
    val gameStatus = GameStatus.InProgress()
    val currentRound = 1

    GameInProgress(
      code = "game-1234",
      gameParameters = gameParameters,
      gameStatus = gameStatus,
      players = playersList,
      deckStack = finalDeck,
      discardDeckStack = discardDeck,
      currentRound = currentRound
    )
  }

  private def generateTurnWithDrawFromDeck(userID: String, game: GameInProgress): (GameInProgress, TurnLog) = {
    val turnLog = new DuringGameTurnLog(userID, game.currentRound + 1)
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
    turnLog.addEvent(TurnEvent.CardDrawnDiscarded(cardDrawn))

    val newGameState = game.copy(
      deckStack = newDeck,
      discardDeckStack = game.discardDeckStack.addTopCard(cardDrawn),
      currentRound = game.currentRound + 1
    )
    (newGameState, turnLog)
  }


  def createCheckFrame(): Frame = new Frame {
    title = "Test"
    preferredSize = new java.awt.Dimension(500, 400)
    peer.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    val panel: BoxPanel = new BoxPanel(Orientation.Vertical) {
      border = Swing.EmptyBorder(30, 30, 30, 30)
      val yesButton = new scala.swing.Button("Yes")
      val noButton = new scala.swing.Button("No")
      listenTo(yesButton, noButton)
      reactions += {
        case scala.swing.event.ButtonClicked(`yesButton`) =>
          probeCheck.ref ! Passed()
          dispose()
        case scala.swing.event.ButtonClicked(`noButton`) =>
          probeCheck.ref ! Failed()
          dispose()
      }
      contents += new Label("Click yes if view has showed the correct behavior.")
      contents += yesButton
      contents += noButton
    }
    contents = panel
    visible = true
    probeCheck.expectMessage(FiniteDuration(30, SECONDS), Passed())
  }

}

