import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import controller.GameCoordinatorActor
import messages.ClientMessages.ClientCommand
import messages.{Message, GameCoordinatorMessage as GCMessage}
import messages.GameCoordinatorMessage.GameCoordinatorMessage
import messages.{ClientMessages, GameViewMessages, IViewMessage}
import model.Game.{GameInConstruction, GameInProgress}
import model.Suit.*
import model.{Card, CardStack, PlayCycleTurnLog, Game, GameParameters, PlayerInLobby, Power, TurnEvent}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import scala.annotation.tailrec

class GameCoordinatorActorSpec extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with Matchers:

  import org.scalatest.matchers.must.Matchers.mustBe

  private val userID = "GoodPlayer01"
  private val opponent01 = "Adversary01"
  private val opponent02 = "Adversary02"
  private var gameCoordinatorActor: ActorRef[GameCoordinatorMessage] = _
  private var viewProbe: TestProbe[IViewMessage] = _
  private var clientProbe: TestProbe[ClientCommand] = _
  private var statusProbe: TestProbe[Message] = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    viewProbe = createTestProbe[IViewMessage]()
    clientProbe = createTestProbe[ClientCommand]()
    statusProbe = createTestProbe[Message]()
  }

  "Single Actor Player" must {
    "send state" when {
      "game data are created" in {
        startActorAndGenerateGameData()
        clientProbe.expectMessageType[ClientMessages.TakeGetInProgressGame]
      }
    }
    "send data to view" when {
      "game is started" in {
        skipGeneration()
        gameCoordinatorActor ! GCMessage.StartGame()
        viewProbe.expectMessageType[GameViewMessages.StartGame]
      }
    }
    "send cards value" when {
      "in revealing section" in {
        val game = jumpToRevealingSection()
        val cardIndex00 = 0
        val cardIndex01 = 1
        val cardSeen00 = showYourNthCard(cardIndex00)
        val cardSeen01 = showYourNthCard(cardIndex01)
        cardSeen00 mustBe game.getPlayerWithID(userID).hand.cards(cardIndex00)
        cardSeen01 mustBe game.getPlayerWithID(userID).hand.cards(cardIndex01)
      }
    }
    "send wait message to view" when {
      "revealing section is ended" in {
        jumpToRevealingSection()
        for i <- 0 until Game.cardsInitialVisible do
          showYourNthCard(i)
        viewProbe.expectMessageType[GameViewMessages.WaitAfterRevealingSection]
      }
    }
    "send who start game" when {
      "after notify to start playcycle" in {
        jumpToRevealingSection()
        for i <- 0 until Game.cardsInitialVisible do
          showYourNthCard(i)
        viewProbe.expectMessageType[GameViewMessages.WaitAfterRevealingSection]
        gameCoordinatorActor ! GCMessage.StartPlayCycle()
        viewProbe.expectMessageType[GameViewMessages.StartTurnPlayer]
      }
    }
    "allow to draw from deck" in {
      val game = jumpToFirstTurnAndGetGame()
      val card = drawAngGetCardFromDeck()
      game.deckStack.drawFirstCard._1 mustBe card
    }
    "allow to draw from discard" in {
      val game = jumpToFirstTurnAndGetGame()
      val card = drawCardFromDiscardStack()
      game.discardDeckStack.drawFirstCard._1 mustBe card
    }
    "send discard top card to view" when {
      "empty for drawing from discard in first turn" in {
        val game = jumpToFirstTurnAndGetGame()
        val card = drawCardFromDiscardStack()
        viewProbe.expectMessageType[GameViewMessages.EmptyDiscardStack]
      }
      "there is at least a card remaining in discard stack" in {

      }
      "discard card drawn" in {
        val game = jumpToFirstTurnAndGetGame()
        val card = drawAngGetCardFromDeck()
        discardCardDrawn()
        val cardDiscarded = viewProbe.expectMessageType[GameViewMessages.NewTopCardDiscardStack].card
        cardDiscarded mustBe card
      }
      "discard one of own cards" in {
        val game = jumpToFirstTurnAndGetGame()
        val cardDrawn = drawAngGetCardFromDeck()
        val indexCardToDiscard = 2
        val handCards = game.getPlayerWithID(userID).hand.cards
        val cardToDiscard = handCards(indexCardToDiscard)
        discardNthCard(indexCardToDiscard)
        val cardDiscarded = viewProbe.expectMessageType[GameViewMessages.NewTopCardDiscardStack].card
        cardDiscarded mustBe cardToDiscard
      }
    }
    "modify hand's card" when {
      "discard one of own" in {
        val game = jumpToFirstTurnAndGetGame()
        val card = drawAngGetCardFromDeck()
        val oldPlayerHand = game.getHandOfPlayerWithID(userID)
        val indexCardToChange = 1
        val newPlayerHand = oldPlayerHand.changeNthCard(indexCardToChange, card)
        discardNthCard(indexCardToChange)
        skipViewProbeMessage()
        val newGame = reqGameInformation()
        newGame.getHandOfPlayerWithID(userID) mustBe newPlayerHand
      }
    }

    "send to view the round is ended by time" when {
      "receive turn time ended" in {
        jumpToFirstTurnAndGetGame()
        endTurnByTime()
        viewProbe.expectMessageType[GameViewMessages.EndTurnByTimeEnded]
      }
    }
    "send to client turn ended message" when {
      "receive turn time ended" in {
        jumpToFirstTurnAndGetGame()
        endTurnByTime()
        skipViewProbeMessage()
        clientProbe.expectMessageType[ClientMessages.TurnEnded]
      }
      "receive end turn command" in {
        jumpToFirstTurnAndGetGame()
        drawAngGetCardFromDeck()
        discardCardDrawn()
        endTurn()
        clientProbe.expectMessageType[ClientMessages.TurnEnded]
      }
      "receive call cabo command" in {
        jumpToFirstTurnAndGetGame()
        drawAngGetCardFromDeck()
        discardCardDrawn()
        callCabo()
        clientProbe.expectMessageType[ClientMessages.TurnEnded]
      }
    }
    "send to view last turn played" when {
      "receive new turn" in {
        val game = jumpToFirstTurnAndGetGame()
        endTurnByTime()
        skipViewProbeMessage()
        skipClientProbeMessage()
        gameCoordinatorActor ! GCMessage.NewTurn(game, new PlayCycleTurnLog("", 0))
        viewProbe.expectMessageType[GameViewMessages.LastTurnPlayed]
      }
    }
    "send turn updated ack" when {
      "receive new turn" in {
        // new turn is sent from coordinator to itself after turn ended
        val game = jumpToFirstTurnAndGetGame()
        endTurnByTime()
        skipViewProbeMessage()
        skipClientProbeMessage()
        gameCoordinatorActor ! GCMessage.NewTurn(game, new PlayCycleTurnLog("", 0))
        clientProbe.expectMessageType[ClientMessages.TurnUpdated]
      }
    }
    "send and empty turn" when {
      "requested" in {
        jumpToFirstTurnAndGetGame()
        endTurnByTime()
        skipViewProbeMessage()
        skipClientProbeMessage()
        val game = reqGameInformation()
        gameCoordinatorActor ! GCMessage.GetEmptyTurn(userID)
        val turnEnded = clientProbe.expectMessageType[ClientMessages.TurnEnded]
        turnEnded.turnLog.events.head mustBe TurnEvent.JumpTurnForDisconnection()
      }
    }
    "send own card value" when {
      "drawn card with power to show one of own card" in {
        val game00 = jumpToFirstTurnAndGetGame()
        endMyTurnForMaxTimeReached()
        val card = CardStack.buildSortedFullDeck.cards.find(_.power == Power.SeeYourCard()).get
        val game01 = game00.copy(deckStack = CardStack(List(card)))
        jumpRoundsUntilMyTurnAgain(game01)
        val cardDrawn = drawAngGetCardFromDeck()
        val cardIndex = 0
        val cardToSee = game01.getPlayerWithID(userID).hand.cards(cardIndex)
        val cardSeen = showYourNthCard(cardIndex)
        cardSeen mustBe cardToSee
      }
    }
    "send opponent card value" when {
      "draw card with power to see one of opponent card" in {
        val game00 = jumpToFirstTurnAndGetGame()
        endMyTurnForMaxTimeReached()
        val card = CardStack.buildSortedFullDeck.cards.find(_.power == Power.SeeYourOpponentCard()).get
        val game01 = game00.copy(deckStack = CardStack(List(card)))
        jumpRoundsUntilMyTurnAgain(game01)
        val cardDrawn = drawAngGetCardFromDeck()
        val adversaryID = game01.players.find(_.userID != userID).get.userID
        val cardIndex = 0
        val cardToSee = game01.getPlayerWithID(adversaryID).hand.cards(cardIndex)
        val cardSeen = showAdversaryNthCard(adversaryID, cardIndex)
        cardSeen mustBe cardToSee
      }
    }
    "change one of own card with opponent one" when {
      "draw card with power to change one of own card with opponent one" in {
        val game00 = jumpToFirstTurnAndGetGame()
        endMyTurnForMaxTimeReached()
        val card = CardStack.buildSortedFullDeck.cards.find(_.power == Power.ChangeOneOfYourCardWithOpponent()).get
        val game01 = game00.copy(deckStack = CardStack(List(card)))
        jumpRoundsUntilMyTurnAgain(game01)
        val cardDrawn = drawAngGetCardFromDeck()

        val adversaryID = game01.players.find(_.userID != userID).get.userID
        val indexOfCardToChange = 0
        val ownCard = game01.getCardOfPlayerWithID(userID, indexOfCardToChange)
        val adversaryCard = game01.getCardOfPlayerWithID(adversaryID, indexOfCardToChange)
        val afterChanged = game01.replaceNthCardOfPlayerWithID(userID, adversaryCard, indexOfCardToChange)
          .replaceNthCardOfPlayerWithID(adversaryID, ownCard, indexOfCardToChange)

        replaceOwnNthCardWithAdversaryNthOne(indexOfCardToChange, adversaryID, indexOfCardToChange)
        viewProbe.expectMessageType[GameViewMessages.ChangeCardWithAdversaryAck]

        val gameReceived = reqGameInformation()

        gameReceived.getHandOfPlayerWithID(userID).cards(indexOfCardToChange) mustBe
          afterChanged.getHandOfPlayerWithID(userID).cards(indexOfCardToChange)

        gameReceived.getHandOfPlayerWithID(adversaryID).cards(indexOfCardToChange) mustBe
          afterChanged.getHandOfPlayerWithID(adversaryID).cards(indexOfCardToChange)
      }
    }
    "send game when cabo specified" when {
      "someone call cabo" in {
        val game = jumpToFirstTurnAndGetGame()
        val _ = drawAngGetCardFromDeck()
        discardCardDrawn()
        callCabo()
        val endedTurn = clientProbe.expectMessageType[ClientMessages.TurnEnded]
        endedTurn.game.caboState.get mustBe game.getPlayerWithID(userID)
      }
    }
    "send game ended by cabo" when {
      "is player-caller turn again" in {
        val _ = jumpToFirstTurnAndGetGame()
        val _ = drawAngGetCardFromDeck()
        discardCardDrawn()
        viewProbe.expectMessageType[GameViewMessages.NewTopCardDiscardStack]
        callCabo()
        val ended = clientProbe.expectMessageType[ClientMessages.TurnEnded]
        viewProbe.expectMessageType[GameViewMessages.LastTurnPlayed]
        viewProbe.expectMessageType[GameViewMessages.StartTurnPlayer]
        val game = ended.game.copy(currentRound = ended.game.currentRound + 1)
        gameCoordinatorActor ! GCMessage.NewTurn(game, new PlayCycleTurnLog("", 0))
        clientProbe.expectMessageType[ClientMessages.TurnUpdated]
        viewProbe.expectMessageType[GameViewMessages.LastTurnPlayed]
        viewProbe.expectMessageType[GameViewMessages.GameEndedByCabo]
      }
    }
    "send game ended by empty deck" when {
      "start a turn with empty deck" in {
        val _ = jumpToFirstTurnAndGetGame()
        val _ = drawAngGetCardFromDeck()
        discardCardDrawn()
        viewProbe.expectMessageType[GameViewMessages.NewTopCardDiscardStack]
        endTurn()
        val ended = clientProbe.expectMessageType[ClientMessages.TurnEnded]
        viewProbe.expectMessageType[GameViewMessages.LastTurnPlayed]
        viewProbe.expectMessageType[GameViewMessages.StartTurnPlayer]
        val newRound = ended.game.currentRound + 1
        val emptyDeck = CardStack.buildEmptyDeck
        val game = ended.game.copy(deckStack = emptyDeck, currentRound = ended.game.currentRound + 1)
        gameCoordinatorActor ! GCMessage.NewTurn(game, new PlayCycleTurnLog("", 0))
        clientProbe.expectMessageType[ClientMessages.TurnUpdated]
        viewProbe.expectMessageType[GameViewMessages.LastTurnPlayed]
        viewProbe.expectMessageType[GameViewMessages.GameEndedByEmptyDeck]
      }
    }
    "send game ended by turn limit deck" when {
      "start a turn with max turn reached" in {
        val _ = jumpToFirstTurnAndGetGame()
        val _ = drawAngGetCardFromDeck()
        discardCardDrawn()
        viewProbe.expectMessageType[GameViewMessages.NewTopCardDiscardStack]
        endTurn()
        val ended = clientProbe.expectMessageType[ClientMessages.TurnEnded]
        viewProbe.expectMessageType[GameViewMessages.LastTurnPlayed]
        viewProbe.expectMessageType[GameViewMessages.StartTurnPlayer]
        val game = ended.game.copy(currentRound = 100)
        gameCoordinatorActor ! GCMessage.NewTurn(game, new PlayCycleTurnLog("", 0))
        clientProbe.expectMessageType[ClientMessages.TurnUpdated]
        viewProbe.expectMessageType[GameViewMessages.LastTurnPlayed]
        viewProbe.expectMessageType[GameViewMessages.GameEndedByTurnsLimit]
      }
    }

  }

  private def endTurnByTime(): Unit = {
    gameCoordinatorActor ! GCMessage.TurnTimeEnded()
  }

  private def endMyTurnForMaxTimeReached() = {
    endTurnByTime()
    viewProbe.expectMessageType[GameViewMessages.EndTurnByTimeEnded]
    viewProbe.expectMessageType[GameViewMessages.LastTurnPlayed]
    viewProbe.expectMessageType[GameViewMessages.StartTurnPlayer]
    clientProbe.expectMessageType[ClientMessages.TurnEnded]
    //    clientProbe.expectMessageType[ClientMessages.TurnUpdated]
  }

  private def startActorAndGenerateGameData(): Unit = {
    val generatedGame = generateGameInConstruction(
      playerIDs = List(userID, opponent01),
      probes = List(createTestProbe[Message](), createTestProbe[Message]())
    )
    gameCoordinatorActor = testKit.spawn(GameCoordinatorActor(clientProbe.ref, viewProbe.ref, userID, generatedGame))
  }

  private def startActorWithSortedDeckData(): Unit = {
    val generatedGameInProgress = generateGameInProgress(
      playerIDs = List(userID, "Adversary01"),
    )
    gameCoordinatorActor = testKit.spawn(GameCoordinatorActor(clientProbe.ref, viewProbe.ref, userID, generatedGameInProgress))
  }

  private def skipGeneration(): Unit = {
    //    startActorAndGenerateGameData()
    //    clientProbe.expectMessageType[ClientMessages.TakeGetInProgressGame]
    startActorWithSortedDeckData()
  }

  private def jumpToRevealingSection() = {
    skipGeneration()
    gameCoordinatorActor ! GCMessage.StartGame()
    viewProbe.expectMessageType[GameViewMessages.StartGame].game
  }

  private def jumpToAfterRevealingSection() = {
    val game = jumpToRevealingSection()
    for i <- 0 until Game.cardsInitialVisible do
      showYourNthCard(i)
    viewProbe.expectMessageType[GameViewMessages.WaitAfterRevealingSection]
    clientProbe.expectMessageType[ClientMessages.IntialPhaseCompleted]
    game
  }


  private def jumpToFirstTurnAndGetGame() = {
    val game = jumpToAfterRevealingSection()
    gameCoordinatorActor ! GCMessage.StartPlayCycle()
    viewProbe.expectMessageType[GameViewMessages.StartTurnPlayer]
    game
  }

  @tailrec
  private def jumpRoundsUntilMyTurnAgain(newGameState: GameInProgress, maxCycle: Int = 5): GameInProgress = {
    gameCoordinatorActor ! GCMessage.GetEmptyTurn(userID)
    val turnEnded = clientProbe.expectMessageType[ClientMessages.TurnEnded]
    val x = newGameState.copy(currentRound = turnEnded.game.currentRound)

    gameCoordinatorActor ! GCMessage.NewTurn(x, turnEnded.turnLog)
    clientProbe.expectMessageType[ClientMessages.TurnUpdated]
    viewProbe.expectMessageType[GameViewMessages.LastTurnPlayed]
    val startTurnMsg = viewProbe.expectMessageType[GameViewMessages.StartTurnPlayer]

    if startTurnMsg.playerID == userID then x
    else if maxCycle <= 0 then throw new RuntimeException("Max cycle reached in jumpRoundsUntilMyTurnAgain to avoid infinite recursion.")
    else jumpRoundsUntilMyTurnAgain(newGameState, maxCycle - 1)
  }

  private def reqGameInformation() =
    gameCoordinatorActor ! GCMessage.SendGameStatus(statusProbe.ref)
    statusProbe.expectMessageType[GCMessage.GameInformation].game

  private def drawAngGetCardFromDeck() =
    gameCoordinatorActor ! GCMessage.DrawCardFromDeck()
    viewProbe.expectMessageType[GameViewMessages.CardDrawn].card

  private def drawCardFromDiscardStack() =
    gameCoordinatorActor ! GCMessage.DrawCardFromDiscardStack()
    viewProbe.expectMessageType[GameViewMessages.CardDrawn].card

  private def discardCardDrawn(): Unit = gameCoordinatorActor ! GCMessage.DiscardCardDrawn()

  private def discardNthCard(index: Int): Unit = gameCoordinatorActor ! GCMessage.DiscardYourNthCard(index)

  private def showYourNthCard(i: Int) =
    gameCoordinatorActor ! GCMessage.ShowYourNthCard(i)
    viewProbe.expectMessageType[GameViewMessages.CardSeen].card

  private def showAdversaryNthCard(playerID: String, cardIndex: Int) =
    gameCoordinatorActor ! GCMessage.ShowAdversaryNthCard(playerID, cardIndex)
    viewProbe.expectMessageType[GameViewMessages.CardSeen].card

  private def replaceOwnNthCardWithAdversaryNthOne(ownCardIndex: Int, adversaryID: String, adversaryCardIndex: Int): Unit =
    gameCoordinatorActor ! GCMessage.ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndex, adversaryID, adversaryCardIndex)

  private def endTurn(): Unit = gameCoordinatorActor ! GCMessage.EndTurn()

  private def callCabo(): Unit = gameCoordinatorActor ! GCMessage.CallCabo()

  // SKIP MESSAGES

  private def skipViewProbeMessage() = viewProbe.receiveMessages(1)

  private def skipClientProbeMessage() = clientProbe.receiveMessages(1)

  // GENERATORS

  def generateGameInConstruction(playerIDs: List[String], probes: List[TestProbe[Message]]): GameInConstruction = {
    assert(playerIDs.size == probes.size, "IDs quantity must be equal to probes one")

    val playersInLobby = playerIDs.zip(probes).map { case (id, probe) =>
      PlayerInLobby(
        userID = id,
        name = s"player_$id",
        address = probe.ref
      )
    }

    GameInConstruction(
      code = "test-game-code",
      gameParameters = GameParameters(),
      players = playersInLobby
    )
  }

  def generateGameInProgress(playerIDs: List[String], shuffle: Boolean = false): Game.GameInProgress = {
    var currentDeck = if (shuffle) model.CardStack.buildShuffledFullDeck else model.CardStack.buildSortedFullDeck

    val playersPlaying = playerIDs.zipWithIndex.map { case (id, index) =>
      val (cards, newDeck) = currentDeck.drawNCards(4)
      currentDeck = newDeck
      model.PlayerPlaying(id, s"Name_$id", index + 1, model.Hand(cards))
    }

    val (firstDiscard, finalDeck) = currentDeck.drawFirstCard
    val discardStack = model.CardStack.buildEmptyDeck.addTopCard(firstDiscard)

    Game.GameInProgress(
      code = "test-game-in-progress",
      //      gameParameters = GameParameters(maxPlayers = playerIDs.size),
      gameParameters = GameParameters(maxPlayers = playerIDs.size, roundLimitation = 20),
      gameStatus = model.GameStatus.InProgress(),
      players = playersPlaying,
      deckStack = finalDeck,
      discardDeckStack = discardStack,
      currentRound = 1
    )
  }
