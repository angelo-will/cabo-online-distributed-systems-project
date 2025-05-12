import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import controller.GameCoordinatorActor
import model.Suit.*
import model.{Card, Game, Power}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import utils.GameCoordinatorMessage.{CardDrawn, DiscardYourNthCard, DrawCardFromDeck, NewTopCardDiscardStack}
import utils.{GameCoordinatorMessage, Message}

class GameCoordinatorActorSpec extends ScalaTestWithActorTestKit
  with AnyWordSpecLike
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with Matchers:

  import org.scalatest.matchers.must.Matchers.mustBe

  // test sequence of steps that user makes for a turn
  // instructions of test emulate messages from view actor to actor representing player
  // view expects messages from player actor
  // other players' actors expect messages from player actor in the end of the turn to know what happened

  // TODO: TEST WORK ONLY WITH SORTED DECK TO GARANTEE THE REPRODUCIBILTY OF IT

  private var gameCoordinatorActor: ActorRef[Message] = _
  private var gameCoordinatorProbe: TestProbe[Message] = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    gameCoordinatorProbe = createTestProbe[Message]()
    gameCoordinatorActor = testKit.spawn(GameCoordinatorActor(gameCoordinatorProbe.ref, playerRank = 0))
    gameCoordinatorActor ! GameCoordinatorMessage.StartGame()
  }

  "Single Actor Player" must {

    "send status of the game" when {
      "receive the command send status" in {
        sendGameInformation()
        gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation]
      }
      "receive the command to end turn" in {
        println("START TEST: send status of the game when receive the command to end turn")
        skipFirstShowPhase()
        val commands: List[() => Unit] = List(
          drawCardFromDeck,
          discardCardDrawn,
          endTurn
        )
        commands.foreach(_())

        val messages = gameCoordinatorProbe.receiveMessages(commands.size)
        messages.last mustBe a[GameCoordinatorMessage.GameInformation]
        println("END TEST: send status of the game when receive the command to end turn")
      }
    }

    "send own card value" when {
      "at the start of the game when selected one of own card" in {
        println("START TEST: send own card value when at the start of the game when selected one of own card")
        sendGameInformation()
        val indexOfPlayer = 0
        val indexOfFirstCardSelected = 0
        val indexOfSecondCardSelected = 2
        val hand = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game.players(indexOfPlayer).hand
        val firstCardSelected = hand.cards(indexOfFirstCardSelected)
        val secondCardSelected = hand.cards(indexOfSecondCardSelected)
        showYourNthCard(indexOfFirstCardSelected)
        val firstCardSeen = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.CardSeen].card
        showYourNthCard(indexOfSecondCardSelected)
        val secondCardSeen = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.CardSeen].card
        firstCardSeen mustBe firstCardSelected
        secondCardSeen mustBe secondCardSelected
        println("END TEST: send own card value when at the start of the game when selected one of own card")
      }
    }

    // Draw a card when receive draw command and send what he draws
    "send information about the card drawn" when {
      "receive the command to draw a card from deck" in {
        skipFirstShowPhase()
        sendGameInformation()
        val message = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation]
        val (firstCardOfDeck, _) = message.game.deckStack.drawFirstCard
        drawCardFromDeck()
        gameCoordinatorProbe.expectMessage(CardDrawn(firstCardOfDeck))
      }
      "receive the command to draw a card from discard stack" in {
        skipFirstShowPhase()
        sendGameInformation()
        val message = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation]
        val (firstCardOfDiscardStack, _) = message.game.discardDeckStack.drawFirstCard
        drawCardFromDiscardStack()
        gameCoordinatorProbe.expectMessage(CardDrawn(firstCardOfDiscardStack))
      }
    }

    //    // Discard when receive the command to discard and send it
    "send new top card of discard card stack equal to card drawn" when {
      "receive the command to discard the card drawn without exchange any of own" in {
        skipFirstShowPhase()
        sendGameInformation()
        val game = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
        println(game)
        val (cardDrawn, newDeck) = game.deckStack.drawFirstCard
        skipDrawCardFromDeckPhase()
        skipDiscardCardDrawnPhase()
        endTurn()
        val newGameState = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
        println(newGameState)
        val (topCardDiscardStack, _) = newGameState.discardDeckStack.drawFirstCard
        topCardDiscardStack mustBe cardDrawn
      }
    }

    "send new top card of discard card stack equal to one of own cards" when {
      "receive the command to discard one of own cards" in {
        skipFirstShowPhase()
        val indexPlayer = 0
        val indexCardToDiscard = 2
        sendGameInformation()
        val game = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
        println(game)
        val (cardDrawn, newDeck) = game.deckStack.drawFirstCard
        skipDrawCardFromDeckPhase()
        val handCards = game.players(indexPlayer).hand.cards
        val cardToDiscard = handCards(indexCardToDiscard)
        discardNthCard(indexCardToDiscard)
        val cardDiscarded = gameCoordinatorProbe.expectMessageType[NewTopCardDiscardStack].card
        cardDiscarded mustBe cardToDiscard
      }
    }

    // powers implementation

    "send own card value" when {
      "drawn card with power to show one of own card" in {
        skipFirstShowPhase()
        val (cardDrawn, game) = skipRoundsUntilDrawThisPower(Power.SeeYourCard())
        val cardToSee = game.players(0).hand.cards(0)
        showYourNthCard(0)
        val cardSeen = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.CardSeen].card
        cardSeen mustBe cardToSee
      }
    }

    "send adversary card value" when {
      "drawn card with power to see one of adversary card" in {
        skipFirstShowPhase()
        val (cardDrawn, game) = skipRoundsUntilDrawThisPower(Power.SeeYourOpponentCard())
        val cardToSeeIndex = 0
        val playerIndex = 1
        val cardToSee = game.players(playerIndex).hand.cards(cardToSeeIndex)
        showAdversaryNthCard(playerIndex, cardToSeeIndex)
        val cardSeen = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.CardSeen].card
        cardSeen mustBe cardToSee
      }
    }
    "change one of own card with adversary one" when {
      "drawn card with power to change one of own card" in {
        skipFirstShowPhase()
        val (cardDrawn, game) = skipRoundsUntilDrawThisPower(Power.ChangeOneOfYourCardWithOpponent())
        val ownCardToChangeIndex = 0
        val playerIndex = 1
        val adversaryCardToChangeIndex = 0
        val newOwnCardAfterReplace = game.players(playerIndex).hand.cards(adversaryCardToChangeIndex)
        val newAdversaryCardAfterReplace = game.players(0).hand.cards(ownCardToChangeIndex)
        replaceOwnNthCardWithAdversaryNthOne(ownCardToChangeIndex, playerIndex, adversaryCardToChangeIndex)
        sendGameInformation()
        val gameInformation = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
        gameInformation.players(0).hand.cards(ownCardToChangeIndex) mustBe newOwnCardAfterReplace
        gameInformation.players(1).hand.cards(adversaryCardToChangeIndex) mustBe newAdversaryCardAfterReplace
      }
    }
  }

  private def skipDrawCardFromDeckPhase(): Unit =
    drawCardFromDeck()
    discardMessage()

  private def skipDrawCardFromDiscardStackPhase(): Unit =
    drawCardFromDiscardStack()
    discardMessage()

  private def skipDiscardCardDrawnPhase(): Unit =
    discardCardDrawn()
    discardMessage()

  private def skipFirstShowPhase(): Unit =
    showYourNthCard(0)
    showYourNthCard(0)
    discardMessages(2)

  private def skipPower(power: Power): Unit =
    power match
      case Power.SeeYourCard() => showYourNthCard(0); discardMessage()
      case Power.SeeYourOpponentCard() => showAdversaryNthCard(0, 0); discardMessage()
      case Power.ChangeOneOfYourCardWithOpponent() => replaceOwnNthCardWithAdversaryNthOne(0, 0, 0)

  private def skipARound(): Unit =
    drawCardFromDeck()
    val power = gameCoordinatorProbe.expectMessageType[CardDrawn].card.power
    if power != Power.NoPower() then skipPower(power)
    discardCardDrawn()
    discardMessage()
    endTurn()
    val gameInformation = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
    newTurn(gameInformation)

  private def skipRoundsUntilDrawThisPower(power: Power): (Card, Game.GameInProgress) =
    sendGameInformation()
    var game = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
    while game.deckStack.drawFirstCard._1.power != power do
      skipARound()
      sendGameInformation()
      game = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
    drawCardFromDeck()
    val cardDrawn = gameCoordinatorProbe.expectMessageType[CardDrawn].card
    (cardDrawn, game)

  private def sendGameInformation(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.SendGameStatus(gameCoordinatorProbe.ref)

  private def drawCardFromDeck(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.DrawCardFromDeck()

  private def drawCardFromDiscardStack(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.DrawCardFromDiscardStack()

  private def discardCardDrawn(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.DiscardCardDrawn()

  private def discardNthCard(index: Int): Unit = gameCoordinatorActor ! GameCoordinatorMessage.DiscardYourNthCard(index)

  private def showYourNthCard(i: Int): Unit = gameCoordinatorActor ! GameCoordinatorMessage.ShowYourNthCard(i)

  private def showAdversaryNthCard(playerIndex: Int, cardIndex: Int): Unit = gameCoordinatorActor ! GameCoordinatorMessage.ShowAdversaryNthCard(playerIndex, cardIndex)

  private def replaceOwnNthCardWithAdversaryNthOne(ownCardIndex: Int, adversaryIndex: Int, adversaryCardIndex: Int): Unit =
    gameCoordinatorActor ! GameCoordinatorMessage.ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndex, adversaryIndex, adversaryCardIndex)

  private def endTurn(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.EndTurn()

  private def newTurn(game: Game.GameInProgress): Unit = gameCoordinatorActor ! GameCoordinatorMessage.NewTurn(game)

  private def discardMessage(): Unit = discardMessages(1)

  private def discardMessages(n: Int): Unit = gameCoordinatorProbe.receiveMessages(n)
