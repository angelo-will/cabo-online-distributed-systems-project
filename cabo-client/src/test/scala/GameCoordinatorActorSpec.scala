import akka.actor.testkit.typed.scaladsl.{ActorTestKit, ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import controller.GameCoordinatorActor
import model.Suit.*
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*
import model.{Card, Game, GameParameters, Power}
import utils.GameCoordinatorMessage.{CardDrawn, DiscardYourNthCard, DrawCardFromDeck, NewTopCardDiscardStack}
import utils.{GameCoordinatorMessage, Message, ServerMessages}

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

  private var gameCoordinatorActor: ActorRef[Message] = _
  private var gameCoordinatorProbe: TestProbe[Message] = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    gameCoordinatorProbe = createTestProbe[Message]()
    gameCoordinatorActor = testKit.spawn(GameCoordinatorActor(gameCoordinatorProbe.ref, playerRank = 0))
    gameCoordinatorActor ! GameCoordinatorMessage.StartGame()
  }

  private def skipFirstShowPhase(): Unit =
    showYourNthCard(0)
    showYourNthCard(0)
    val _ = gameCoordinatorProbe.receiveMessages(2)

  private def jumpARound(): Unit =
    drawCardFromDeck()
    val card = gameCoordinatorProbe.expectMessageType[CardDrawn].card
    card.power match
      case Power.SeeYourCard() =>
        showYourNthCard(0)
        discardCardDrawn()
        val _ = gameCoordinatorProbe.receiveMessages(2)
      case Power.SeeYourOpponentCard() =>
        showAdversaryNthCard(0,0)
        discardCardDrawn()
        val _ = gameCoordinatorProbe.receiveMessages(2)
      case Power.NoPower() => discardCardDrawn()
        val _ = gameCoordinatorProbe.expectMessageType[NewTopCardDiscardStack]
    endTurn()
    val gameInformation = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
    newTurn(gameInformation)

  private def skipRoundUntilThisPowerAppear(power: Power): Game.GameInProgress =
    sendGameStatus()
    var game = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
    while game.deckStack.drawFirstCard._1.power != power do
      jumpARound()
      sendGameStatus()
      game = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
    game

  private def drawCardFromDeck(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.DrawCardFromDeck()

  private def drawCardFromDiscardStack(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.DrawCardFromDiscardStack()

  private def discardCardDrawn(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.DiscardCardDrawn()

  private def discardNthCard(index: Int): Unit = gameCoordinatorActor ! GameCoordinatorMessage.DiscardYourNthCard(index)

  private def showYourNthCard(i: Int): Unit = gameCoordinatorActor ! GameCoordinatorMessage.ShowYourNthCard(i)

  private def showAdversaryNthCard(playerIndex: Int, cardIndex: Int): Unit = gameCoordinatorActor ! GameCoordinatorMessage.ShowAdversaryNthCard(playerIndex, cardIndex)

  private def sendGameStatus(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.SendGameStatus(gameCoordinatorProbe.ref)

  private def endTurn(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.EndTurn()

  private def newTurn(game: Game.GameInProgress): Unit = gameCoordinatorActor ! GameCoordinatorMessage.NewTurn(game)

  "Single Actor Player" must {

    "send status of the game" when {
      "receive the command send status" in {
        sendGameStatus()
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
        sendGameStatus()
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
        sendGameStatus()
        val message = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation]
        val (firstCardOfDeck, _) = message.game.deckStack.drawFirstCard
        drawCardFromDeck()
        gameCoordinatorProbe.expectMessage(CardDrawn(firstCardOfDeck))
      }
      "receive the command to draw a card from discard stack" in {
        skipFirstShowPhase()
        sendGameStatus()
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
        sendGameStatus()
        val game = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
        println(game)
        val (cardDrawn, newDeck) = game.deckStack.drawFirstCard
        drawCardFromDeck()
        val _ = gameCoordinatorProbe.expectMessageType[CardDrawn]
        discardCardDrawn()
        val _ = gameCoordinatorProbe.expectMessageType[NewTopCardDiscardStack]
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
        sendGameStatus()
        val game = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
        println(game)
        val (cardDrawn, newDeck) = game.deckStack.drawFirstCard
        drawCardFromDeck()
        val _ = gameCoordinatorProbe.expectMessageType[CardDrawn]
        val handCards = game.players(indexPlayer).hand.cards
        val cardToDiscard = handCards(indexCardToDiscard)
        discardNthCard(indexCardToDiscard)
        val cardDiscarded = gameCoordinatorProbe.expectMessageType[NewTopCardDiscardStack].card
        cardDiscarded mustBe cardToDiscard
      }
    }


    // during the game there are more ways to see the cards
    "send card value" when {
      // at start or power
      "receive the command to see one of own card" in {
        // TODO:
        //  È possibile farlo all'inizio del gioco o quando hai una carta potere
        //  effettuare questo test per la fase iniziale in cui puoi vedere due carte
        val indexPlayer = 0
        val indexCardToSee = 2
        sendGameStatus()
        val game = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
        println(game)
        val handCards = game.players(indexPlayer).hand.cards
        val cardToSee = handCards(indexCardToSee)
        showYourNthCard(indexCardToSee)
        val cardSeen = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.CardSeen].card
        cardSeen mustBe cardToSee
      }
//      // with power
//      "receive the command to see one card of opponents" in {
////        fail("Not implemented yet")
//      }
    }

    // powers implementation
    "send own card value" when {
      "drawn card with power to show one of own card" in {
        skipFirstShowPhase()
        val game = skipRoundUntilThisPowerAppear(Power.SeeYourCard())
        drawCardFromDeck()
        val cardToSee = game.players(0).hand.cards(0)
        val _ = gameCoordinatorProbe.expectMessageType[CardDrawn].card
        showYourNthCard(0)
        val cardSeen = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.CardSeen].card
        cardSeen mustBe cardToSee
      }
    }

    "send adversary card value" when {
      "drawn card with power to see one of adversary card" in {
        skipFirstShowPhase()
        val game = skipRoundUntilThisPowerAppear(Power.SeeYourOpponentCard())
        drawCardFromDeck()
        val cardToSeeIndex = 0
        val playerIndex = 1
        val cardToSee = game.players(playerIndex).hand.cards(cardToSeeIndex)
        val _ = gameCoordinatorProbe.expectMessageType[CardDrawn].card
        showAdversaryNthCard(playerIndex,cardToSeeIndex)
        val cardSeen = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.CardSeen].card
        cardSeen mustBe cardToSee
      }
    }
    "change one of own card with adversary one" when {
      "drawn card with power to change one of own card" in {
        skipFirstShowPhase()


//        fail("Not implemented yet")
      }
    }
  }
