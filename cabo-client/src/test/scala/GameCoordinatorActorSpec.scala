import akka.actor.testkit.typed.scaladsl.{ActorTestKit, ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import controller.GameCoordinatorActor
import model.Suit.*
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*
import model.{Card, GameParameters}
import model.Game
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
  }

  private def drawCardFromDeck(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.DrawCardFromDeck()

  private def drawCardFromDiscardStack(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.DrawCardFromDiscardStack()

  private def discardCardDrawn(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.DiscardCardDrawn()

  private def discardNthCard(index: Int): Unit = gameCoordinatorActor ! GameCoordinatorMessage.DiscardYourNthCard(index)

  private def sendGameStatus(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.SendGameStatus(gameCoordinatorProbe.ref)

  private def endTurn(): Unit = gameCoordinatorActor ! GameCoordinatorMessage.EndTurn()

  "Actor Player" must {
    "send status of the game" when {
      "receive the command send status" in {
        sendGameStatus()
        gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation]
      }
      "receive the command to end turn" in {
        sendGameStatus()
        drawCardFromDeck()
        discardCardDrawn()
        endTurn()
        // TODO: decide if codify this sequence to not have magic numbers 4 and 3
        val messages = gameCoordinatorProbe.receiveMessages(4)
        messages(3) mustBe a[GameCoordinatorMessage.GameInformation]
      }
    }
    // Draw a card when receive draw command and send what he draws
    "send information about the card drawn" when {
      "receive the command to draw a card from deck" in {
        sendGameStatus()
        val message = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation]
        val (firstCardOfDeck, _) = message.game.deckStack.drawFirstCard
        drawCardFromDeck()
        gameCoordinatorProbe.expectMessage(CardDrawn(firstCardOfDeck))
      }
      "receive the command to draw a card from discard stack" in {
        sendGameStatus()
        val message = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation]
        val (firstCardOfDiscardStack, _) = message.game.discardDeckStack.drawFirstCard
        drawCardFromDiscardStack()
        gameCoordinatorProbe.expectMessage(CardDrawn(firstCardOfDiscardStack))
      }
    }

    // Discard when receive the command to discard and send it
    "send new top card of discard card stack equal to card drawn" when {
      "receive the command to discard the card drawn without exchange any of own" in {
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
        //        discardCardDrawn()
        //        val _ = gameCoordinatorProbe.expectMessageType[NewTopCardDiscardStack]
        //        endTurn()
        //        val newGameState = gameCoordinatorProbe.expectMessageType[GameCoordinatorMessage.GameInformation].game
        //        println(newGameState)
        //        val (topCardDiscardStack, _) = newGameState.discardDeckStack.drawFirstCard
        //        topCardDiscardStack mustBe cardDrawn
      }
    }


    // during the game there are more ways to see the cards
    "send card value" when {
      // at start or power
      "receive the command to see one of own card" in {
        fail("Not implemented yet")
      }
      // with power
      "receive the command to see one card of opponents" in {
        fail("Not implemented yet")
      }
    }
  }
