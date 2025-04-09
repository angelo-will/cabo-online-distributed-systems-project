import org.scalatest.matchers.must.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.wordspec.AnyWordSpecLike

import scala.language.postfixOps

class CardSpec extends AnyWordSpecLike
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with Matchers:

  import model.*

  "Card" must {
    "have a score" when {
      "a card is created" in {
        val card = Card("5", Suit.Spades())
        card.score mustBe 5
      }
    }

    "have a power" when {
      "a card is created" in {
        val card = Card("J", Suit.Spades())
        card.power mustBe Power.SeeYourCard()
      }
    }
  }

  "Hand" must {
    "have a score" when {
      "a hand is created" in {
        val fiveOfSpades = Card("5", Suit.Spades())
        val jackOfClubs = Card("J", Suit.Clubs())
        val kingOfHearts = Card("K", Suit.Hearts())
        val twoOfDiamonds = Card("2", Suit.Diamonds())
        val listHand = List(fiveOfSpades, jackOfClubs, kingOfHearts, twoOfDiamonds)
        val hand = Hand(listHand)
        hand.score mustBe listHand.map(_.score).sum
      }
    }
    "change a card" when {
      "a card is changed" in {
        val fiveOfSpades = Card("5", Suit.Spades())
        val jackOfClubs = Card("J", Suit.Clubs())
        val kingOfHearts = Card("K", Suit.Hearts())
        val twoOfDiamonds = Card("2", Suit.Diamonds())
        val listHand = List(fiveOfSpades, jackOfClubs, kingOfHearts, twoOfDiamonds)
        val hand = Hand(listHand)
        val newCard = Card("3", Suit.Diamonds())
        val newHand = hand.changeFirstCard(newCard)
        newHand.cards mustBe List(newCard, jackOfClubs, kingOfHearts, twoOfDiamonds)
      }
    }
  }

  "Deck" must {
    "be equal to another" when {
      "have the same cards in the same order" in {
        val deck1 = CardStack.buildSortedFullDeck
        val deck2 = CardStack.buildSortedFullDeck
        deck1 mustBe deck2
      }
    }
    "give a random card" when {
      "a card is drawn from the deck" in {
        val deck = CardStack.buildShuffledFullDeck
        val (card, newDeck) = deck.drawFirstCard
        card mustBe a[Card]
      }
    }
    "have the card added" when {
      "a card is added to the deck" in {
        val deck = CardStack.buildEmptyDeck
        val card = Card("5", Suit.Spades())
        val newDeck = deck.addCard(card)
        newDeck.cards must contain(card)
      }
    }
  }

