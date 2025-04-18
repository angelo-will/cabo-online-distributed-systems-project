package model

import scala.concurrent.ExecutionContext.Implicits.global

abstract class Suit(val name: String, val shortName: String)

abstract class Rank(val value: Int, val name: String, val shortName: String)

object Suit:
  case class Clubs() extends Suit("Clubs", "♣")

  case class Spades() extends Suit("Spades", "♠")

  case class Diamonds() extends Suit("Diamonds", "♦")

  case class Hearts() extends Suit("Hearts", "♥")

  // italian suits order
  def all: List[Suit] = List(Hearts(), Diamonds(), Clubs(), Spades())

  implicit def string2suit(s: String): Suit = s match
    case "♣" => Clubs()
    case "♠" => Spades()
    case "♦" => Diamonds()
    case "♥" => Hearts()
    case _ => throw new RuntimeException(f"Unknown suit ${s}")


object Rank:
  case class Ace() extends Rank(1, "Ace", "A")

  case class Two() extends Rank(2, "Two", "2")

  case class Three() extends Rank(3, "Three", "3")

  case class Four() extends Rank(4, "Four", "4")

  case class Five() extends Rank(5, "Five", "5")

  case class Six() extends Rank(6, "Six", "6")

  case class Seven() extends Rank(7, "Seven", "7")

  case class Eight() extends Rank(8, "Eight", "8")

  case class Nine() extends Rank(9, "Nine", "9")

  case class Ten() extends Rank(10, "Ten", "10")

  case class Jack() extends Rank(11, "Jack", "J")

  case class Queen() extends Rank(12, "Queen", "Q")

  case class King() extends Rank(13, "King", "K")

  def all: List[Rank] = List(Ace(), Two(), Three(), Four(), Five(), Six(), Seven(), Eight(), Nine(), Ten(), Jack(), Queen(), King())

  implicit def string2rank(s: String): Rank = s match
    case "A" => Ace()
    case "2" => Two()
    case "3" => Three()
    case "4" => Four()
    case "5" => Five()
    case "6" => Six()
    case "7" => Seven()
    case "8" => Eight()
    case "9" => Nine()
    case "0" => Ten()
    case "J" => Jack()
    case "Q" => Queen()
    case "K" => King()
    case _ => throw new RuntimeException(f"Unknown rank ${s}")

object Card:
  private val pattern = "^([AJKQ2-9])\\s*([♣♠♦♥])$".r

  def fullDeck: List[Card] = for {
    suit <- Suit.all
    rank <- Rank.all
  } yield Card(rank, suit)

  implicit def string2card(s: String): Card = s match
    case pattern(rank, suit) => Card(rank, suit)
    case _ => throw new RuntimeException(f"Invalid card string $s")

case class Card(rank: Rank, suit: Suit):

  def name: String = f"${rank.name} of ${suit.name}"

  def shortName: String = f"${rank.shortName}${suit.shortName}"

  def score: Int =
    rank match
      case Rank.Ace() => 1
      case Rank.Two() => 2
      case Rank.Three() => 3
      case Rank.Four() => 4
      case Rank.Five() => 5
      case Rank.Six() => 6
      case Rank.Seven() => 7
      case Rank.Eight() => 8
      case Rank.Nine() => 9
      case Rank.Ten() => 0
      case Rank.Jack() => 10
      case Rank.Queen() => 10
      case Rank.King() => 10
      case _ => 10

  def power: Power = rank match
    case Rank.Jack() => Power.SeeYourCard()
    case Rank.Queen() => Power.SeeYourOpponentCard()
    case Rank.King() => Power.ChangeOneOfYourCardWithOpponent()
    case _ => Power.NoPower()

  def canFish(other: Card): Boolean =
    if (rank == Rank.Jack()) true
    else this == other

  override def toString: String = name

object CardStack:
  /**
   * Creates a sorted full deck of cards.
   *
   * @return a CardStack containing all cards in sorted order
   */
  def buildSortedFullDeck: CardStack = CardStack(Card.fullDeck)

  /**
   * Creates a shuffled full deck of cards.
   *
   * @return a CardStack containing all cards in shuffled order
   */
  def buildShuffledFullDeck: CardStack = CardStack(scala.util.Random.shuffle(Card.fullDeck))

  /**
   * Creates an empty deck of cards.
   *
   * @return a CardStack containing no cards
   */
  def buildEmptyDeck: CardStack = CardStack(List())

  implicit def cards2stack(cards: List[Card]): CardStack = CardStack(cards)

  def removeLast[A](list: List[A], item: A): List[A] =
    def remove(iter: List[A]): List[A] = iter match {
      case x :: y =>
        if (x == item) y
        else x :: remove(y)
      case Nil => Nil
    }

    remove(list.reverse).reverse

case class CardStack(cards: List[Card]):
  def removeCard(card: Card): CardStack = CardStack(CardStack.removeLast(cards, card))

  /**
   * Removes a list of cards from the stack.
   *
   * @param cards the cards to be removed
   * @return a new CardStack with the specified cards removed
   */
  def removeCards(cards: Seq[Card]): CardStack = cards.foldLeft(this)((stack, card) => stack.removeCard(card))

  /**
   * Adds a card at the end of the stack.
   *
   * @param card the card to be added
   * @return a new CardStack with the specified card added
   */
  def addEndCard(card: Card): CardStack = CardStack(cards :+ card)
  
  def addTopCard(card: Card): CardStack = CardStack(card :: cards)

  /**
   * Draws the first card from the stack and returns it along with the new CardStack.
   *
   * @return a tuple containing the drawn card and the new CardStack
   */
  def drawFirstCard: (Card, CardStack) = (cards.head, CardStack(cards.tail))

  def isEmpty: Boolean = cards.isEmpty

  override def toString: String = "CardStack(" + cards.mkString(", ") + ")"

object Hand:
  private val maxCardsNumber = 4

  /**
   * Creates a hand with the given cards.
   *
   * @param cards the cards to be included in the hand
   * @throws IllegalArgumentException if the number of cards is not equal to 4
   */
  def apply(cards: List[Card]): Hand =
    if (cards.size != maxCardsNumber)
      throw new IllegalArgumentException(s"Hand must have $maxCardsNumber cards, but has ${cards.size}")
    else
      new Hand(cards)

/**
 * Represents a hand of cards.
 *
 * @param cards the cards in the hand
 */
case class Hand private(cards: List[Card]):

  def score: Int = cards.map(_.score).sum

  def viewFirstCard: Card = cards.head

  def viewSecondCard: Card = cards(1)

  def viewThirdCard: Card = cards(2)

  def viewFourthCard: Card = cards(Hand.maxCardsNumber - 1)

  def changeFirstCard(card: Card): Hand = Hand(cards.updated(0, card))

  def changeSecondCard(card: Card): Hand = Hand(cards.updated(1, card))

  def changeThirdCard(card: Card): Hand = Hand(cards.updated(2, card))

  def changeFourthCard(card: Card): Hand = Hand(cards.updated(3, card))
