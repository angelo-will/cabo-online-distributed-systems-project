package model

import scala.concurrent.ExecutionContext.Implicits.global

abstract class Suit(val name: String, val shortName: String)

abstract class Rank(val value: Int, val name: String, val shortName: String)

object Suit:
  case class Clubs() extends Suit("Clubs", "♣")

  case class Spades() extends Suit("Spades", "♠")

  case class Diamonds() extends Suit("Diamonds", "♦")

  case class Hearts() extends Suit("Hearts", "♥")

  def all: List[Suit] = List(Clubs(), Spades(), Diamonds(), Hearts())

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

  import model.Power

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

  def canFish(other: Card): Boolean =
    if (rank == Rank.Jack()) true
    else this == other

  override def toString: String = shortName

object CardStack:
  def sorted: CardStack = CardStack(Card.fullDeck)

  def shuffled: CardStack = CardStack(scala.util.Random.shuffle(Card.fullDeck))

  val empty: CardStack = CardStack(List())

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
  def removed(card: Card): CardStack = CardStack(CardStack.removeLast(cards, card))

  def removed(cards: Seq[Card]): CardStack = cards.foldLeft(this)((stack, card) => stack.removed(card))

  def added(card: Card): CardStack = CardStack(cards :+ card)

  def isEmpty: Boolean = cards.isEmpty

  override def toString: String = cards.mkString(", ")
