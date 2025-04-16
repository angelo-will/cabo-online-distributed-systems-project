package controller

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import model.Suit.Spades
import model.{Card, GameInProgress, GameParameters}

object GameCoordinatorActor:

  import akka.actor.typed.ActorRef

  import utils.Message
  import utils.GameCoordinatorMessage
  import model.CardStack
  import model.GameStatus

  val debugValue: GameInProgress = GameInProgress(
    "gameCode",
    GameParameters(maxTimeRound = 5),
    GameStatus.InProgress(),
    List.empty,
    CardStack.buildSortedFullDeck,
    CardStack(List(Card("A", Spades()))),
    0
  )

  def apply(gameCoordinatorRef: ActorRef[Message]): Behavior[Message] = Behaviors.setup { ctx =>
    ctx.log.info("GameLogic Actor started")
    myTurn(gameCoordinatorRef)
  }

  private def myTurn(gameCoordinatorRef: ActorRef[Message]): Behavior[Message] = Behaviors.receivePartial {
    handleDrawCardFromDeck(gameCoordinatorRef, myTurn)
      .orElse(handleDrawCardFromDiscardStack(gameCoordinatorRef, myTurn))
  }

  private def handleDrawCardFromDeck(gameCoordinatorRef: ActorRef[Message], nextBehaviors: ActorRef[Message] => Behavior[Message]): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DrawCardFromDeck()) =>
      ctx.log.info(s"I draw a card from deck")

      val (topCard, _) = debugValue.deckStack.drawFirstCard

      gameCoordinatorRef ! GameCoordinatorMessage.CardDrawn(topCard)
      nextBehaviors(gameCoordinatorRef)

  private def handleDrawCardFromDiscardStack(gameCoordinatorRef: ActorRef[Message], nextBehaviors: ActorRef[Message] => Behavior[Message]): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DrawCardFromDiscardStack()) =>
      ctx.log.info(s"I draw a card from discard stack")

      val (topCard, _) = debugValue.discardDeckStack.drawFirstCard

      gameCoordinatorRef ! GameCoordinatorMessage.CardDrawn(topCard)
      nextBehaviors(gameCoordinatorRef)