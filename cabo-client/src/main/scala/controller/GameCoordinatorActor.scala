package controller

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import model.{GameInProgress, GameParameters}

object GameCoordinatorActor:

  import akka.actor.typed.ActorRef

  import utils.Message
  import utils.GameCoordinatorMessage

  val game: GameInProgress = GameInProgress(GameParameters(maxTimeRound = 5), List.empty, "code", 0)

  def apply(gameCoordinatorRef: ActorRef[Message]): Behavior[Message] = Behaviors.setup { ctx =>
    ctx.log.info("GameLogic Actor started")
    idle(gameCoordinatorRef)
  }

  private def idle(gameCoordinatorRef: ActorRef[Message]): Behavior[Message] = Behaviors.receivePartial {
    handleDrawCardFromDeck(gameCoordinatorRef, idle)
  }

  private def handleDrawCardFromDeck(gameCoordinatorRef: ActorRef[Message], nextBehaviors: ActorRef[Message] => Behavior[Message]): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DrawCardFromDeck()) =>
      ctx.log.info(s"I draw a card from deck")

      val card = game.deck.head

      gameCoordinatorRef ! GameCoordinatorMessage.CardDrawn(card)
      nextBehaviors(gameCoordinatorRef)
