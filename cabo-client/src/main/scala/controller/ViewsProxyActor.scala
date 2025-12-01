package controller

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import utils.Message
import utils.InitialViewMessages
import utils.DuringGameViewMessages
import view.lobbyphase.actors.InitialPhaseViewActor
import view.gamephase.actors.DuringGameViewActor

object ViewsProxyActor {

  sealed trait Command extends Message

  case class SwitchToInitialView() extends Command

  case class SwitchToGameView() extends Command

  def apply(userId: String, userName: String, clientRef: ActorRef[Message]): Behavior[Message] =
    Behaviors.setup { ctx =>
      ctx.log.info(s"ViewCoordinator started for user $userId")

      val initialView = ctx.spawn(InitialPhaseViewActor(clientRef, userName), "InitialView")

      initialView ! InitialViewMessages.WhoToSendResponse(clientRef)

      new ViewsProxyActor(ctx, userId, userName, clientRef).active(initialView, isGamePhase = false)
    }
}

private class ViewsProxyActor(ctx: ActorContext[Message], userId: String, userName: String, clientRef: ActorRef[Message]) {

  import ViewsProxyActor.*

  def active(currentView: ActorRef[Message], isGamePhase: Boolean): Behavior[Message] = {
    Behaviors.receiveMessage {
      case SwitchToGameView() =>
        ctx.log.info("Switching to GAME View")
        val actorName = s"DuringGameView-$userId-${System.currentTimeMillis()}"
        val gameView = ctx.spawn(DuringGameViewActor(userId, clientRef, null), actorName)
        active(gameView, isGamePhase = true)

      case SwitchToInitialView() =>
        ctx.log.info("Switching to INITIAL View")
        ctx.stop(currentView)
        val actorName = s"InitialView-$userId-${System.currentTimeMillis()}"
        val initialView = ctx.spawn(InitialPhaseViewActor(clientRef, userName), actorName)
        initialView ! InitialViewMessages.WhoToSendResponse(clientRef)
        active(initialView, isGamePhase = false)

      case msg: InitialViewMessages.ViewCommand if !isGamePhase =>
        currentView ! msg
        Behaviors.same

      case msg: DuringGameViewMessages.DuringGameViewMessage if isGamePhase =>
        currentView ! msg
        Behaviors.same

      case other =>
        ctx.log.debug(s"Received unknown message to view: $other")
        Behaviors.same
    }
  }
}