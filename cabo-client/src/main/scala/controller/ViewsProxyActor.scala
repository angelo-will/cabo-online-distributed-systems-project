package controller

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import messages.ClientMessages.ClientCommand
import messages.{GameViewMessages, IGameViewMessage, IPreGameViewMessage, IViewMessage, PreGameViewMessages}
import utils.Message
import view.lobbyphase.actors.InitialPhaseViewActor
import view.gamephase.actors.DuringGameViewActor

object ViewsProxyActor {

  case class SwitchToInitialView() extends IViewMessage

  case class SwitchToGameView() extends IViewMessage

  def apply(userId: String, userName: String, clientRef: ActorRef[ClientCommand]): Behavior[IViewMessage] =
    Behaviors.setup { ctx =>
      ctx.log.info(s"ViewCoordinator started for user $userId")
      val initialView = ctx.spawn(InitialPhaseViewActor(clientRef, userName), "InitialView")
      initialView ! PreGameViewMessages.WhoToSendResponse(clientRef)
      new ViewsProxyActor(ctx, userId, userName, clientRef).preGame(initialView)
    }
}

private class ViewsProxyActor(ctx: ActorContext[IViewMessage], userId: String, userName: String, clientRef: ActorRef[ClientCommand]) {

  import ViewsProxyActor.*

  private def preGame(currentView: ActorRef[IPreGameViewMessage]): Behavior[IViewMessage] = {
    Behaviors.receiveMessage {

      case SwitchToGameView() =>
        ctx.log.info("Switching to GAME View")
        val actorName = s"DuringGameView-$userId-${System.currentTimeMillis()}"
        val gameView= ctx.spawn(DuringGameViewActor(userId, clientRef, null), actorName)
        game(gameView)
      case msg: IPreGameViewMessage =>
        currentView ! msg
        Behaviors.same
      case msg: IGameViewMessage =>
        ctx.log.warn(s"Dropped Game message in PreGame phase: $msg")
        Behaviors.same
    }
  }

  private def game(currentView: ActorRef[IGameViewMessage]): Behavior[IViewMessage] = {
    Behaviors.receiveMessage {
      case SwitchToInitialView() =>
        ctx.log.info("Switching to INITIAL View")
        ctx.stop(currentView)
        val actorName = s"InitialView-$userId-${System.currentTimeMillis()}"
        val initialView = ctx.spawn(InitialPhaseViewActor(clientRef, userName), actorName)
        initialView ! PreGameViewMessages.WhoToSendResponse(clientRef)
        preGame(initialView)
      case msg: IGameViewMessage =>
        currentView ! msg
        Behaviors.same
      case msg: IPreGameViewMessage =>
        ctx.log.warn(s"Dropped PreGame message in Game phase: $msg")
        Behaviors.same
    }
  }
}