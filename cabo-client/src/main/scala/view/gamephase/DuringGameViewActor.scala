package view.gamephase

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import utils.{GameCoordinatorMessage, Message}

object DuringGameViewActor {
  def apply(clientRef: ActorRef[Message]): Behavior[Message] = {
    //    val duringGameMainFrame = new DuringGameMainFrame()
    new DuringGameViewActor(clientRef).start()
  }
}

class DuringGameViewActor private(val clientRef: ActorRef[Message]) {
  private case class Properties(
                                 gameCoordinatorRef: ActorRef[GameCoordinatorMessage.PlayerCommand],
                                 frame: DuringGameMainFrame
                               )

  def start(): Behavior[Message] = Behaviors.setup { ctx =>
    ctx.log.info("DuringGameViewActor started")
    val frame = new DuringGameMainFrame(DuringGameViewListener(ctx.self))
    frame.open()
    frame.visible = true

    clientRef ! utils.ClientMessages.DuringGameViewReady(ctx.self)

    waitingGameCreated(Properties(gameCoordinatorRef = null, frame = frame))
  }

  private def waitingGameCreated(properties: Properties): Behavior[Message] = {
    Behaviors.receivePartial {
      handleGameStarted()
        .orElse({
          case msg =>
            println(s"DuringGameViewActor in waitingGameCreated received message: $msg")
            Behaviors.same
        })
    }
  }

  private def handleGameStarted(): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, msg) =>
      ctx.log.info(s"DuringGameViewActor handling game started with message: $msg")
      msg match {
        case _ =>
          ctx.log.info("Game started, switching to game in progress behavior")
          Behaviors.same
      }
  }
}
