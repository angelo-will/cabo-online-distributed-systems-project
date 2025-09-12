package view.gamephase

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import utils.Message

object DuringGameViewActor {
  def apply(clientRef: ActorRef[Message]): Behavior[Message] = {
    //    val duringGameMainFrame = new DuringGameMainFrame()
    new DuringGameViewActor(clientRef).start()
  }
}

class DuringGameViewActor private(val clientRef: ActorRef[Message]) {

  private var frame = Option.empty[DuringGameMainFrame]
  private var coordinator = ???

  def start(): Behavior[Message] = Behaviors.setup { ctx =>
    ctx.log.info("DuringGameViewActor started")
    frame = Some(new DuringGameMainFrame(DuringGameViewListener(ctx.self)))
    frame.get.open()

    frame.get.visible = true

    clientRef ! utils.ClientMessages.DuringGameViewReady(ctx.self)

    waitingGameCreated()
  }

  def waitingGameCreated(): Behavior[Message] = {
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
