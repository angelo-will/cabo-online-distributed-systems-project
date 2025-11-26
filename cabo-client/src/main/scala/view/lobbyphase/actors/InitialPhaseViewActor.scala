package view.lobbyphase.actors

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import model.Game
import utils.{InitialViewMessages, Message}
import utils.InitialViewMessages.WhoToSendResponse
import view.lobbyphase.components.{IWaitingToStartListener, WaitingFrame}
import view.lobbyphase.{InitialPhaseMainFrame, ViewApplication}

object InitialPhaseViewActor {

  case class ViewCreated() extends Message

  case class RestartView() extends Message

  def apply(ref: ActorRef[Message], playerName: String): Behavior[Message] = {
    Behaviors.setup { ctx => {
      ctx.log.info(s"InitialPhaseViewActor started for player: $playerName")
      Behaviors.receiveMessage {
        case WhoToSendResponse(clientRef) => {
          ctx.log.info("Client reference received. Starting View Logic.")
          new InitialPhaseViewLogic(ctx, clientRef, playerName).startViewCreation()
        }
        case other => {
          ctx.log.error(s"Unexpected message while waiting for client ref: $other")
          Behaviors.same
        }
      }
    }
    }
  }

  private class InitialPhaseViewLogic(
                                       ctx: ActorContext[Message],
                                       clientRef: ActorRef[Message],
                                       playerName: String
                                     ) {

    import InitialPhaseViewActor.*

    private case class ViewEndCreation(mainFrame: InitialPhaseMainFrame) extends Message

    def startViewCreation(): Behavior[Message] = {
      ViewApplication.startView(
        ViewActorListener(clientRef),
        playerName,
        afterCreation = frame => {
          println("View constructed, sending ViewEndCreation to self")
          ctx.self ! ViewEndCreation(frame)
        }
      )

      Behaviors.receiveMessage {
        case ViewEndCreation(frame) =>
          ctx.log.info("View creation completed. Transitioning to IDLE.")
          clientRef ! ViewCreated()
          idle(frame)

        case other =>
          ctx.log.warn(s"Received unexpected message during view creation: $other")
          Behaviors.same
      }
    }

    private def idle(frame: InitialPhaseMainFrame): Behavior[Message] = {
      Behaviors.receiveMessage {

        case InitialViewMessages.GameList(games) => {
          ctx.log.info(s"Received game list: ${games.size} games")
          frame.updateGameList(games)
          Behaviors.same
        }
        case InitialViewMessages.GameCreated(game) => {
          ctx.log.info(s"Game created: ${game.code}. Switching to Waiting Room (Host).")
          frame.dispose()
          val waitingFrame = createWaitingFrame(game, isHost = true,
            new IWaitingToStartListener {
              override def startGame(): Unit =
                frame.viewListener.startGame()

              override def exitFromTheGame(): Unit =
                frame.viewListener.exitFromTheGame()
                ctx.self ! RestartView()
            })
          waitingFrame.open()
          waiting(waitingFrame)
        }

        case InitialViewMessages.GameJoined(game) => {
          ctx.log.info(s"Joined game: ${game.code}. Switching to Waiting Room (Guest).")
          frame.dispose()
          val waitingFrame = createWaitingFrame(game, isHost = false,
            new IWaitingToStartListener {
              override def startGame(): Unit = {}

              override def exitFromTheGame(): Unit =
                frame.viewListener.exitFromTheGame()
                ctx.self ! RestartView()
            })
          waitingFrame.open()
          waiting(waitingFrame)
        }
        case RestartView() => {
          ctx.log.info("Restart requested while in IDLE. Reloading view.")
          frame.dispose()
          startViewCreation()
        }
        case other => {
          ctx.log.warn(s"Unhandled message in IDLE: $other")
          Behaviors.same
        }
      }
    }

    private def waiting(frame: WaitingFrame): Behavior[Message] = {
      Behaviors.receiveMessage {

        case InitialViewMessages.GameInfoUpdate(game) =>
          ctx.log.info(s"Update received for game ${game.code}. Updating player list.")
          frame.updatePlayersList(game.players)
          Behaviors.same

        case InitialViewMessages.FailedToPublishToServer() =>
          ctx.log.error("Failed to publish game to server.")
          frame.openErrorPubOnServerDialog()
          Behaviors.same

        case InitialViewMessages.GameAborted() =>
          ctx.log.info("Game aborted. returning to Main Menu.")
          //        frame.dispose()
          frame.hostCancelledTheGame(() => ctx.self ! RestartView())
          //        startViewCreation()

          Behaviors.same

        case InitialViewMessages.GameStarted() =>
          ctx.log.info("Game started. Closing Waiting Room.")
          frame.dispose()
          Behaviors.stopped

        case RestartView() =>
          ctx.log.info("User left or restart requested. Returning to Main Menu.")
          frame.dispose()
          startViewCreation()

        case other =>
          ctx.log.warn(s"Unhandled message in WAITING: $other")
          Behaviors.same
      }
    }


    private def createWaitingFrame(
                                    game: Game.GameInConstruction,
                                    isHost: Boolean,
                                    listener: IWaitingToStartListener
                                  ): WaitingFrame =
      new WaitingFrame(listener, game, isHost)
  }
}
