package view.pregamephase.actors

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import model.Game
import messages.ClientMessages.ClientCommand
import messages.IPreGameViewMessage
import messages.PreGameViewMessages.*

import view.pregamephase.components.{IWaitingToStartListener, LobbyWaitingFrame}
import view.pregamephase.{PreGameMainFrame, ViewApplication}

object PreGameViewActor {

  case class ViewCreated() extends ClientCommand

  private case class RestartView() extends IPreGameViewMessage

  def apply(ref: ActorRef[ClientCommand], playerName: String): Behavior[IPreGameViewMessage] = {
    Behaviors.setup { ctx => {
      ctx.log.info(s"InitialPhaseViewActor started for player: $playerName")
      Behaviors.receiveMessage {
        case WhoToSendResponse(clientRef) => {
          ctx.log.info("Client reference received. Starting View Logic.")
          new PreGameViewBehavior(ctx, clientRef, playerName).startViewCreation()
        }
        case other => {
          ctx.log.error(s"Unexpected message while waiting for client ref: $other")
          Behaviors.same
        }
      }
    }
    }
  }

  private class PreGameViewBehavior(
                                       ctx: ActorContext[IPreGameViewMessage],
                                       clientRef: ActorRef[ClientCommand],
                                       playerName: String
                                     ) {

    import PreGameViewActor.*

    private case class ViewEndCreation(mainFrame: PreGameMainFrame) extends IPreGameViewMessage

    def startViewCreation(): Behavior[IPreGameViewMessage] = {
      ViewApplication.startView(
        PreGameViewListener(clientRef),
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

    private def idle(frame: PreGameMainFrame): Behavior[IPreGameViewMessage] = {
      Behaviors.receiveMessage {

        case GameList(games) => {
          ctx.log.info(s"Received game list: ${games.size} games")
          frame.updateGameList(games)
          Behaviors.same
        }
        case GameCreated(game) => {
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
          waitingLobby(waitingFrame)
        }

        case GameJoined(game) => {
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
          waitingLobby(waitingFrame)
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

    private def waitingLobby(frame: LobbyWaitingFrame): Behavior[IPreGameViewMessage] = {
      Behaviors.receiveMessage {

        case GameInfoUpdate(game) =>
          ctx.log.info(s"Update received for game ${game.code}. Updating player list.")
          frame.updatePlayersList(game.players)
          Behaviors.same

        case FailedToPublishToServer() =>
          ctx.log.error("Failed to publish game to server.")
          frame.openErrorPubOnServerDialog()
          Behaviors.same

        case GameAborted() =>
          ctx.log.info("Game aborted. returning to Main Menu.")
          //        frame.dispose()
          frame.hostCancelledTheGame(() => ctx.self ! RestartView())
          //        startViewCreation()

          Behaviors.same

        case GameStarted() =>
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
                                  ): LobbyWaitingFrame =
      new LobbyWaitingFrame(listener, game, isHost)
  }
}
