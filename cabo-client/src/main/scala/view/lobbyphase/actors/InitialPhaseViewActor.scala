package view.lobbyphase.actors

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import com.typesafe.config.ConfigFactory
import model.Game
import utils.{Message, ViewMessages}
import view.*
import view.lobbyphase.{IViewListener, InitialPhaseMainFrame, ViewApplication}
import view.*
import view.lobbyphase.components.WaitingFrame


object InitialPhaseViewActor:

  case class ViewCreated() extends Message

  private case class ViewEndCreation(mainFrame: InitialPhaseMainFrame) extends Message


  def apply(whoToSendResponse: ActorRef[Message]): Behavior[Message] =

    Behaviors.setup { ctx =>
      // TODO: delete remove this than -AAA- decide if wait a message to create view or create it directly
      ViewApplication.startView(ViewActorListener(whoToSendResponse), afterCreation = frame => {
        //      ViewApplication.startView(myViewListener, afterCreation = frame => {
        ctx.self ! ViewEndCreation(frame)
      })
      Behaviors.receivePartial {
        case (ctx, ViewEndCreation(frame)) =>
          ctx.log.info("Inside handleViewEndCreation")
          whoToSendResponse ! ViewCreated()
          //          idle(ViewActorInfoInitPhase(frame, whoToSendResponse))
          idle(
            frame,
            whoToSendResponse,
          )

      }
    }

  private def idle(
                    frame: InitialPhaseMainFrame,
                    whoToSendResponse: ActorRef[Message],
                  ): Behavior[Message] =
    Behaviors.receivePartial {
      handleGameCreated(frame, whoToSendResponse, lobbyWaitingRoom)
        //        .orElse(handleFailedToPublishToServer(infoInIdle))
        .orElse(handleGameListFromServer(frame, whoToSendResponse, idle))
        .orElse(handlePositiveGameJoinedAnswer(frame, whoToSendResponse, lobbyWaitingRoom))
        .orElse(handleNegativeGameJoinedAnswer(frame, whoToSendResponse, idle))
      //        .orElse(handleGameUpdate(infoInIdle))
      //        .orElse(handleGameStarted(infoInIdle))
    }

  private def lobbyWaitingRoom(frame: WaitingFrame, whoToSendResponse: ActorRef[Message]): Behavior[Message] =
    Behaviors.receivePartial {
      handlePlayerRequestToJoinTheGame(frame, whoToSendResponse, lobbyWaitingRoom)
        .orElse(handleGameUpdate(frame, whoToSendResponse, lobbyWaitingRoom))
        .orElse({
          case _ => Behaviors.same
        })
      //      handleGameCreated(infoInLobby)
      //        .orElse(handleFailedToPublishToServer(infoInLobby))
      //        .orElse(handleGameListFromServer(infoInLobby))
      //        .orElse(handlePositiveGameJoinedAnswer(infoInLobby))
      //        .orElse(handleGameStarted(infoInLobby))
    }
  // handlers for messages from View

  // Handlers for messages from Client  

  private def handleGameCreated(
                                 initialPhaseMainFrame: InitialPhaseMainFrame,
                                 whoToSendResponse: ActorRef[Message],
                                 nextBehavior: (WaitingFrame, ActorRef[Message]) => Behavior[Message]
                               ):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.GameCreated(game)) =>
      ctx.log.info("Game created successfully")
      initialPhaseMainFrame.dispose()
      //      whoToSendResponse ! ViewMessages.GameCreated(game)
      val waitingFrame = new WaitingFrame(
        initialPhaseMainFrame.viewListener,
        game.players, // TODO: pass the list of players
        true // TODO: select base on host
      )
      waitingFrame.open()
      // TODO: create waiting frame
      nextBehavior(waitingFrame, whoToSendResponse)

  //  private def handleFailedToPublishToServer(info: ViewActorInfoWaitingRoom):
  //  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
  //    case (ctx, ViewMessages.FailedToPublishToServer()) =>
  //      ctx.log.error(s"Failed to publish to server the game created")
  //      info.frame.failedToPublishToServer()
  //      info.nextBehavior(info)
  //
  private def handleGameListFromServer(
                                        initialPhaseMainFrame: InitialPhaseMainFrame,
                                        whoToSendResponse: ActorRef[Message],
                                        nextBehavior: (InitialPhaseMainFrame, ActorRef[Message]) => Behavior[Message]
                                      ):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.GameList(games)) =>
      initialPhaseMainFrame.updateGameList(games)
      ctx.log.info(s"Received game list from server: $games")
      nextBehavior(initialPhaseMainFrame, whoToSendResponse)

  private def handlePositiveGameJoinedAnswer(
                                              initialPhaseMainFrame: InitialPhaseMainFrame,
                                              whoToSendResponse: ActorRef[Message],
                                              nextBehavior: (WaitingFrame, ActorRef[Message]) => Behavior[Message]
                                            ):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.GameJoined(game)) =>
      ctx.log.info(s"Successfully joined game: $game")
      initialPhaseMainFrame.dispose()
      val waitingFrame = new WaitingFrame(
        initialPhaseMainFrame.viewListener,
        game.players, // TODO: pass the list of players
        false
      )
      waitingFrame.open()
      nextBehavior(waitingFrame, whoToSendResponse)

  private def handleNegativeGameJoinedAnswer(
                                              initialPhaseMainFrame: InitialPhaseMainFrame,
                                              whoToSendResponse: ActorRef[Message],
                                              nextBehavior: (InitialPhaseMainFrame, ActorRef[Message]) => Behavior[Message]
                                            ):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.GameJoinedFailed(game)) =>
      ctx.log.info(s"Unsuccessfully joined game: $game")
      // initialPhaseMainFrame.userFailedToEnterInTheGame(game)
      // TODO: inform the view
      // TODO: change ending behavior
      nextBehavior(initialPhaseMainFrame, whoToSendResponse)

  //
  private def handleGameUpdate(
                                waitingFrame: WaitingFrame,
                                whoToSendResponse: ActorRef[Message],
                                nextBehavior: (WaitingFrame, ActorRef[Message]) => Behavior[Message]
                              ):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.GameInfoUpdate(game)) =>
      ctx.log.info(s"Arrived new info about the game: $game")
      waitingFrame.updatePlayersList(game.players)
      nextBehavior(waitingFrame, whoToSendResponse)
  //
  //  private def handleGameStarted(info: ViewActorInfoWaitingRoom):
  //  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
  //    case (ctx, ViewMessages.GameStarted()) =>
  //      ctx.log.info(s"Received message to start the game: GameStarted")
  //      // TODO: inform the view to start the game
  //      // TODO: change ending behavior
  //      Behaviors.same

  private def handlePlayerRequestToJoinTheGame(
                                                waitingFrame: WaitingFrame,
                                                whoToSendResponse: ActorRef[Message],
                                                nextBehavior: (WaitingFrame, ActorRef[Message]) => Behavior[Message]
                                              ):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.PlayerRequestedToJoinGame(player)) =>
      ctx.log.info(s"Player requested to join the game: $player")
      waitingFrame.playerHasRequestedToJoinTheGame(player)
      nextBehavior(waitingFrame, whoToSendResponse)

// TODO: delete remove this than -AAA- remove this in deploy phase
// to use rename application.conf to something in common resources.
// This allow to run the test without a full application configuration.
@main def runViewActorTest(): Unit =
  import akka.actor.typed.ActorSystem

  object ApplicationRootActor:
    def apply(): Behavior[Message] = Behaviors.setup { ctx =>
      ctx.log.info("ApplicationRootActor: Avvio...")
      ctx.spawn(InitialPhaseViewActor(ctx.self), "ViewActor")
      Behaviors.receive { (context, message) =>
        println("Received message in ApplicationRootActor: " + message)
        Behaviors.same
      }
    }
  println("Avvio test del ActorSystem...")
  val system: ActorSystem[Message] = ActorSystem(ApplicationRootActor(), "ViewActorTestSystem", ConfigFactory.empty())
  
