package view.lobbyphase.actors

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import com.typesafe.config.ConfigFactory
import model.Game
import utils.{Message, ViewMessages}
import view.lobbyphase.{InitialPhaseMainFrame, ViewApplication}
import view.*
import view.lobbyphase.components.{IWaitingToStartListener, WaitingFrame}


object InitialPhaseViewActor:

  case class ViewCreated() extends Message

  case class RestartView() extends Message

  private case class ViewEndCreation(mainFrame: InitialPhaseMainFrame) extends Message


  def apply(whoToSendResponse: ActorRef[Message]): Behavior[Message] =
    Behaviors.setup { (ctx: ActorContext[Message]) =>
      ctx.log.info("InitialPhaseViewActor started")
      //      ViewApplication.startView(ViewActorListener(whoToSendResponse), afterCreation = frame => {
      //        println("Creating view of InitialPhaseViewActor")
      //        ctx.self ! ViewEndCreation(frame)
      //      })
      startInitialViewBehavior(ctx, whoToSendResponse)
    }


  private def idle(
                    frame: InitialPhaseMainFrame,
                    whoToSendResponse: ActorRef[Message],
                  ): Behavior[Message] =
    println("Enter in idle")
    Behaviors.receivePartial {
      handleGameCreated(frame, whoToSendResponse, lobbyWaitingRoom)
        .orElse(handleGameListFromServer(frame, whoToSendResponse, idle))
        .orElse(handlePositiveGameJoinedAnswer(frame, whoToSendResponse, lobbyWaitingRoom))
      //        .orElse(handleNegativeGameJoinedAnswer(frame, whoToSendResponse, idle))
    }

  private def lobbyWaitingRoom(frame: WaitingFrame, whoToSendResponse: ActorRef[Message]): Behavior[Message] =
    Behaviors.receivePartial {
      handleGameUpdate(frame, whoToSendResponse, lobbyWaitingRoom)
        //        .orElse(handlePlayerRequestToJoinTheGame(frame, whoToSendResponse, lobbyWaitingRoom))
        .orElse(handleFailedToPublishToServer(frame, whoToSendResponse, lobbyWaitingRoom))
        .orElse(handleGameAborted(frame, whoToSendResponse, lobbyWaitingRoom))
        .orElse(handleRestartView(frame, whoToSendResponse, idle))
        .orElse({
          case _ => Behaviors.same
        })
      //        .orElse(handleGameStarted(infoInLobby))
    }

  // handlers for messages from View
  private def startInitialViewBehavior(ctx: ActorContext[Message], whoToSendResponse: ActorRef[Message]): Behavior[Message] =
    ViewApplication.startView(ViewActorListener(whoToSendResponse), afterCreation = frame => {
      println("Creating view of InitialPhaseViewActor")
      ctx.self ! ViewEndCreation(frame)
    })
    // TODO: delete remove this than -AAA- decide if wait a message to create view or create it directly
    Behaviors.receivePartial {
      case (ctx, ViewEndCreation(frame)) =>
        ctx.log.info("Inside handleViewEndCreation")
        whoToSendResponse ! ViewCreated()
        idle(
          frame,
          whoToSendResponse,
        )

    }


  // Handlers for messages from Client

  private def handleRestartView(
                                 waitingFrame: WaitingFrame,
                                 whoToSendResponse: ActorRef[Message],
                                 nextBehavior: (InitialPhaseMainFrame, ActorRef[Message]) => Behavior[Message]
                               ):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, RestartView()) =>
      ctx.log.info("Restarting the view to initial state")
      startInitialViewBehavior(ctx, whoToSendResponse)


  private def handleGameCreated(
                                 initialPhaseMainFrame: InitialPhaseMainFrame,
                                 whoToSendResponse: ActorRef[Message],
                                 nextBehavior: (WaitingFrame, ActorRef[Message]) => Behavior[Message]
                               ):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.GameCreated(game)) =>
      ctx.log.info("Game created successfully")
      ctx.log.info(s"Game created at context $ctx")
      initialPhaseMainFrame.dispose()
      //      whoToSendResponse ! ViewMessages.GameCreated(game)
      val waitingFrame = new WaitingFrame(
        new IWaitingToStartListener {
          override def startGame(): Unit =
            initialPhaseMainFrame.viewListener.startGame()

          override def exitFromTheGame(): Unit =
            initialPhaseMainFrame.viewListener.exitFromTheGame()
            ctx.self ! RestartView()
          //            startInitialViewBehavior(ctx, whoToSendResponse)
        },
        game,
        true
      )
      waitingFrame.open()
      nextBehavior(waitingFrame, whoToSendResponse)
  //    case (ctx, some) =>
  //      ctx.log.warn(s"Unhandled message in handleGameCreated: $some")
  //      Behaviors.unhandled

  private def handleFailedToPublishToServer(
                                             waitingFrame: WaitingFrame,
                                             whoToSendResponse: ActorRef[Message],
                                             nextBehavior: (WaitingFrame, ActorRef[Message]) => Behavior[Message]
                                           ):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.FailedToPublishToServer()) =>
      ctx.log.error(s"Failed to publish to server the game created")
      waitingFrame.openErrorPubOnServerDialog()
      nextBehavior(waitingFrame, whoToSendResponse)

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
        new IWaitingToStartListener {
          override def startGame(): Unit = {}

          //            initialPhaseMainFrame.viewListener.startGame()
          override def exitFromTheGame(): Unit =
            initialPhaseMainFrame.viewListener.exitFromTheGame()
            ctx.self ! RestartView()
          //            startInitialViewBehavior(ctx, whoToSendResponse)
        },
        game,
        false
      )
      waitingFrame.open()
      nextBehavior(waitingFrame, whoToSendResponse)

  //  private def handleNegativeGameJoinedAnswer(
  //                                              initialPhaseMainFrame: InitialPhaseMainFrame,
  //                                              whoToSendResponse: ActorRef[Message],
  //                                              nextBehavior: (InitialPhaseMainFrame, ActorRef[Message]) => Behavior[Message]
  //                                            ):
  //  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
  //    case (ctx, ViewMessages.GameJoinedFailed(game)) =>
  //      ctx.log.info(s"Unsuccessfully joined game: $game")
  //      // initialPhaseMainFrame.userFailedToEnterInTheGame(game)
  //      // T ODO: inform the view
  //      // T ODO: change ending behavior
  //      nextBehavior(initialPhaseMainFrame, whoToSendResponse)

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

  private def handleGameAborted(
                                 waitingFrame: WaitingFrame,
                                 whoToSendResponse: ActorRef[Message],
                                 nextBehavior: (WaitingFrame, ActorRef[Message]) => Behavior[Message]
                               ):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.GameAborted()) =>
      ctx.log.info(s"Arrived message that the game has been aborted")
      waitingFrame.dispose()
      startInitialViewBehavior(ctx, whoToSendResponse)

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
  
