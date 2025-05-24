package view.actors

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import com.typesafe.config.ConfigFactory
import model.Game
import utils.{Message, ViewMessages}
import view.*
import view.ui.*


object ViewActor:

  case class ViewCreated() extends Message

  private case class ViewEndCreation(mainFrame: InitialPhaseMainFrame) extends Message

  private case class ViewActorInfo(frame: InitialPhaseMainFrame, whoToSendResponse: ActorRef[Message], nextBehavior: Behavior[Message]) extends Message

  def apply(whoToSendResponse: ActorRef[Message]): Behavior[Message] = Behaviors.setup { ctx =>
    // TODO: delete remove this than -AAA- decide if wait a message to create view or create it directly
    ViewApplication.startView(ViewActorListener(whoToSendResponse), afterCreation = frame => {
      ctx.self ! ViewEndCreation(frame)
    })
    Behaviors.receivePartial {
      case (ctx, ViewEndCreation(frame)) =>
        ctx.log.info("Inside handleViewEndCreation")
        whoToSendResponse ! ViewCreated()
        idle(ViewActorInfo(frame, whoToSendResponse, Behaviors.same))

    }
  }

  private def idle(info: ViewActorInfo): Behavior[Message] =
    val infoInIdle = info.copy(nextBehavior = idle(info))
    Behaviors.receivePartial {
      handleFailedToPublishToServer(infoInIdle)
        .orElse(handleGameListFromServer(infoInIdle))
        .orElse(handleGameJoinedAnswer(infoInIdle))
        .orElse(handleGameUpdate(infoInIdle))
        .orElse(handleGameStarted(infoInIdle))
    }

  private def handleFailedToPublishToServer(info: ViewActorInfo):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.FailedToPublishToServer()) =>
      ctx.log.error(s"Failed to publish to server the game created")
      info.frame.failedToPublishToServer()
      info.nextBehavior

  private def handleGameListFromServer(info: ViewActorInfo):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.GameList(games)) =>
      ctx.log.info(s"Received game list from server: $games")
      info.frame.updateGameList(games)
      info.nextBehavior

  private def handleGameJoinedAnswer(info: ViewActorInfo):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.GameJoined(game)) =>
      ctx.log.info(s"Successfully joined game: $game")
      info.frame.userIsEnteredInTheGame(game)
      // TODO: change ending behavior
      // waiting start game
      Behaviors.same
    case (ctx, ViewMessages.GameJoinedFailed(game)) =>
      ctx.log.info(s"Unsuccessfully joined game: $game")
      info.frame.userFailedToEnterInTheGame(game)
      // TODO: inform the view
      // TODO: change ending behavior
      Behaviors.same

  private def handleGameUpdate(info: ViewActorInfo):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.GameInfoUpdate(game)) =>
      ctx.log.info(s"Arrived new info about the game: $game")
      // TODO: inform the view
      // TODO: change ending behavior
      Behaviors.same

  private def handleGameStarted(info: ViewActorInfo):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, ViewMessages.GameStarted()) =>
      ctx.log.info(s"Received message to start the game: GameStarted")
      // TODO: inform the view to start the game
      // TODO: change ending behavior
      Behaviors.same


// TODO: delete remove this than -AAA- remove this in deploy phase
// to use rename application.conf to something in common resources.
// This allow to run the test without a full application configuration.
@main def runViewActorTest(): Unit =
  import akka.actor.typed.ActorSystem

  object ApplicationRootActor:
    def apply(): Behavior[Message] = Behaviors.setup { ctx =>
      ctx.log.info("ApplicationRootActor: Avvio...")
      ctx.spawn(ViewActor(ctx.self), "ViewActor")
      Behaviors.receive { (context, message) =>
        println("Received message in ApplicationRootActor: " + message)
        Behaviors.same
      }
    }
  println("Avvio test del ActorSystem...")
  val system: ActorSystem[Message] = ActorSystem(ApplicationRootActor(), "ViewActorTestSystem", ConfigFactory.empty())
  
