package view.gamephase

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import model.Game
import utils.{DuringGameViewMessages, GameCoordinatorMessage, InitialViewMessages, Message}
import utils.DuringGameViewMessages.*

object DuringGameViewActor {
  def apply(
             userID: String,
             clientRef: ActorRef[Message],
             mainMenuRef: ActorRef[InitialViewMessages.ViewCommand]
           ): Behavior[Message] = {
    //    val duringGameMainFrame = new DuringGameMainFrame()
    new DuringGameViewActor(userID, clientRef, mainMenuRef).start()
  }
}

class DuringGameViewActor private(
                                   val userID: String,
                                   val clientRef: ActorRef[Message],
                                   val mainMenuRef: ActorRef[InitialViewMessages.ViewCommand]
                                 ) {

  private case class PropertiesBeforeInitialization(
                                                     frame: DuringGameMainFrame
                                                   )

  private case class PropertiesAfterInitialization(
                                                    gameCoordinatorRef: ActorRef[GameCoordinatorMessage.PlayerCommand],
                                                    frame: DuringGameMainFrame,
                                                    userInterface: IDuringGameInterface
                                                  )
  
  private var lastGameUpdate: Game.GameInProgress = _
  
//  private var cardsSeenQuantity = 0

  def start(): Behavior[Message] = Behaviors.setup { ctx =>
    ctx.log.info("DuringGameViewActor started")
    val frame = new DuringGameMainFrame(DuringGameViewListener(ctx.self))
    frame.open()
    frame.visible = true

    clientRef ! utils.ClientMessages.DuringGameViewReady(ctx.self)

    waitingGameCreated(PropertiesBeforeInitialization(frame))
  }

  private def waitingGameCreated(properties: PropertiesBeforeInitialization): Behavior[Message] = {
    Behaviors.receivePartial {
      handleGameStarted(properties)
        .orElse({
          case msg =>
            println(s"DuringGameViewActor in waitingGameCreated received message: $msg")
            Behaviors.same
        })
    }
  }

  private def watchYourCards(properties: PropertiesAfterInitialization): Behavior[Message] = {
    Behaviors.receivePartial {
      case (ctx, OwnCardSelected(index)) =>
        println(s"DuringGameViewActor in watchYourCards received OwnCardSelected with index: $index")
        properties.gameCoordinatorRef ! GameCoordinatorMessage.ShowYourNthCard(index)
        waitCardSelected(properties)
    }
  }
  
  private def waitCardSelected(properties: PropertiesAfterInitialization): Behavior[Message] = {
    Behaviors.receivePartial {
      case (ctx, CardSeen(card)) =>
        println(s"DuringGameViewActor in waitCardSelected received Card seen with card: $card")
        properties.userInterface.showYourNthCard(card)
        watchYourCards(properties)
    }
  }

  // HANDLERS

  private def handleGameStarted(properties: PropertiesBeforeInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, StartGame(game, gameCoordinatorRef)) =>
      ctx.log.info(s"DuringGameViewActor handling game started with message: ${StartGame(game, gameCoordinatorRef)}")
      val userInterface = properties.frame.startGame(game, userID)
      lastGameUpdate = game
      watchYourCards(PropertiesAfterInitialization(gameCoordinatorRef, properties.frame, userInterface))
  }
}
