package view.gamephase.actors

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import model.{Card, Game, Power, TurnLog}
import utils.DuringGameViewMessages.*
import utils.{DuringGameViewMessages, InitialViewMessages, Message, GameCoordinatorMessage as GCMsg}
import view.gamephase.{DuringGameMainFrame, IDuringGameInterface}

object DuringGameViewActor {
  def apply(
             userID: String,
             clientRef: ActorRef[Message],
             mainMenuRef: ActorRef[InitialViewMessages.ViewCommand]
           ): Behavior[Message] = {
    Behaviors.setup { ctx =>
      new DuringGameViewActor(ctx, userID, clientRef, mainMenuRef).start()
    }
  }
}

private class DuringGameViewActor private(
                                           val ctx: ActorContext[Message],
                                           val userID: String,
                                           val clientRef: ActorRef[Message],
                                           val mainMenuRef: ActorRef[InitialViewMessages.ViewCommand]
                                         ) {

  private case class GameContext(
                                  coordinator: ActorRef[GCMsg.GameCoordinatorMessage],
                                  frame: DuringGameMainFrame,
                                  ui: IDuringGameInterface
                                )

  private case class AdversaryCardRequested(
                                             adversaryID: String,
                                             adversaryCardIndex: Int
                                           )

  private case class ExchangeState(
                                    ownCardIndex: Option[Int] = None,
                                    adversarySelection: Option[AdversaryCardRequested] = None
                                  ) {
    def isComplete: Boolean = ownCardIndex.isDefined && adversarySelection.isDefined
  }

  private var hasDrawnFromDeck: Boolean = false

  // MY TURN BEHAVIORS - START
  // TODO: add the exit from game behavior
  def start(): Behavior[Message] = Behaviors.setup { ctx =>
    ctx.log.info("DuringGameViewActor started")
    val frame = new DuringGameMainFrame(DuringGameViewListener(ctx.self))
    frame.open()
    frame.visible = true

    clientRef ! utils.ClientMessages.DuringGameViewReady(ctx.self)

    Behaviors.receivePartial {
      case (ctx, StartGame(game, gameCoordinatorRef)) =>
        ctx.log.info(s"DuringGameViewActor of player ${userID}, my ref is ${ctx.self}")
        ctx.log.info(s"DuringGameViewActor of player $userID handling game started with message: ${StartGame(game, gameCoordinatorRef)}")
        val userInterface = frame.startGame(game, userID)
        userInterface.enterRevealingInitialCardsPhase()
        watchYourCards(GameContext(gameCoordinatorRef, frame, userInterface))
      case msg =>
        println(s"DuringGameViewActor of player $userID in waitingGameCreated received message: $msg")
        Behaviors.same

    }
  }

  private def watchYourCards(properties: GameContext): Behavior[Message] = {
    Behaviors.receivePartial {
      handleShowCard(properties, watchYourCards)
        .orElse(handleAdversariesRevealingLog(properties))
        .orElse({
          case (ctx, WaitAfterRevealingSection()) => {
            ctx.log.info(s"DuringGameViewActor of player $userID handling message: ${WaitAfterRevealingSection()}")
            properties.ui.enterWaitingPhase()
            waitFirstTurn(properties)
          }
          case msg =>
            println(s"DuringGameViewActor of player $userID in watchYourCards received message: $msg")
            Behaviors.same
        })
    }
  }

  private def waitFirstTurn(properties: GameContext): Behavior[Message] = {
    Behaviors.receivePartial {
      handleAdversariesRevealingLog(properties)
        .orElse(handleExitSelected(properties))
        .orElse({
          case (ctx, StartTurnPlayer(playerID)) =>
            ctx.log.info(s"DuringGameViewActor of player $userID handling message: ${StartTurnPlayer(playerID)}")
            properties.ui.updatePlayerWhoIsPlaying(playerID)
            if playerID == this.userID then
              myTurnBeforeDraw(properties)
            else
              waitMyTurn(properties)
          case (ctx, msg) =>
            ctx.log.info(s"DuringGameViewActor of player $userID in waitFirstTurn received unexpected message: $msg")
            Behaviors.same
        })
    }
  }

  private def waitMyTurn(properties: GameContext): Behavior[Message] = {
    Behaviors.receivePartial {
      handleUpdateLastTurnPlayed(properties)
        .orElse(handleExitSelected(properties))
        .orElse({
          case (ctx, StartTurnPlayer(playerID)) =>
            ctx.log.info(s"DuringGameViewActor of player $userID handling StartTurnPlayer with message: ${StartTurnPlayer(playerID)}")
            properties.ui.updatePlayerWhoIsPlaying(playerID)
            Behaviors.same
          case (ctx, GameEndedByCabo(game)) =>
            ctx.log.info(s"DuringGameViewActor of player $userID handling GameEnded with message: ${GameEndedByCabo(game)}")
            properties.ui.gameEndedByCabo(game)
            waitingCloseGameFrame(properties)
          case (ctx, GameEndedByTurnsLimit(game)) =>
            ctx.log.info(s"DuringGameViewActor of player $userID handling GameEnded with message: ${GameEndedByTurnsLimit(game)}")
            properties.ui.gameEndedByTurns(game)
            waitingCloseGameFrame(properties)
          case (ctx, GameEndedByEmptyDeck(game)) =>
            ctx.log.info(s"DuringGameViewActor of player $userID handling GameEnded with message: ${GameEndedByEmptyDeck(game)}")
            properties.ui.gameEndedByEmptyDeck(game)
            waitingCloseGameFrame(properties)
          case msg =>
            println(s"DuringGameViewActor of player $userID in waitMyTurn received message: $msg")
            Behaviors.same
        })
    }
  }

  private def waitingCloseGameFrame(gameContext: GameContext): Behavior[Message] = Behaviors.receivePartial {
    handleConsultingResultsEnded(gameContext)
      .orElse(handleExitSelected(gameContext))
      .orElse {
        case (ctx, msg) =>
          println(s"DuringGameViewActor of player $userID in waitingCloseGameFrame received message: $msg")
          Behaviors.same
      }

  }

  private def myTurnBeforeDraw(properties: GameContext): Behavior[Message] = {
    properties.ui.startTurn()
    properties.ui.updatePlayerWhoIsPlaying(userID)
    Behaviors.receivePartial {
      handleExitSelected(properties)
        .orElse(handleEndTurn(properties))
        .orElse({
          case (ctx, DeckSelected()) =>
            println(s"DuringGameViewActor of player $userID handling DeckSelected with message: ${DeckSelected()}")
            properties.coordinator ! GCMsg.DrawCardFromDeck()
            hasDrawnFromDeck = true
            myTurnWaitDrawnCard(properties)
          case (ctx, DiscardStackSelected()) =>
            println(s"DuringGameViewActor of player $userID handling DiscardStackSelected with message: ${DiscardStackSelected()}")
            properties.coordinator ! GCMsg.DrawCardFromDiscardStack()
            hasDrawnFromDeck = false
            myTurnWaitDrawnCard(properties)
        })
    }
  }

  private def myTurnWaitDrawnCard(properties: GameContext): Behavior[Message] = {
    Behaviors.receivePartial {
      handleNewTopDiscardCard(properties)
        .orElse(handleExitSelected(properties))
        .orElse(handleEndTurn(properties))
        .orElse({
          case (ctx, CardDrawn(card)) =>
            ctx.log.info(s"DuringGameViewActor of player $userID handling CardDrawn with message: ${CardDrawn(card)}")
            if hasDrawnFromDeck then
              properties.ui.showCardDrawnFromDeck(card)
              card.power match
                case Power.SeeYourCard() => myTurnPowerSeeMyCard(properties)
                case Power.SeeYourOpponentCard() => myTurnPowerSeeOpponentCard(properties)
                case Power.ChangeOneOfYourCardWithOpponent() =>
                  properties.ui.usePowerToExchangeCardWithAdversary()
                  myTurnPowerExchange(properties, ExchangeState())
                case _ => myTurnAfterDraw(properties)
            else
              properties.ui.showCardDrawnFromDiscards(card)
              myTurnAfterDraw(properties)
        })
    }
  }

  private def myTurnAfterDraw(
                               properties: GameContext
                             ): Behavior[Message] = {
    properties.ui.afterDrawPhase(hasDrawnFromDeck)
    Behaviors.receivePartial {
      handleNewTopDiscardCard(properties)
        .orElse(handleExitSelected(properties))
        .orElse(handleEndTurn(properties))
        .orElse({
          case (ctx, DiscardCardDrawnSelected()) if hasDrawnFromDeck =>
            println(s"DuringGameViewActor of player $userID in myTurnAfterDraw received DiscardCardDrawn")
            properties.coordinator ! GCMsg.DiscardCardDrawn()
            properties.ui.afterDiscarded()
            myTurnAfterDiscard(properties)
          case (ctx, OwnCardSelected(index)) =>
            println(s"DuringGameViewActor of player $userID in myTurnAfterDraw received OwnCardSelected with index: $index")
            properties.coordinator ! GCMsg.DiscardYourNthCard(index)
            properties.ui.afterDiscarded()
            myTurnAfterDiscard(properties)
        })
    }
  }


  private def myTurnPowerSeeMyCard(properties: GameContext): Behavior[Message] = {
    properties.ui.usePowerToSeeOwnCard()
    Behaviors.receivePartial {
      handleShowCard(properties, myTurnAfterDraw)
        .orElse(handleExitSelected(properties))
        .orElse(handleEndTurn(properties))
    }
  }

  private def myTurnPowerSeeOpponentCard(properties: GameContext): Behavior[Message] = {
    properties.ui.usePowerToSeeAdversaryCard()
    Behaviors.receivePartial {
      handleEndTurn(properties)
        .orElse(handleExitSelected(properties))
        .orElse({
          case (ctx, AdversaryCardSelected(adversaryID, index)) =>
            println(s"DuringGameViewActor of player $userID in myTurnPowerSeeOpponentCard received AdversaryCardSelected with index: $index")
            properties.coordinator ! GCMsg.ShowAdversaryNthCard(adversaryID, index)
            waitAdversaryCardSelected(properties, AdversaryCardRequested(adversaryID, index))
        })
    }
  }

  private def myTurnPowerExchange(data: GameContext, state: ExchangeState): Behavior[Message] = {
    if (state.isComplete) {
      val adv = state.adversarySelection.get
      data.coordinator ! GCMsg.ReplaceOwnNthCardWithAdversaryNthOne(state.ownCardIndex.get, adv.adversaryID, adv.adversaryCardIndex)
      return myTurnWaitPowerChangeAck(data)
    }
    Behaviors.receivePartial {
      handleExitSelected(data)
        .orElse({
          case (ctx, AdversaryCardSelected(adversaryID, index)) if state.adversarySelection.isEmpty =>
            data.ui.activateAdversariesCards(false)
            data.ui.notifyYourAdversaryCardSelection(adversaryID, index)
            myTurnPowerExchange(data, state.copy(adversarySelection = Some(AdversaryCardRequested(adversaryID, index))))

          case (ctx, OwnCardSelected(ownIndex)) if state.ownCardIndex.isEmpty =>
            data.ui.activateOwnCards(false)
            data.ui.notifyYourOwnCardSelection(ownIndex)
            myTurnPowerExchange(data, state.copy(ownCardIndex = Some(ownIndex)))
        }
        )
    }
  }

  private def myTurnWaitPowerChangeAck(properties: GameContext): Behavior[Message] = {
    Behaviors.receivePartial {
      handleEndTurn(properties)
        .orElse(handleExitSelected(properties))
        .orElse({
          case (ctx, DuringGameViewMessages.ChangeCardWithAdversaryAck()) =>
            println(s"DuringGameViewActor of player $userID in myTurnWaitPowerChangeAck received ChangeCardWithAdversaryAck")
            properties.ui.changeCardWithAdversaryIsDone()
            myTurnAfterDraw(properties)
        })
    }
  }

  private def waitCardSelected(
                                properties: GameContext,
                                behaviorAfterCardReceived: GameContext => Behavior[Message]
                              ): Behavior[Message] = {
    Behaviors.receivePartial {
      handleAdversariesRevealingLog(properties)
        .orElse(handleEndTurn(properties))
        .orElse(handleExitSelected(properties))
        .orElse({
          case (ctx, CardSeen(card)) =>
            println(s"DuringGameViewActor of player $userID in waitCardSelected received Card seen with card: $card")
            properties.ui.showYourNthCard(card)
            behaviorAfterCardReceived(properties)
          case (ctx, msg) =>
            ctx.log.info(s"DuringGameViewActor ID $userID in waitCardSelected received unexpected message: $msg")
            Behaviors.same
        })
    }
  }

  private def waitAdversaryCardSelected(properties: GameContext, advCRequested: AdversaryCardRequested): Behavior[Message] = {
    Behaviors.receivePartial {
      handleEndTurn(properties)
        .orElse(handleExitSelected(properties))
        .orElse({
          case (ctx, CardSeen(card)) =>
            println(s"DuringGameViewActor of player $userID in waitAdversaryCardSelected received: ${CardSeen(card)}")
            properties.ui.showAdversaryNthCard(advCRequested.adversaryID, advCRequested.adversaryCardIndex, card)
            myTurnAfterDraw(properties)
        })
    }
  }

  private def myTurnAfterDiscard(properties: GameContext): Behavior[Message] = {
    hasDrawnFromDeck = false
    Behaviors.receivePartial {
      handleNewTopDiscardCard(properties)
        .orElse(handleExitSelected(properties))
        .orElse(handleEndTurn(properties))
        .orElse({
          case (ctx, CallCaboSelected()) =>
            ctx.log.info(s"DuringGameViewActor of player $userID in myTurnAfterDiscard received CallCaboSelected")
            properties.ui.enterWaitingPhase()
            properties.coordinator ! GCMsg.CallCabo()
            waitMyTurn(properties)
        })
    }
  }
  // MY TURN BEHAVIORS - END

  // HANDLERS

  private def handleShowCard(
                              properties: GameContext,
                              behaviorAfterWatched: GameContext => Behavior[Message]):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, OwnCardSelected(index)) =>
      ctx.log.info(s"DuringGameViewActor of player ${userID}, my ref is ${ctx.self}")
      println(s"DuringGameViewActor HANDLER handleWatchYourCards received OwnCardSelected with index: $index")
      properties.coordinator ! GCMsg.ShowYourNthCard(index)
      waitCardSelected(properties, behaviorAfterWatched)
  }

  private def handleUpdateLastTurnPlayed(properties: GameContext):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, LastTurnPlayed(turnLog, game, isMyTurn)) =>
      ctx.log.info(s"DuringGameViewActor of player $userID handling LastTurnPlayed with isMyTurn = $isMyTurn}")
      ctx.log.info(s"DuringGameViewActor of player $userID handling LastTurnPlayed with turnLog =\n$turnLog")
      ctx.log.info(s"DuringGameViewActor of player $userID handling LastTurnPlayed with game =\n$game")
      properties.ui.updateLastTurnLog(turnLog)
      properties.ui.updateGameInfo(game)
      properties.ui.updateDiscardsTopCard(game.discardDeckStack.cards.head)
      if isMyTurn then myTurnBeforeDraw(properties) else waitMyTurn(properties)
  }

  private def handleNewTopDiscardCard(properties: GameContext):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, NewTopCardDiscardStack(card)) =>
      ctx.log.info(s"DuringGameViewActor of player $userID handling NewTopDiscardCard with message: ${NewTopCardDiscardStack(card)}")
      properties.ui.updateDiscardsTopCard(card)
      Behaviors.same
    case (ctx, EmptyDiscardStack()) =>
      ctx.log.info(s"DuringGameViewActor of player $userID handling EmptyDiscardStack with message: ${EmptyDiscardStack()}")
      properties.ui.emptyDiscardStack()
      Behaviors.same
  }

  private def handleExitSelected(properties: GameContext):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, ExitSelected()) =>
      println(s"DuringGameViewActor of player $userID handling ExitSelected() with message: ${ExitSelected()}")
      properties.frame.dispose()
      clientRef ! utils.ClientMessages.LeaveTheGame()
      // TODO: add exit to game coordinator x
      //        properties.gameCoordinatorRef ! GCMsg.Exit()
      Behaviors.stopped
  }

  private def handleConsultingResultsEnded(properties: GameContext):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, ConsultingResultsEnded()) =>
      println(s"DuringGameViewActor of player $userID handling ConsultingResultsEnded() with message: ${ConsultingResultsEnded()}")
      properties.frame.dispose()
      //      mainMenuRef ! InitialViewMessages.ReturnToMainMenuFromGame(userID)
      clientRef ! utils.ClientMessages.GameEnded()
      Behaviors.stopped
  }

  private def handleAdversariesRevealingLog(properties: GameContext):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, RevealingCardsPhaseAdversaryLog(revealingLog)) =>
      ctx.log.info(s"DuringGameViewActor of player $userID handling RevealingCardsPhaseAdversaryLog with message: ${RevealingCardsPhaseAdversaryLog(revealingLog)}")
      properties.ui.updateRevealingLog(revealingLog)
      Behaviors.same
  }

  private def handleEndTurn(properties: GameContext):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, EndTurnSelected()) =>
      ctx.log.info(s"DuringGameViewActor of player $userID in myTurn received EndTurn")
      properties.ui.enterWaitingPhase()
      properties.coordinator ! GCMsg.EndTurn()
      waitMyTurn(properties)
  }

}