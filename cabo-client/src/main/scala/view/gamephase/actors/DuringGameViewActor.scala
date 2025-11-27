package view.gamephase.actors

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import model.{EndGameReason, Game, Power}
import utils.DuringGameViewMessages.*
import utils.{DuringGameViewMessages, InitialViewMessages, Message, GameCoordinatorMessage as GCMsg}
import view.gamephase.DuringGameMainFrame
import view.gamephase.traits.IDuringGameInterface

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

  // --- INITIALIZATION ---

  def start(): Behavior[Message] = Behaviors.setup { _ =>
    log("start", "DuringGameViewActor started")
    val frame = new DuringGameMainFrame(DuringGameViewListener(ctx.self))
    frame.open()
    frame.visible = true

    clientRef ! utils.ClientMessages.DuringGameViewReady(ctx.self)

    Behaviors.receiveMessagePartial {
      case StartGame(game, gameCoordinatorRef) =>
        log("", s"Handling game started with message: ${StartGame(game, gameCoordinatorRef)}")
        val userInterface = frame.startGame(game, userID)
        userInterface.enterRevealingInitialCardsPhase()
        watchYourCards(GameContext(gameCoordinatorRef, frame, userInterface))
      case msg =>
        ctx.log.warn(s"DuringGameViewActor of player $userID in state start received unexpected message: $msg")
        Behaviors.same
    }
  }

  // --- WAITING STATES ---

  private def watchYourCards(context: GameContext): Behavior[Message] = {
    Behaviors.receiveMessagePartial {
      handleShowCard(context, watchYourCards)
        .orElse(handleAdversariesRevealingLog(context))
        .orElse({
          case WaitAfterRevealingSection() =>
            log("", s"Handling message: ${WaitAfterRevealingSection()}")
            context.ui.enterWaitingPhase()
            waitFirstTurn(context)
        })
        .orElse(handleUnexpectedMessage("watchYourCards"))
    }
  }

  private def waitFirstTurn(context: GameContext): Behavior[Message] = {
    Behaviors.receiveMessagePartial {
      handleAdversariesRevealingLog(context)
        .orElse(handleExitSelected(context))
        .orElse(handleStartTurnPlayer(context))
        .orElse(handleUnexpectedMessage("waitFirstTurn"))
    }
  }

  private def waitMyTurn(context: GameContext): Behavior[Message] = {
    Behaviors.receiveMessagePartial {
      handleUpdateLastTurnPlayed(context)
        .orElse(handleExitSelected(context))
        .orElse(handleStartTurnPlayer(context))
        .orElse(handleGameEnding(context))
        .orElse(handleUnexpectedMessage("waitMyTurn"))
    }
  }

  private def waitingCloseGameFrame(context: GameContext): Behavior[Message] = {
    Behaviors.receiveMessagePartial {
      handleConsultingResultsEnded(context)
        .orElse(handleExitSelected(context))
        .orElse(handleUnexpectedMessage("waitingCloseGameFrame"))
    }
  }

  // --- MY TURN BEHAVIORS ---

  private def myTurnBeforeDraw(context: GameContext): Behavior[Message] = {
    context.ui.startTurn()
    context.ui.updatePlayerWhoIsPlaying(userID)
    Behaviors.receiveMessagePartial {
      handleExitSelected(context)
        .orElse(handleWhichDeckSelectedForDrawing(context))
        .orElse(handleUnexpectedMessage("myTurnBeforeDraw"))
    }
  }

  private def myTurnWaitDrawnCard(context: GameContext): Behavior[Message] = {
    Behaviors.receiveMessagePartial {
      handleCardDrawn(context)
        .orElse(handleNewTopDiscardCard(context))
        .orElse(handleUnexpectedMessage("myTurnWaitDrawnCard"))
        .orElse(handleExitSelected(context))
    }
  }

  private def myTurnAfterDraw(context: GameContext): Behavior[Message] = {
    context.ui.afterDrawPhase(hasDrawnFromDeck)
    Behaviors.receiveMessagePartial {
      handleNewTopDiscardCard(context)
        .orElse(handleExitSelected(context))
        .orElse(handleWhichCardKeep(context))
        .orElse(handleUnexpectedMessage("myTurnAfterDraw"))
    }
  }

  private def myTurnPowerSeeMyCard(context: GameContext): Behavior[Message] = {
    context.ui.usePowerToSeeOwnCard()
    Behaviors.receiveMessagePartial {
      handleShowCard(context, myTurnAfterDraw)
        .orElse(handleExitSelected(context))
        .orElse(handleUnexpectedMessage("myTurnPowerSeeMyCard"))
    }
  }

  private def myTurnPowerSeeOpponentCard(context: GameContext): Behavior[Message] = {
    context.ui.usePowerToSeeAdversaryCard()
    Behaviors.receiveMessagePartial {
      handleExitSelected(context)
        .orElse({
          case AdversaryCardSelected(adversaryID, index) =>
            log("myTurnPowerSeeOpponentCard", s"Received AdversaryCardSelected with index: $index")
            context.coordinator ! GCMsg.ShowAdversaryNthCard(adversaryID, index)
            waitAdversaryCardSelected(context, AdversaryCardRequested(adversaryID, index))
        })
        .orElse(handleUnexpectedMessage("myTurnPowerSeeOpponentCard"))
    }
  }

  private def myTurnPowerExchange(context: GameContext, state: ExchangeState): Behavior[Message] = {
    if (state.isComplete) {
      val adv = state.adversarySelection.get
      context.coordinator ! GCMsg.ReplaceOwnNthCardWithAdversaryNthOne(state.ownCardIndex.get, adv.adversaryID, adv.adversaryCardIndex)
      Behaviors.receiveMessagePartial {
        handleExitSelected(context)
          .orElse({
            case ChangeCardWithAdversaryAck() =>
              log("myTurnWaitPowerChangeAck", "Received ChangeCardWithAdversaryAck")
              context.ui.changeCardWithAdversaryIsDone()
              myTurnAfterDraw(context)
          })
          .orElse(handleUnexpectedMessage("myTurnWaitPowerChangeAck"))
      }
    } else {
      Behaviors.receiveMessagePartial {
        handleExitSelected(context)
          .orElse({
            case AdversaryCardSelected(adversaryID, index) if state.adversarySelection.isEmpty =>
              context.ui.activateAdversariesCards(false)
              context.ui.notifyYourAdversaryCardSelection(adversaryID, index)
              myTurnPowerExchange(context, state.copy(adversarySelection = Some(AdversaryCardRequested(adversaryID, index))))

            case OwnCardSelected(ownIndex) if state.ownCardIndex.isEmpty =>
              context.ui.activateOwnCards(false)
              context.ui.notifyYourOwnCardSelection(ownIndex)
              myTurnPowerExchange(context, state.copy(ownCardIndex = Some(ownIndex)))
          })
          .orElse(handleUnexpectedMessage("myTurnPowerExchange"))
      }
    }
  }

  private def waitCardSelected(
                                context: GameContext,
                                behaviorAfterCardReceived: GameContext => Behavior[Message]
                              ): Behavior[Message] = {
    Behaviors.receiveMessagePartial {
      handleAdversariesRevealingLog(context)
        .orElse(handleExitSelected(context))
        .orElse({
          case CardSeen(card) =>
            log("waitCardSelected", s"Received Card seen with card: $card")
            context.ui.showYourNthCard(card)
            behaviorAfterCardReceived(context)
        })
        .orElse(handleUnexpectedMessage("waitCardSelected"))
    }
  }

  private def waitAdversaryCardSelected(context: GameContext, advCRequested: AdversaryCardRequested): Behavior[Message] = {
    Behaviors.receiveMessagePartial {
      handleExitSelected(context)
        .orElse({
          case CardSeen(card) =>
            log("waitAdversaryCardSelected", s"Received: ${CardSeen(card)}")
            context.ui.showAdversaryNthCard(advCRequested.adversaryID, advCRequested.adversaryCardIndex, card)
            myTurnAfterDraw(context)
        })
        .orElse(handleUnexpectedMessage("waitAdversaryCardSelected"))
    }
  }

  private def myTurnAfterDiscard(context: GameContext): Behavior[Message] = {
    hasDrawnFromDeck = false
    Behaviors.receiveMessagePartial {
      handleNewTopDiscardCard(context)
        .orElse(handleEndTurn(context))
        .orElse(handleExitSelected(context))
        .orElse(handleCaboSelected(context))
        .orElse(handleUnexpectedMessage("myTurnAfterDiscard"))
    }
  }

  // --- HANDLERS ---

  // HANDLERS revealing section - START ---
  private def handleShowCard(
                              context: GameContext,
                              behaviorAfterWatched: GameContext => Behavior[Message]):
  PartialFunction[Message, Behavior[Message]] = {
    case OwnCardSelected(index) =>
      ctx.log.info(s"DuringGameViewActor of player ${userID}, my ref is ${ctx.self}")
      ctx.log.info(s"DuringGameViewActor HANDLER handleWatchYourCards received OwnCardSelected with index: $index")
      context.coordinator ! GCMsg.ShowYourNthCard(index)
      waitCardSelected(context, behaviorAfterWatched)
  }

  private def handleAdversariesRevealingLog(context: GameContext): PartialFunction[Message, Behavior[Message]] = {
    case RevealingCardsPhaseAdversaryLog(revealingLog) =>
      log("", s"Handling RevealingCardsPhaseAdversaryLog with message: ${RevealingCardsPhaseAdversaryLog(revealingLog)}")
      context.ui.updateRevealingLog(revealingLog)
      Behaviors.same
  }
  // HANDLERS revealing section - END ---

  private def handleUpdateLastTurnPlayed(context: GameContext): PartialFunction[Message, Behavior[Message]] = {
    case LastTurnPlayed(turnLog, game) =>
      log("", s"Handling LastTurnPlayed")
      context.ui.updateLastTurnLog(turnLog)
      context.ui.updateGameInfo(game)
      context.ui.updateDiscardsTopCard(game.discardDeckStack.cards.head)
      waitMyTurn(context)
  }

  private def handleStartTurnPlayer(context: GameContext): PartialFunction[Message, Behavior[Message]] = {
    case StartTurnPlayer(playerID) =>
      log("", s"Handling StartTurnPlayer with message: ${StartTurnPlayer(playerID)}")
      context.ui.updatePlayerWhoIsPlaying(playerID)
      if (playerID == this.userID)
        myTurnBeforeDraw(context)
      else
        waitMyTurn(context)
  }

  // --- HANDLERS my turn - START ---
  private def handleWhichDeckSelectedForDrawing(context: GameContext): PartialFunction[Message, Behavior[Message]] = {
    case DeckSelected() =>
      log("myTurnBeforeDraw", s"Handling DeckSelected with message: ${DeckSelected()}")
      context.coordinator ! GCMsg.DrawCardFromDeck()
      hasDrawnFromDeck = true
      myTurnWaitDrawnCard(context)
    case DiscardStackSelected() =>
      log("myTurnBeforeDraw", s"Handling DiscardStackSelected with message: ${DiscardStackSelected()}")
      context.coordinator ! GCMsg.DrawCardFromDiscardStack()
      hasDrawnFromDeck = false
      myTurnWaitDrawnCard(context)
  }

  private def handleCardDrawn(context: GameContext): PartialFunction[Message, Behavior[Message]] = {
    case CardDrawn(card) =>
      log("myTurnWaitDrawnCard", s"Handling CardDrawn with message: ${CardDrawn(card)}")
      if (hasDrawnFromDeck) {
        context.ui.showCardDrawnFromDeck(card)
        card.power match
          case Power.SeeYourCard() => myTurnPowerSeeMyCard(context)
          case Power.SeeYourOpponentCard() => myTurnPowerSeeOpponentCard(context)
          case Power.ChangeOneOfYourCardWithOpponent() =>
            context.ui.usePowerToExchangeCardWithAdversary()
            myTurnPowerExchange(context, ExchangeState())
          case _ => myTurnAfterDraw(context)
      } else {
        context.ui.showCardDrawnFromDiscards(card)
        myTurnAfterDraw(context)
      }
  }

  private def handleWhichCardKeep(context: GameContext): PartialFunction[Message, Behavior[Message]] = {
    case DiscardCardDrawnSelected() if hasDrawnFromDeck =>
      log(" myTurnAfterDraw", s"Received DiscardCardDrawn")
      context.coordinator ! GCMsg.DiscardCardDrawn()
      context.ui.afterDiscarded()
      myTurnAfterDiscard(context)

    case OwnCardSelected(index) =>
      log("myTurnAfterDraw", s"Received OwnCardSelected with index: $index")
      context.coordinator ! GCMsg.DiscardYourNthCard(index)
      context.ui.afterDiscarded()
      myTurnAfterDiscard(context)
  }

  private def handleNewTopDiscardCard(context: GameContext): PartialFunction[Message, Behavior[Message]] = {
    case NewTopCardDiscardStack(card) =>
      log("", s"Handling NewTopDiscardCard with message: ${NewTopCardDiscardStack(card)}")
      context.ui.updateDiscardsTopCard(card)
      Behaviors.same
    case EmptyDiscardStack() =>
      log("", s"Handling EmptyDiscardStack with message: ${EmptyDiscardStack()}")
      context.ui.emptyDiscardStack()
      Behaviors.same
  }

  private def handleExitSelected(context: GameContext): PartialFunction[Message, Behavior[Message]] = {
    case ExitSelected() =>
      log("", s"Handling ExitSelected() with message: ${ExitSelected()}")
      context.frame.dispose()
      clientRef ! utils.ClientMessages.LeaveTheGame()
      Behaviors.stopped
  }

  private def handleCaboSelected(context: GameContext): PartialFunction[Message, Behavior[Message]] = {
    case CallCaboSelected() =>
      log("myTurnAfterDiscard", "Received CallCaboSelected")
      context.ui.enterWaitingPhase()
      context.coordinator ! GCMsg.CallCabo()
      waitMyTurn(context)
  }

  private def handleEndTurn(context: GameContext): PartialFunction[Message, Behavior[Message]] = {
    case EndTurnSelected() =>
      log("myTurn", "Received EndTurn")
      context.ui.enterWaitingPhase()
      context.coordinator ! GCMsg.EndTurn()
      waitMyTurn(context)
  }
  // --- HANDLERS my turn - END ---

  // --- HANDLERS game ending - START ---
  private def handleGameEnding(context: GameContext): PartialFunction[Message, Behavior[Message]] = {
    case GameEndedByCabo(game) => {
      log("waitMyTurn", s"Handling GameEnded with message: ${GameEndedByCabo(game)}")
      context.ui.gameEndedWithData(game)(EndGameReason.Cabo)
      waitingCloseGameFrame(context)
    }
    case GameEndedByTurnsLimit(game) => {
      log("waitMyTurn", s"Handling GameEnded with message: ${GameEndedByTurnsLimit(game)}")
      context.ui.gameEndedWithData(game)(EndGameReason.TurnsLimit)
      waitingCloseGameFrame(context)
    }
    case GameEndedByEmptyDeck(game) => {
      log("waitMyTurn", s"Handling GameEnded with message: ${GameEndedByEmptyDeck(game)}")
      context.ui.gameEndedWithData(game)(EndGameReason.EmptyDeck)
      waitingCloseGameFrame(context)
    }
  }

  private def handleConsultingResultsEnded(context: GameContext): PartialFunction[Message, Behavior[Message]] = {
    case ConsultingResultsEnded() =>
      log("", s"Handling ConsultingResultsEnded() with message: ${ConsultingResultsEnded()}")
      context.frame.dispose()
      clientRef ! utils.ClientMessages.GameEnded()
      Behaviors.stopped
  }
  // --- HANDLERS game ending - END ---

  private def handleUnexpectedMessage(actualState: String): PartialFunction[Message, Behavior[Message]] = {
    case msg =>
      // Uso WARN per i messaggi inaspettati per distinguerli meglio nei log
      ctx.log.warn(s"DuringGameViewActor of player $userID in state $actualState received unexpected message: $msg")
      Behaviors.same
  }

  // Helper functions

  private def log(state: String, msg: String): Unit = {
    ctx.log.info(s"[DuringGameViewActor]-[Player: $userID]-[STATE:$state]- $msg")
  }

}