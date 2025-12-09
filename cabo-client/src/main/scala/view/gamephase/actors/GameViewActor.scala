package view.gamephase.actors

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import messages.{ClientMessages, GameCoordinatorMessage, IGameViewMessage, IPreGameViewMessage}
import model.{EndGameReason, Game, Power}
import messages.ClientMessages.ClientCommand
import messages.GameViewMessages.*
import messages.ViewUserCommandMessages.*
import messages.{IGameViewMessage, IPreGameViewMessage, GameCoordinatorMessage as GCMsg}
import utils.Message
import view.gamephase.DuringGameMainFrame
import view.gamephase.traits.IDuringGameInterface

object GameViewActor {
  def apply(
             userID: String,
             clientRef: ActorRef[ClientCommand],
             mainMenuRef: ActorRef[IPreGameViewMessage]
           ): Behavior[IGameViewMessage] = {
    Behaviors.setup { ctx =>
      new GameViewActor(ctx, userID, clientRef, mainMenuRef).start()
    }
  }
}

private class GameViewActor private(
                                           val ctx: ActorContext[IGameViewMessage],
                                           val userID: String,
                                           val clientRef: ActorRef[ClientCommand],
                                           val mainMenuRef: ActorRef[IPreGameViewMessage]
                                         ) {

  private case class GameContext(
                                  coordinator: ActorRef[GameCoordinatorMessage.GameCoordinatorMessage],
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

  def start(): Behavior[IGameViewMessage] = Behaviors.setup { _ =>
    log("start", "DuringGameViewActor started")
    val frame = new DuringGameMainFrame(ViewUserCommandListener(ctx.self))
    frame.open()
    frame.visible = true

    clientRef ! ClientMessages.DuringGameViewReady(ctx.self)

    Behaviors.receiveMessagePartial {
      case StartGame(game, gameCoordinatorRef) =>
        log("start", s"Handling game started with message: ${StartGame(game, gameCoordinatorRef)}")
        val userInterface = frame.startGame(game, userID)
        userInterface.enterRevealingInitialCardsPhase()
        watchYourCards(GameContext(gameCoordinatorRef, frame, userInterface))
      case msg =>
        ctx.log.warn(s"DuringGameViewActor of player $userID in state start received unexpected message: $msg")
        Behaviors.same
    }
  }

  // --- WAITING STATES ---

  private def watchYourCards(context: GameContext): Behavior[IGameViewMessage] = {
    val stateName = "watchYourCards"
    Behaviors.receiveMessagePartial {
      handleShowCard(context, watchYourCards)(stateName)
        .orElse(handleAdversariesRevealingLog(context)(stateName))
        .orElse(handleGameDeleted(context)(stateName))
        .orElse({
          case WaitAfterRevealingSection() =>
            log(stateName, s"Handling message: ${WaitAfterRevealingSection()}")
            context.ui.enterWaitingPhase()
            waitFirstTurn(context)
        })
      //        .orElse(sharedHandlers(context)(stateName))
    }
  }

  private def waitFirstTurn(context: GameContext): Behavior[IGameViewMessage] = {
    val stateName = "waitFirstTurn"
    Behaviors.receiveMessagePartial {
      handleAdversariesRevealingLog(context)(stateName)
        .orElse(handleStartTurnPlayer(context)(stateName))
        .orElse(handleGameDeleted(context)(stateName))
        .orElse(sharedHandlers(context)(stateName))
    }
  }

  private def waitMyTurn(context: GameContext): Behavior[IGameViewMessage] = {
    val stateName = "waitMyTurn"
    Behaviors.receiveMessagePartial {
      handleUpdateLastTurnPlayed(context)(stateName)
        .orElse(handleStartTurnPlayer(context)(stateName))
        .orElse(handleGameEnding(context)(stateName))
        .orElse(sharedHandlers(context)(stateName))
    }
  }

  private def waitingCloseGameFrame(context: GameContext): Behavior[IGameViewMessage] = {
    val stateName = "waitingCloseGameFrame"
    Behaviors.receiveMessagePartial {
      handleConsultingResultsEnded(context)(stateName)
        .orElse(sharedHandlers(context)(stateName))
    }
  }

  // --- MY TURN BEHAVIORS ---

  private def myTurnBeforeDraw(context: GameContext): Behavior[IGameViewMessage] = {
    val stateName = "myTurnBeforeDraw"
    context.ui.startTurn()
    context.ui.updatePlayerWhoIsPlaying(userID)
    Behaviors.receiveMessagePartial {
      handleWhichDeckSelectedForDrawing(context)(stateName)
        .orElse(sharedHandlers(context)(stateName))
    }
  }

  private def myTurnWaitDrawnCard(context: GameContext): Behavior[IGameViewMessage] = {
    val stateName = "myTurnWaitDrawnCard"
    Behaviors.receiveMessagePartial {
      handleCardDrawn(context)(stateName)
        .orElse(handleNewTopDiscardCard(context)(stateName))
        .orElse(sharedHandlers(context)(stateName))
    }
  }

  private def myTurnAfterDraw(context: GameContext): Behavior[IGameViewMessage] = {
    val stateName = "myTurnAfterDraw"
    context.ui.afterDrawPhase(hasDrawnFromDeck)
    Behaviors.receiveMessagePartial {
      handleNewTopDiscardCard(context)(stateName)
        .orElse(handleWhichCardKeep(context)(stateName))
        .orElse(sharedHandlers(context)(stateName))
    }
  }

  private def myTurnPowerSeeMyCard(context: GameContext): Behavior[IGameViewMessage] = {
    val stateName = "myTurnPowerSeeMyCard"
    context.ui.usePowerToSeeOwnCard()
    Behaviors.receiveMessagePartial {
      handleShowCard(context, myTurnAfterDraw)(stateName)
        .orElse(sharedHandlers(context)(stateName))
    }
  }

  private def myTurnPowerSeeOpponentCard(context: GameContext): Behavior[IGameViewMessage] = {
    val stateName = "myTurnPowerSeeOpponentCard"
    context.ui.usePowerToSeeAdversaryCard()
    Behaviors.receiveMessagePartial {
      case AdversaryCardSelected(adversaryID, index) =>
        log(stateName, s"Received AdversaryCardSelected with index: $index")
        context.coordinator ! GCMsg.ShowAdversaryNthCard(adversaryID, index)
        waitAdversaryCardSelected(context, AdversaryCardRequested(adversaryID, index))
      case msg =>
        sharedHandlers(context)(stateName)(msg)
    }
  }

  private def myTurnPowerExchange(context: GameContext, state: ExchangeState): Behavior[IGameViewMessage] = {
    val stateName = "myTurnPowerExchange"
    if (state.isComplete) {
      val adv = state.adversarySelection.get
      context.coordinator ! GCMsg.ReplaceOwnNthCardWithAdversaryNthOne(state.ownCardIndex.get, adv.adversaryID, adv.adversaryCardIndex)
      Behaviors.receiveMessagePartial {
        case ChangeCardWithAdversaryAck() =>
          log("myTurnWaitPowerChangeAck", "Received ChangeCardWithAdversaryAck")
          context.ui.changeCardWithAdversaryIsDone()
          myTurnAfterDraw(context)
        case msg =>
          sharedHandlers(context)("myTurnWaitPowerChangeAck")(msg)
      }
    } else {
      Behaviors.receiveMessagePartial {
        case AdversaryCardSelected(adversaryID, index) if state.adversarySelection.isEmpty =>
          context.ui.activateAdversariesCards(false)
          context.ui.notifyYourAdversaryCardSelection(adversaryID, index)
          myTurnPowerExchange(context, state.copy(adversarySelection = Some(AdversaryCardRequested(adversaryID, index))))

        case OwnCardSelected(ownIndex) if state.ownCardIndex.isEmpty =>
          context.ui.activateOwnCards(false)
          context.ui.notifyYourOwnCardSelection(ownIndex)
          myTurnPowerExchange(context, state.copy(ownCardIndex = Some(ownIndex)))

        case msg =>
          sharedHandlers(context)(stateName)(msg)
      }
    }
  }

  private def waitCardSelected(
                                context: GameContext,
                                behaviorAfterCardReceived: GameContext => Behavior[IGameViewMessage]
                              ): Behavior[IGameViewMessage] = {
    val stateName = "waitCardSelected"
    Behaviors.receiveMessagePartial {
      handleAdversariesRevealingLog(context)(stateName)
        .orElse({
          case CardSeen(card) =>
            log(stateName, s"Received Card seen with card: $card")
            context.ui.showYourNthCard(card)
            behaviorAfterCardReceived(context)
        })
        .orElse(sharedHandlers(context)(stateName))
    }
  }

  private def waitAdversaryCardSelected(context: GameContext, advCRequested: AdversaryCardRequested): Behavior[IGameViewMessage] = {
    val stateName = "waitAdversaryCardSelected"
    Behaviors.receiveMessagePartial {
      case CardSeen(card) =>
        log(stateName, s"Received: ${CardSeen(card)}")
        context.ui.showAdversaryNthCard(advCRequested.adversaryID, advCRequested.adversaryCardIndex, card)
        myTurnAfterDraw(context)
      case msg =>
        sharedHandlers(context)(stateName)(msg)
    }
  }

  private def myTurnAfterDiscard(context: GameContext): Behavior[IGameViewMessage] = {
    val stateName = "myTurnAfterDiscard"
    hasDrawnFromDeck = false
    Behaviors.receiveMessagePartial {
      handleNewTopDiscardCard(context)(stateName)
        .orElse(handleEndTurn(context)(stateName))
        .orElse(handleCaboSelected(context)(stateName))
        .orElse(sharedHandlers(context)(stateName))
    }
  }

  // --- HANDLERS ---

  private def sharedHandlers(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    handleExitSelected(context)(actualState)
      .orElse(handleOpponentDisconnected(context)(actualState))
      .orElse(handleOpponentImpossibleToReach(context)(actualState))
      .orElse(handleUnexpectedMessage(actualState))
  }

  // HANDLERS revealing section - START ---
  private def handleShowCard(
                              context: GameContext,
                              behaviorAfterWatched: GameContext => Behavior[IGameViewMessage]
                            )(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case OwnCardSelected(index) =>
      log(actualState, s"DuringGameViewActor of player ${userID}, my ref is ${ctx.self}")
      log(actualState, s"DuringGameViewActor HANDLER handleWatchYourCards received OwnCardSelected with index: $index")
      context.coordinator ! GCMsg.ShowYourNthCard(index)
      waitCardSelected(context, behaviorAfterWatched)
  }

  private def handleAdversariesRevealingLog(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case RevealingCardsPhaseAdversaryLog(revealingLog) =>
      log(actualState, s"Handling RevealingCardsPhaseAdversaryLog with message: ${RevealingCardsPhaseAdversaryLog(revealingLog)}")
      context.ui.updateRevealingLog(revealingLog)
      Behaviors.same
  }

  private def handleGameDeleted(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case GameDeleted() =>
      //todo: aggiungere modifiche alla view da fare
      Behaviors.same
  }
  // HANDLERS revealing section - END ---

  private def handleUpdateLastTurnPlayed(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case LastTurnPlayed(turnLog, game) =>
      log(actualState, s"Handling LastTurnPlayed")
      context.ui.updateLastTurnLog(turnLog)
      context.ui.updateGameInfo(game)
      context.ui.updateDiscardsTopCard(game.discardDeckStack.cards.head)
      waitMyTurn(context)
  }

  private def handleStartTurnPlayer(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case StartTurnPlayer(playerID) =>
      log(actualState, s"Handling StartTurnPlayer with message: ${StartTurnPlayer(playerID)}")
      context.ui.updatePlayerWhoIsPlaying(playerID)
      if (playerID == this.userID)
        myTurnBeforeDraw(context)
      else
        waitMyTurn(context)
  }

  // --- HANDLERS my turn - START ---
  private def handleWhichDeckSelectedForDrawing(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case DeckSelected() =>
      log(actualState, s"Handling DeckSelected with message: ${DeckSelected()}")
      context.coordinator ! GCMsg.DrawCardFromDeck()
      hasDrawnFromDeck = true
      myTurnWaitDrawnCard(context)
    case DiscardStackSelected() =>
      log(actualState, s"Handling DiscardStackSelected with message: ${DiscardStackSelected()}")
      context.coordinator ! GCMsg.DrawCardFromDiscardStack()
      hasDrawnFromDeck = false
      myTurnWaitDrawnCard(context)
  }

  private def handleCardDrawn(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case CardDrawn(card) =>
      log(actualState, s"Handling CardDrawn with message: ${CardDrawn(card)}")
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

  private def handleWhichCardKeep(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case DiscardCardDrawnSelected() if hasDrawnFromDeck =>
      log(actualState, s"Received DiscardCardDrawn")
      context.coordinator ! GCMsg.DiscardCardDrawn()
      context.ui.afterDiscarded()
      myTurnAfterDiscard(context)

    case OwnCardSelected(index) =>
      log(actualState, s"Received OwnCardSelected with index: $index")
      context.coordinator ! GCMsg.DiscardYourNthCard(index)
      context.ui.afterDiscarded()
      myTurnAfterDiscard(context)
  }

  private def handleNewTopDiscardCard(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case NewTopCardDiscardStack(card) =>
      log(actualState, s"Handling NewTopDiscardCard with message: ${NewTopCardDiscardStack(card)}")
      context.ui.updateDiscardsTopCard(card)
      Behaviors.same
    case EmptyDiscardStack() =>
      log(actualState, s"Handling EmptyDiscardStack with message: ${EmptyDiscardStack()}")
      context.ui.emptyDiscardStack()
      Behaviors.same
  }

  private def handleExitSelected(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case ExitSelected() =>
      log(actualState, s"Handling ExitSelected() with message: ${ExitSelected()}")
      context.frame.dispose()
      clientRef ! messages.ClientMessages.LeaveTheGame()
      Behaviors.stopped
  }

  private def handleCaboSelected(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case CallCaboSelected() =>
      log(actualState, "Received CallCaboSelected")
      context.ui.enterWaitingPhase()
      context.coordinator ! GCMsg.CallCabo()
      waitMyTurn(context)
  }

  private def handleEndTurn(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case EndTurnSelected() =>
      log(actualState, "Received EndTurn")
      context.ui.enterWaitingPhase()
      context.coordinator ! GCMsg.EndTurn()
      waitMyTurn(context)
  }
  // --- HANDLERS my turn - END ---

  // --- HANDLERS game ending - START ---
  private def handleGameEnding(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case GameEndedByCabo(game) => {
      log(actualState, s"Handling GameEnded with message: ${GameEndedByCabo(game)}")
      context.ui.gameEndedWithData(game)(EndGameReason.Cabo)
      waitingCloseGameFrame(context)
    }
    case GameEndedByTurnsLimit(game) => {
      log(actualState, s"Handling GameEnded with message: ${GameEndedByTurnsLimit(game)}")
      context.ui.gameEndedWithData(game)(EndGameReason.TurnsLimit)
      waitingCloseGameFrame(context)
    }
    case GameEndedByEmptyDeck(game) => {
      log(actualState, s"Handling GameEnded with message: ${GameEndedByEmptyDeck(game)}")
      context.ui.gameEndedWithData(game)(EndGameReason.EmptyDeck)
      waitingCloseGameFrame(context)
    }
  }

  private def handleConsultingResultsEnded(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case ConsultingResultsEnded() =>
      log(actualState, s"Handling ConsultingResultsEnded() with message: ${ConsultingResultsEnded()}")
      context.frame.dispose()
      clientRef ! messages.ClientMessages.GameEnded()
      Behaviors.stopped
  }
  // --- HANDLERS game ending - END ---

  // HANDLERS connections problem - START

  private def handleOpponentDisconnected(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case OpponentDisconnected(player) =>
      log(actualState, s"Handling OpponentDisconnected with message: ${OpponentDisconnected(player)}")
      context.ui.opponentsDisconnected(player)
      Behaviors.same
  }

  private def handleOpponentImpossibleToReach(context: GameContext)(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
    case OpponentImpossibleToReach(player) =>
      log(actualState, s"Handling OpponentImpossibleToReach with message: ${OpponentImpossibleToReach(player)}")
      context.ui.opponentImpossibleToReach(player)
      Behaviors.same
  }

  // HANDLERS connections problem - END


  private def handleUnexpectedMessage(actualState: String): PartialFunction[IGameViewMessage, Behavior[IGameViewMessage]] = {
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