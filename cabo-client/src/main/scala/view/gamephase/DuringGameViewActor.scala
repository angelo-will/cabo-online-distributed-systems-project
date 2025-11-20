package view.gamephase

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import model.{Card, Game, Power, TurnLog}
import utils.{DuringGameViewMessages, GameCoordinatorMessage as GCMsg, InitialViewMessages, Message}
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
                                                    gameCoordinatorRef: ActorRef[GCMsg.GameCoordinatorMessage],
                                                    frame: DuringGameMainFrame,
                                                    userInterface: IDuringGameInterface
                                                  )

  private var lastGameUpdate: Game.GameInProgress = _
  private var lastTurnLog: TurnLog = _

  private var adversaryIDRequested: String = _
  private var adversaryIndexCardRequested: Int = _
  private var adversaryCardRequested: Card = _

  private var ownCardIndexSelected: Int = _

  private var isAdversaryCardRequested: Boolean = false
  private var isOwnCardRequested: Boolean = false

  private var hasDrawnFromDeck: Boolean = false

  //  private var cardsSeenQuantity = 0

  // MY TURN BEHAVIORS - START
  // TODO: add the exit from game behavior
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
      case (ctx, StartGame(game, gameCoordinatorRef)) =>
        ctx.log.info(s"DuringGameViewActor of player ${userID}, my ref is ${ctx.self}")
        ctx.log.info(s"DuringGameViewActor of player $userID handling game started with message: ${StartGame(game, gameCoordinatorRef)}")
        val userInterface = properties.frame.startGame(game, userID)
        userInterface.enterRevealingInitialCardsPhase()
        lastGameUpdate = game
        watchYourCards(PropertiesAfterInitialization(gameCoordinatorRef, properties.frame, userInterface))
      case msg =>
        println(s"DuringGameViewActor of player $userID in waitingGameCreated received message: $msg")
        Behaviors.same

    }
  }

  private def watchYourCards(properties: PropertiesAfterInitialization): Behavior[Message] = {
    Behaviors.receivePartial {
      handleShowCard(properties, watchYourCards)
        .orElse(handleAdversariesRevealingLog(properties))
        .orElse({
          case (ctx, WaitAfterRevealingSection()) => {
            ctx.log.info(s"DuringGameViewActor of player $userID handling message: ${WaitAfterRevealingSection()}")
            properties.userInterface.enterWaitingPhase()
            waitFirstTurn(properties)
          }
          case msg =>
            println(s"DuringGameViewActor of player $userID in watchYourCards received message: $msg")
            Behaviors.same
        })
    }
  }

  private def waitFirstTurn(properties: PropertiesAfterInitialization): Behavior[Message] = {
    Behaviors.receivePartial {
      handleAdversariesRevealingLog(properties)
        .orElse({
          case (ctx, StartTurnPlayer(playerID)) =>
            ctx.log.info(s"DuringGameViewActor of player $userID handling message: ${StartTurnPlayer(playerID)}")
            properties.userInterface.updatePlayerWhoIsPlaying(playerID)
            waitMyTurn(properties)
          case (ctx, FirstTurn()) =>
            ctx.log.info(s"DuringGameViewActor of player $userID handling message: ${FirstTurn()}")
            myTurnBeforeDraw(properties)
          case (ctx, msg) =>
            ctx.log.info(s"DuringGameViewActor of player $userID in waitFirstTurn received unexpected message: $msg")
            Behaviors.same
        })
    }
  }

  private def waitMyTurn(properties: PropertiesAfterInitialization): Behavior[Message] = {
    Behaviors.receivePartial {
      handleUpdateLastTurnPlayed(properties)
        .orElse({
          case (ctx, StartTurnPlayer(playerID)) =>
            ctx.log.info(s"DuringGameViewActor of player $userID handling StartTurnPlayer with message: ${StartTurnPlayer(playerID)}")
            properties.userInterface.updatePlayerWhoIsPlaying(playerID)
            Behaviors.same
          case (ctx, GameEndedByCabo(game)) =>
            ctx.log.info(s"DuringGameViewActor of player $userID handling GameEnded with message: ${GameEndedByCabo(game)}")
            properties.userInterface.gameEndedByCabo(game)
            waitingCloseGameFrame()
          case (ctx, GameEndedByTurnsLimit(game)) =>
            ctx.log.info(s"DuringGameViewActor of player $userID handling GameEnded with message: ${GameEndedByTurnsLimit(game)}")
            properties.userInterface.gameEndedByTurns(game)
            waitingCloseGameFrame()
          case (ctx, GameEndedByEmptyDeck(game)) =>
            ctx.log.info(s"DuringGameViewActor of player $userID handling GameEnded with message: ${GameEndedByEmptyDeck(game)}")
            properties.userInterface.gameEndedByEmptyDeck(game)
            waitingCloseGameFrame()
          case msg =>
            println(s"DuringGameViewActor of player $userID in waitMyTurn received message: $msg")
            Behaviors.same
        })
    }
  }

  private def waitingCloseGameFrame(): Behavior[Message] = Behaviors.same

  private def myTurnBeforeDraw(properties: PropertiesAfterInitialization): Behavior[Message] = {
    properties.userInterface.startTurn()
    properties.userInterface.updatePlayerWhoIsPlaying(userID)
    Behaviors.receivePartial {
      handleExitSelected(properties)
        .orElse(handleEndTurn(properties))
        .orElse({
          case (ctx, DeckSelected()) =>
            println(s"DuringGameViewActor of player $userID handling DeckSelected with message: ${DeckSelected()}")
            properties.gameCoordinatorRef ! GCMsg.DrawCardFromDeck()
            hasDrawnFromDeck = true
            myTurnWaitDrawnCard(properties)
          case (ctx, DiscardStackSelected()) =>
            println(s"DuringGameViewActor of player $userID handling DiscardStackSelected with message: ${DiscardStackSelected()}")
            properties.gameCoordinatorRef ! GCMsg.DrawCardFromDiscardStack()
            hasDrawnFromDeck = false
            myTurnWaitDrawnCard(properties)
        })
    }
  }

  private def myTurnWaitDrawnCard(properties: PropertiesAfterInitialization): Behavior[Message] = {
    Behaviors.receivePartial {
      handleNewTopDiscardCard(properties)
        .orElse(handleEndTurn(properties))
        .orElse({
          case (ctx, CardDrawn(card)) =>
            ctx.log.info(s"DuringGameViewActor of player $userID handling CardDrawn with message: ${CardDrawn(card)}")
            if hasDrawnFromDeck then
              properties.userInterface.showCardDrawnFromDeck(card)
              card.power match
                case Power.SeeYourCard() => myTurnPowerSeeMyCard(properties)
                case Power.SeeYourOpponentCard() => myTurnPowerSeeOpponentCard(properties)
                case Power.ChangeOneOfYourCardWithOpponent() => myTurnPowerExchange(properties)
                case _ => myTurnAfterDraw(properties)
            else
              properties.userInterface.showCardDrawnFromDiscards(card)
              myTurnAfterDraw(properties)
        })
    }
  }

  private def myTurnAfterDraw(
                               properties: PropertiesAfterInitialization
                             ): Behavior[Message] = {
    properties.userInterface.afterDrawPhase(hasDrawnFromDeck)
    Behaviors.receivePartial {
      handleNewTopDiscardCard(properties)
        .orElse(handleEndTurn(properties))
        .orElse({
          case (ctx, DiscardCardDrawn()) if hasDrawnFromDeck =>
            println(s"DuringGameViewActor of player $userID in myTurnAfterDraw received DiscardCardDrawn")
            properties.gameCoordinatorRef ! GCMsg.DiscardCardDrawn()
            properties.userInterface.afterDiscarded()
            myTurnAfterDiscard(properties)
          case (ctx, OwnCardSelected(index)) =>
            println(s"DuringGameViewActor of player $userID in myTurnAfterDraw received OwnCardSelected with index: $index")
            properties.gameCoordinatorRef ! GCMsg.DiscardYourNthCard(index)
            properties.userInterface.afterDiscarded()
            myTurnAfterDiscard(properties)
        })
    }
  }


  private def myTurnPowerSeeMyCard(properties: PropertiesAfterInitialization): Behavior[Message] = {
    properties.userInterface.usePowerToSeeOwnCard()
    Behaviors.receivePartial {
      handleShowCard(properties, myTurnAfterDraw)
        .orElse(handleEndTurn(properties))
    }
  }

  private def myTurnPowerSeeOpponentCard(properties: PropertiesAfterInitialization): Behavior[Message] = {
    properties.userInterface.usePowerToSeeAdversaryCard()
    Behaviors.receivePartial {
      handleEndTurn(properties)
        .orElse({
          case (ctx, AdversaryCardSelected(adversaryID, index)) =>
            println(s"DuringGameViewActor of player $userID in myTurnPowerSeeOpponentCard received AdversaryCardSelected with index: $index")
            properties.gameCoordinatorRef ! GCMsg.ShowAdversaryNthCard(adversaryID, index)
            this.adversaryIndexCardRequested = index
            this.adversaryIDRequested = adversaryID
            waitAdversaryCardSelected(properties)
        })
    }
  }

  private def myTurnPowerExchange(properties: PropertiesAfterInitialization): Behavior[Message] = {
    if !isAdversaryCardRequested || !isOwnCardRequested then
      properties.userInterface.usePowerToExchangeCardWithAdversary()
    Behaviors.receivePartial {
      handleEndTurn(properties)
        .orElse({
          case (ctx, AdversaryCardSelected(adversaryID, index)) if !this.isAdversaryCardRequested =>
            println(s"DuringGameViewActor of player $userID in myTurnPowerExchange received AdversaryCardSelected with index: $index")
            this.adversaryIndexCardRequested = index
            this.adversaryIDRequested = adversaryID
            this.isAdversaryCardRequested = true
            properties.userInterface.activateAdversariesCards(false)
            properties.userInterface.notifyYourAdversaryCardSelection(adversaryID, index)
            checkIfCanExchangeCard(properties)
          case (ctx, OwnCardSelected(ownIndex)) if !this.isOwnCardRequested =>
            println(s"DuringGameViewActor of player $userID in myTurnPowerExchange received OwnCardSelected with index: $ownIndex")
            this.ownCardIndexSelected = ownIndex
            this.isOwnCardRequested = true
            properties.userInterface.activateOwnCards(false)
            properties.userInterface.notifyYourOwnCardSelection(ownIndex)
            checkIfCanExchangeCard(properties)
        })
    }
  }

  private def myTurnWaitPowerChangeAck(properties: PropertiesAfterInitialization): Behavior[Message] = {
    Behaviors.receivePartial {
      handleEndTurn(properties)
        .orElse({
          case (ctx, DuringGameViewMessages.ChangeCardWithAdversaryAck()) =>
            println(s"DuringGameViewActor of player $userID in myTurnWaitPowerChangeAck received ChangeCardWithAdversaryAck")
            properties.userInterface.changeCardWithAdversaryIsDone()
            myTurnAfterDraw(properties)
        })
    }
  }

  private def waitCardSelected(
                                properties: PropertiesAfterInitialization,
                                behaviorAfterCardReceived: PropertiesAfterInitialization => Behavior[Message]
                              ): Behavior[Message] = {
    Behaviors.receivePartial {
      handleAdversariesRevealingLog(properties)
        .orElse(handleEndTurn(properties))
        .orElse({
          case (ctx, CardSeen(card)) =>
            println(s"DuringGameViewActor of player $userID in waitCardSelected received Card seen with card: $card")
            properties.userInterface.showYourNthCard(card)
            behaviorAfterCardReceived(properties)
          case (ctx, msg) =>
            ctx.log.info(s"DuringGameViewActor ID $userID in waitCardSelected received unexpected message: $msg")
            Behaviors.same
        })
    }
  }

  private def waitAdversaryCardSelected(properties: PropertiesAfterInitialization): Behavior[Message] = {
    Behaviors.receivePartial {
      handleEndTurn(properties)
        .orElse({
          case (ctx, CardSeen(card)) =>
            println(s"DuringGameViewActor of player $userID in waitAdversaryCardSelected received: ${CardSeen(card)}")
            properties.userInterface.showAdversaryNthCard(adversaryIDRequested, adversaryIndexCardRequested, card)
            myTurnAfterDraw(properties)
        })
    }
  }

  private def myTurnAfterDiscard(properties: PropertiesAfterInitialization): Behavior[Message] = {
    hasDrawnFromDeck = false
    Behaviors.receivePartial {
      handleNewTopDiscardCard(properties)
        .orElse(handleEndTurn(properties))
        .orElse({
          case (ctx, CallCaboSelected()) =>
            ctx.log.info(s"DuringGameViewActor of player $userID in myTurnAfterDiscard received CallCaboSelected")
            properties.userInterface.enterWaitingPhase()
            properties.gameCoordinatorRef ! GCMsg.CallCabo()
            waitMyTurn(properties)
        })
    }
  }
  // MY TURN BEHAVIORS - END

  // HANDLERS

  private def handleShowCard(
                              properties: PropertiesAfterInitialization,
                              behaviorAfterWatched: PropertiesAfterInitialization => Behavior[Message]):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, OwnCardSelected(index)) =>
      ctx.log.info(s"DuringGameViewActor of player ${userID}, my ref is ${ctx.self}")
      println(s"DuringGameViewActor HANDLER handleWatchYourCards received OwnCardSelected with index: $index")
      properties.gameCoordinatorRef ! GCMsg.ShowYourNthCard(index)
      waitCardSelected(properties, behaviorAfterWatched)
  }

  private def handleUpdateLastTurnPlayed(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, LastTurnPlayed(turnLog, game, isMyTurn)) =>
      ctx.log.info(s"DuringGameViewActor of player $userID handling LastTurnPlayed with isMyTurn = $isMyTurn}")
      ctx.log.info(s"DuringGameViewActor of player $userID handling LastTurnPlayed with turnLog =\n$turnLog")
      ctx.log.info(s"DuringGameViewActor of player $userID handling LastTurnPlayed with game =\n$game")
      lastGameUpdate = game
      lastTurnLog = turnLog
      properties.userInterface.updateLastTurnLog(turnLog)
      properties.userInterface.updateGameInfo(game)
      properties.userInterface.updateDiscardsTopCard(game.discardDeckStack.cards.head)
      if isMyTurn then myTurnBeforeDraw(properties) else waitMyTurn(properties)
  }

  private def handleNewTopDiscardCard(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, NewTopCardDiscardStack(card)) =>
      ctx.log.info(s"DuringGameViewActor of player $userID handling NewTopDiscardCard with message: ${NewTopCardDiscardStack(card)}")
      properties.userInterface.updateDiscardsTopCard(card)
      Behaviors.same
    case (ctx, EmptyDiscardStack()) =>
      ctx.log.info(s"DuringGameViewActor of player $userID handling EmptyDiscardStack with message: ${EmptyDiscardStack()}")
      properties.userInterface.emptyDiscardStack()
      Behaviors.same
  }

  private def handleExitSelected(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, ExitSelected()) =>
      println(s"DuringGameViewActor of player $userID handling ExitSelected() with message: ${ExitSelected()}")
      // TODO: add exit to game coordinator x
      //        properties.gameCoordinatorRef ! GCMsg.Exit()
      Behaviors.same
  }

  private def handleAdversariesRevealingLog(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, RevealingCardsPhaseAdversaryLog(revealingLog)) =>
      ctx.log.info(s"DuringGameViewActor of player $userID handling RevealingCardsPhaseAdversaryLog with message: ${RevealingCardsPhaseAdversaryLog(revealingLog)}")
      properties.userInterface.updateRevealingLog(revealingLog)
      Behaviors.same
  }

  private def handleEndTurn(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, EndTurn()) =>
      ctx.log.info(s"DuringGameViewActor of player $userID in myTurn received EndTurn")
      properties.userInterface.enterWaitingPhase()
      properties.gameCoordinatorRef ! GCMsg.EndTurn()
      waitMyTurn(properties)
  }

  // SUPPORT FUNCTIONS

  private def checkIfCanExchangeCard(properties: PropertiesAfterInitialization): Behavior[Message] = {
    if isAdversaryCardRequested && isOwnCardRequested then {
      properties.gameCoordinatorRef ! GCMsg.ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndexSelected, adversaryIDRequested, adversaryIndexCardRequested)
      isAdversaryCardRequested = false
      isOwnCardRequested = false
      myTurnWaitPowerChangeAck(properties)
    } else {
      Behaviors.same
    }
  }
}