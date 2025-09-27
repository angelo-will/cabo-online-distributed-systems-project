package view.gamephase

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import model.{Card, Game, Power, TurnLog}
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
  private var lastTurnLog: TurnLog = _

  private var adversaryIDRequested: String = _
  private var adversaryIndexCardRequested: Int = _
  private var adversaryCardRequested: Card = _

  private var ownCardIndexSelected: Int = _

  private var isAdversaryCardRequested: Boolean = false
  private var isOwnCardRequested: Boolean = false

  private var canDiscardDrawnCard: Boolean = false

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
      handleShowCard(properties, watchYourCards)
        .orElse(handleStartPlayPhase(properties))
        .orElse({
          case _ => Behaviors.same
        })
    }
  }

  private def waitMyTurn(properties: PropertiesAfterInitialization): Behavior[Message] = {
    Behaviors.receivePartial {
      handleUpdateLastTurnPlayed(properties)
        .orElse(handleFirstTurn(properties))
        .orElse({
          case msg =>
            println(s"DuringGameViewActor in waitMyTurn received message: $msg")
            Behaviors.same
        })
    }
  }

  // MY TURN BEHAVIORS - START
  private def myTurn(properties: PropertiesAfterInitialization): Behavior[Message] = {
    properties.userInterface.startTurn()
    Behaviors.receivePartial {
      handleDeckSelected(properties)
        .orElse(handleDiscardStackSelected(properties))
        .orElse(handleCardDrawn(properties))
        .orElse(handleNewTopDiscardCard(properties))
      //        .orElse(handleChangeCardWithDrawnOne(properties))
      //        .orElse(handleDiscardCardDrawn(properties))
    }
  }

  private def myTurnAfterDraw(
                               properties: PropertiesAfterInitialization
                             ): Behavior[Message] = {
    properties.userInterface.afterDrawPhase(canDiscardDrawnCard)
    Behaviors.receivePartial {
      case (ctx, DiscardCardDrawn()) if canDiscardDrawnCard =>
        println(s"DuringGameViewActor in myTurnAfterDraw received DiscardCardDrawn")
        properties.userInterface.emptyDrawnCardArea()
        properties.gameCoordinatorRef ! GameCoordinatorMessage.DiscardCardDrawn()
        properties.userInterface.afterDiscarded()
        myTurnAfterDiscard(properties)
      case (ctx, OwnCardSelected(index)) =>
        println(s"DuringGameViewActor in myTurnAfterDraw received OwnCardSelected with index: $index")
        properties.gameCoordinatorRef ! GameCoordinatorMessage.DiscardYourNthCard(index)
        properties.userInterface.afterDiscarded()
        myTurnAfterDiscard(properties)
    }
  }

  private def myTurnAfterDiscard(properties: PropertiesAfterInitialization): Behavior[Message] = {
    canDiscardDrawnCard = false
    Behaviors.receivePartial {
      handleNewTopDiscardCard(properties)
        .orElse({
          case (ctx, EndTurn()) =>
            ctx.log.info(s"DuringGameViewActor in myTurn received EndTurn")
            properties.userInterface.enterWaitingPhase()
            properties.gameCoordinatorRef ! GameCoordinatorMessage.EndTurn()
            waitMyTurn(properties)
        })
    }
  }

  private def myTurnPowerSeeMyCard(properties: PropertiesAfterInitialization): Behavior[Message] = {
    //    properties.userInterface.enterSeeYourCardPhase()
    Behaviors.receivePartial {
      handleShowCard(properties, myTurnAfterDraw)
    }
  }

  private def myTurnPowerSeeOpponentCard(properties: PropertiesAfterInitialization): Behavior[Message] = {
    properties.userInterface.usePowerToSeeAdversaryCard()
    Behaviors.receivePartial {
      case (ctx, AdversaryCardSelected(adversaryID, index)) =>
        println(s"DuringGameViewActor in myTurnPowerSeeOpponentCard received AdversaryCardSelected with index: $index")
        properties.gameCoordinatorRef ! GameCoordinatorMessage.ShowAdversaryNthCard(adversaryID, index)
        this.adversaryIndexCardRequested = index
        this.adversaryIDRequested = adversaryID
        waitAdversaryCardSelected(properties)
    }
  }

  private def myTurnPowerExchange(properties: PropertiesAfterInitialization): Behavior[Message] = {
    if !isAdversaryCardRequested || !isOwnCardRequested then
      properties.userInterface.usePowerToExchangeCardWithAdversary()
    Behaviors.receivePartial {
      case (ctx, AdversaryCardSelected(adversaryID, index)) if !this.isAdversaryCardRequested =>
        println(s"DuringGameViewActor in myTurnPowerExchange received AdversaryCardSelected with index: $index")
        this.adversaryIndexCardRequested = index
        this.adversaryIDRequested = adversaryID
        this.isAdversaryCardRequested = true
        properties.userInterface.activateAdversariesCards(false)
        properties.userInterface.notifyYourAdversaryCardSelection(adversaryID, index)
        checkIfCanExchangeCard(properties)
      case (ctx, OwnCardSelected(ownIndex)) if !this.isOwnCardRequested =>
        println(s"DuringGameViewActor in myTurnPowerExchange received OwnCardSelected with index: $ownIndex")
        this.ownCardIndexSelected = ownIndex
        this.isOwnCardRequested = true
        properties.userInterface.activateOwnCards(false)
        properties.userInterface.notifyYourOwnCardSelection(ownIndex)
        checkIfCanExchangeCard(properties)
    }
  }

  private def myTurnWaitPowerChangeAck(properties: PropertiesAfterInitialization): Behavior[Message] = {
    Behaviors.receivePartial {
      case (ctx, DuringGameViewMessages.ChangeCardWithAdversaryAck()) =>
        println(s"DuringGameViewActor in myTurnWaitPowerChangeAck received ChangeCardWithAdversaryAck")
        properties.userInterface.changeCardWithAdversaryIsDone()
        myTurnAfterDraw(properties)
    }
  }
  // MY TURN BEHAVIORS - END


  private def waitCardSelected(
                                properties: PropertiesAfterInitialization,
                                behaviorAfterCardReceived: PropertiesAfterInitialization => Behavior[Message]
                              ): Behavior[Message] = {
    Behaviors.receivePartial {
      case (ctx, CardSeen(card)) =>
        println(s"DuringGameViewActor in waitCardSelected received Card seen with card: $card")
        properties.userInterface.showYourNthCard(card)
        behaviorAfterCardReceived(properties)
    }
  }

  private def waitAdversaryCardSelected(properties: PropertiesAfterInitialization): Behavior[Message] = {
    Behaviors.receivePartial {
      case (ctx, CardSeen(card)) =>
        println(s"DuringGameViewActor in waitAdversaryCardSelected received: ${CardSeen(card)}")
        properties.userInterface.showAdversaryNthCard(adversaryIDRequested, adversaryIndexCardRequested, card)
        myTurnAfterDraw(properties)
    }
  }

  // HANDLERS

  private def handleGameStarted(properties: PropertiesBeforeInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, StartGame(game, gameCoordinatorRef)) =>
      ctx.log.info(s"DuringGameViewActor handling game started with message: ${StartGame(game, gameCoordinatorRef)}")
      val userInterface = properties.frame.startGame(game, userID)
      userInterface.enterRevealingInitialCardsPhase()
      lastGameUpdate = game
      watchYourCards(PropertiesAfterInitialization(gameCoordinatorRef, properties.frame, userInterface))
  }

  private def handleStartPlayPhase(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, StartPlayPhase()) => {
      ctx.log.info(s"DuringGameViewActor handling StartPlayPhase with message: ${StartPlayPhase()}")
      properties.userInterface.enterWaitingPhase()
      waitMyTurn(properties)
    }
  }

  private def handleShowCard(
                              properties: PropertiesAfterInitialization,
                              behaviorAfterWatched: PropertiesAfterInitialization => Behavior[Message]):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, OwnCardSelected(index)) =>
      println(s"DuringGameViewActor HANDLER handleWatchYourCards received OwnCardSelected with index: $index")
      properties.gameCoordinatorRef ! GameCoordinatorMessage.ShowYourNthCard(index)
      waitCardSelected(properties, behaviorAfterWatched)
  }

  private def handleUpdateLastTurnPlayed(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, LastTurnPlayed(turnLog, game, isMyTurn)) =>
      ctx.log.info(s"DuringGameViewActor handling LastTurnPlayed with message: ${LastTurnPlayed(turnLog, game, isMyTurn)}")
      lastGameUpdate = game
      lastTurnLog = turnLog
      properties.userInterface.updateLastTurnLog(turnLog.playerName, (game.currentRound - 1), turnLog)
      properties.userInterface.updateGameInfo(game)
      properties.userInterface.updateDiscardsTopCard(game.discardDeckStack.cards.head)
      if isMyTurn then myTurn(properties) else waitMyTurn(properties)
  }

  private def handleFirstTurn(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, FirstTurn()) =>
      ctx.log.info(s"DuringGameViewActor handling FirstTurn with message: ${FirstTurn()}")
      myTurn(properties)
    // next behave
  }

  private def handleDeckSelected(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, DeckSelected()) =>
      println(s"DuringGameViewActor handling DeckSelected with message: ${DeckSelected()}")
      properties.gameCoordinatorRef ! GameCoordinatorMessage.DrawCardFromDeck()
      canDiscardDrawnCard = true
      Behaviors.same
    //      waitCardSelected(properties, waitMyTurn)
  }

  private def handleDiscardStackSelected(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, DiscardStackSelected()) =>
      println(s"DuringGameViewActor handling DiscardStackSelected with message: ${DiscardStackSelected()}")
      properties.gameCoordinatorRef ! GameCoordinatorMessage.DrawCardFromDiscardStack()
      canDiscardDrawnCard = false
      Behaviors.same
  }

  private def handleCardDrawn(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, CardDrawn(card)) =>
      ctx.log.info(s"DuringGameViewActor handling CardDrawn with message: ${CardDrawn(card)}")
      properties.userInterface.afterDrawPhase(canDiscardDrawnCard)
      properties.userInterface.showCardDrawnFromDeck(card)
      card.power match
        case Power.SeeYourCard() => myTurnPowerSeeMyCard(properties)
        case Power.SeeYourOpponentCard() => myTurnPowerSeeOpponentCard(properties)
        case Power.ChangeOneOfYourCardWithOpponent() => myTurnPowerExchange(properties)
        case _ => myTurnAfterDraw(properties)
  }

  private def handleNewTopDiscardCard(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, NewTopCardDiscardStack(card)) =>
      ctx.log.info(s"DuringGameViewActor handling NewTopDiscardCard with message: ${NewTopCardDiscardStack(card)}")
      properties.userInterface.updateDiscardsTopCard(card)
      Behaviors.same
    case (ctx, EmptyDiscardStack()) =>
      ctx.log.info(s"DuringGameViewActor handling EmptyDiscardStack with message: ${EmptyDiscardStack()}")
      properties.userInterface.emptyDiscardStack()
      Behaviors.same
  }

  private def handleDiscardCardDrawn(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, DiscardCardDrawn()) => {
      println(s"DuringGameViewActor handling Disc] with message: ${DiscardCardDrawn()}")
      properties.userInterface.emptyDrawnCardArea()
      properties.gameCoordinatorRef ! GameCoordinatorMessage.DiscardCardDrawn()
      properties.userInterface.afterDiscarded()
      Behaviors.same
    }
  }

  private def handleChangeCardWithDrawnOne(properties: PropertiesAfterInitialization):
  PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, OwnCardSelected(index)) =>
      println(s"DuringGameViewActor HANDLER OwnCardSelected with message: ${OwnCardSelected(index)}")
      properties.gameCoordinatorRef ! GameCoordinatorMessage.DiscardYourNthCard(index)
      Behaviors.same
  }

  // SUPPORT FUNCTIONS

  private def checkIfCanExchangeCard(properties: PropertiesAfterInitialization): Behavior[Message] = {
    if isAdversaryCardRequested && isOwnCardRequested then {
      properties.gameCoordinatorRef ! GameCoordinatorMessage.ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndexSelected, adversaryIDRequested, adversaryIndexCardRequested)
      isAdversaryCardRequested = false
      isOwnCardRequested = false
      myTurnWaitPowerChangeAck(properties)
    } else {
      Behaviors.same
    }
  }
}