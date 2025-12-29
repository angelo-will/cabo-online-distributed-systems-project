package controller

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import messages.{IGameViewMessage, ClientMessages as CLMsg, GameCoordinatorMessage as GCMsg, GameViewMessages as DGVMsg}
import model.*
import model.Game.{GameInConstruction, GameInProgress}
import utils.AppLogger
import messages.ClientMessages.ClientCommand as CCommand
import messages.GameCoordinatorMessage.GameCoordinatorMessage

import scala.concurrent.duration.*

object GameCoordinatorActor:

  import akka.actor.typed.ActorRef

  import model.CardStack
  import model.GameStatus
  import model.Game

  private abstract class EndingGame

  private def gameEnded(game: GameInProgress, nextWhoPlayID: String): EndingGame = {
    if game.caboState.isDefined && nextWhoPlayID == game.caboState.get.userID then
      EndedByCabo()
    else if game.gameParameters.roundLimitation.isRoundsEnded(game.currentRound) then
      EndedByTurnsLimit()
    else if game.deckStack.isEmpty then
      EndedByEmptyDeck()
    else NotEnded()
  }

  private case class EndedByCabo() extends EndingGame

  private case class EndedByTurnsLimit() extends EndingGame

  private case class EndedByEmptyDeck() extends EndingGame

  private case class NotEnded() extends EndingGame

  private case class GameData(
                               clientReference: ActorRef[CCommand],
                               // todo: change type in ViewCommand moving the messages of GameCoordinatorMessage in ViewMessages?
                               viewReference: ActorRef[IGameViewMessage],
                               playerOwnRank: Int,
                               playerOwnUserID: String,
                               game: GameInProgress,
                               temporaryGame: GameInProgress,
                               turnLog: TurnLog,
                             ):
    def getOurHand: Hand = this.getSelfPlayer.hand

    def getHandOPlayerWithID(playerID: String): Hand = this.getPlayerWithID(playerID).hand

    def getSelfPlayer: PlayerPlaying = this.temporaryGame.getPlayerWithID(this.playerOwnUserID)

    def getPlayerWithID(playerID: String): PlayerPlaying = this.temporaryGame.getPlayerWithID(playerID)

    def syncAllTemporaryDecks: GameData =
      this.copy(game = temporaryGame)

    override def toString: String =
      "GameData: \n" +
        "whoToSendResponse=" + viewReference + "\n" +
        "playerRank=" + playerOwnRank + "\n" +
        "game=" + game + "\n" +
        "temporaryGame=" + temporaryGame + "\n" +
        "turnLog=" + turnLog + "\n"

  private val log = AppLogger.Log(true)

  def apply(client: ActorRef[CCommand], viewToContact: ActorRef[IGameViewMessage], userId: String, gameToStart: GameInConstruction): Behavior[GameCoordinatorMessage] = {
    val game = generateGameInProgressFromInConstruction(gameToStart)
    // todo: this one is used to test rounds limit
    // val game = generateGameInProgressFromInConstruction(gameToStart.copy(gameParameters = GameParameters(roundLimitation = 3)))
    client ! CLMsg.TakeGetInProgressGame(game)
    apply(client, viewToContact, userId, game)
  }

  def apply(client: ActorRef[CCommand], viewToContact: ActorRef[IGameViewMessage], userId: String, gameInProgress: GameInProgress): Behavior[GameCoordinatorMessage] = {

    val playerRank = gameInProgress.players.find(_.userID == userId) match {
      case Some(player) => player.rank
      //todo - remove exception
      case None => throw new IllegalArgumentException(s"User ID $userId not found in the game players")
    }

    val gameData = GameData(
      clientReference = client,
      viewReference = viewToContact,
      playerOwnRank = playerRank,
      playerOwnUserID = userId,
      game = gameInProgress,
      temporaryGame = gameInProgress,
      turnLog = new RevealingSectionTurnLog(userId),
    )

    waitingStart(gameData)
  }

  private def generateGameInProgressFromInConstruction(gameInConstruction: GameInConstruction): GameInProgress = {

    var fullDeckShuffled = CardStack.buildShuffledFullDeck
    val playersPlaying = gameInConstruction.players.zipWithIndex.map((p, index) => {
      val (hand, remainingDeck) = fullDeckShuffled.drawNCards(4)
      fullDeckShuffled = remainingDeck
      PlayerPlaying(p.userID, p.name, index + 1, Hand(hand))
    })
    val (firstCardDiscard, remainingDeck) = fullDeckShuffled.drawNCards(1)
    //    val (_, fewCardsToTestEndByEmptyDeck) = remainingDeck.drawNCards(remainingDeck.cards.size - playersPlaying.size)
    GameInProgress(gameInConstruction.code, gameInConstruction.gameParameters, GameStatus.InProgress(), playersPlaying, remainingDeck, firstCardDiscard, currentRound = 1)
  }

  // Behaviors during player turn

  private def waitingStart(gameData: GameData): Behavior[GameCoordinatorMessage] = {
    Behaviors.receivePartial {
      case (ctx, GCMsg.StartGame()) =>
        ctx.log.info("GameLogic Actor started")
        ctx.log.info(s"Game has started with this data:\n${gameData.game}")
        gameData.viewReference ! DGVMsg.StartGame(gameData.game, ctx.self)
        revealingSection(gameData, cardSeenRemaining = Game.cardsInitialVisible)
    }
  }

  // START of Behaviors - states

  // FIRST PHASE - player watch two of own cards
  private def revealingSection(gameData: GameData, cardSeenRemaining: Int): Behavior[GameCoordinatorMessage] = {
    log.log(s"GCoord actor of ${gameData.playerOwnUserID}, watchOwnCardsPhase called with cardSeenRemaining $cardSeenRemaining")

    if cardSeenRemaining <= 0 then
      println(s"GCoord actor of ${gameData.playerOwnUserID}, sending StartPlayPhase to ${gameData.viewReference}")
      gameData.clientReference ! CLMsg.IntialPhaseCompleted(gameData.turnLog)
      gameData.viewReference ! DGVMsg.WaitAfterRevealingSection()
      waitStartPlayCycle(gameData)
    else
      Behaviors.receivePartial {
        handleShowOwnNthCard(gameData, revealingSection(_, cardSeenRemaining - 1))
          .orElse(handleSendGameStatus(gameData, revealingSection(_, cardSeenRemaining)))
      }
  }

  // WAIT OTHERS HAVE SEEN CARDS
  private def waitStartPlayCycle(gameData: GameData): Behavior[GameCoordinatorMessage] = {
    Behaviors.receivePartial {
      handleSendGameStatus(gameData, waitStartPlayCycle)
        .orElse({
          case (ctx, GCMsg.StartPlayCycle()) =>
            val firstPlayer = gameData.game.players.find(_.rank == 1).get.userID
            gameData.viewReference ! DGVMsg.StartTurnPlayer(firstPlayer)
            if gameData.playerOwnUserID == firstPlayer then
              myTurnBeforeDraw(gameData.copy(turnLog = new PlayCycleTurnLog(gameData.playerOwnUserID, 1)))
            else
              notMyTurn(gameData.copy(turnLog = new PlayCycleTurnLog(gameData.playerOwnUserID, 1)))
        })
    }
  }

  // BEFORE DRAW

  private def myTurnBeforeDraw(gameData: GameData): Behavior[GameCoordinatorMessage] = Behaviors.setup { ctx =>
    Behaviors.withTimers { timers =>
      // todo: handle offset seconds, gamecoordinator's timer should be longer than view's timer
      ctx.log.info("Setting timer")
      val timeForTurn = (gameData.game.gameParameters.maxTimeRound + 5).seconds
      timers.startSingleTimer(GCMsg.TurnTimeEnded(), timeForTurn)
      Behaviors.receivePartial {
        handleDrawCardFromDeck(gameData)
          .orElse(handleDrawCardFromDiscardStack(gameData))
          .orElse(handleTurnTimeEnded(gameData))
          .orElse(handleSendGameStatus(gameData, myTurnBeforeDraw))
      }
    }
  }

  // AFTER DRAW

  private def myTurnAfterDrawNoPower(gameData: GameData, cardInHand: Card): Behavior[GameCoordinatorMessage] = Behaviors.receivePartial {
    handleDiscardCardDrawn(gameData, cardInHand)
      .orElse(handleDiscardOwnNthCard(gameData, cardInHand))
      .orElse(handleTurnTimeEnded(gameData))
      .orElse(handleSendGameStatus(gameData, myTurnAfterDrawNoPower(_, cardInHand)))
  }

  private def myTurnAfterDrawPower(gameData: GameData, cardInHand: Card): Behavior[GameCoordinatorMessage] = Behaviors.receivePartial {
    (cardInHand.power match
      case Power.SeeYourCard() => handleShowOwnNthCard(gameData, myTurnAfterDrawNoPower(_, cardInHand))
      case Power.SeeYourOpponentCard() => handleShowAdversaryNthCard(gameData, myTurnAfterDrawNoPower(_, cardInHand))
      case Power.ChangeOneOfYourCardWithOpponent() => handleChangeAdversaryCardWithOwnNthCard(gameData, myTurnAfterDrawNoPower(_, cardInHand)))
      .orElse(handleTurnTimeEnded(gameData))
      .orElse(handleSendGameStatus(gameData, myTurnAfterDrawPower(_, cardInHand)))
  }

  private def myTurnAfterDrawFromDiscard(gameData: GameData, cardInHand: Card): Behavior[GameCoordinatorMessage] = Behaviors.receivePartial {
    // TODO: siccome una volta che si è presa la carta dalla pila degli scarti bisogna usarla,
    //       allo scadere del tempo una carta a caso verrà sostituita.
    //       Implementare questa cosa.
    handleDiscardOwnNthCard(gameData, cardInHand)
      .orElse(handleTurnTimeEnded(gameData))
      .orElse(handleSendGameStatus(gameData, myTurnAfterDrawFromDiscard(_, cardInHand)))
  }

  // AFTER DISCARD

  private def myTurnAfterDiscard(gameData: GameData): Behavior[GameCoordinatorMessage] = Behaviors.receivePartial {
    handleSendGameStatus(gameData, myTurnAfterDiscard)
      .orElse(handleTurnTimeEnded(gameData))
      .orElse({
        case (ctx, GCMsg.EndTurn()) =>
          gameData.turnLog.addEvent(TurnEvent.EndTurn())
          val newGameData = gameData.syncAllTemporaryDecks
          gameData.clientReference ! CLMsg.TurnEnded(newGameData.game, gameData.turnLog)
          updateNewTurn(newGameData.game, newGameData.turnLog, newGameData)
        case (ctx, GCMsg.CallCabo()) =>
          gameData.turnLog.addEvent(TurnEvent.CaboCalled())
          val tempGame = gameData.temporaryGame.copy(caboState = Some(gameData.getSelfPlayer))
          val newGameData = gameData.copy(temporaryGame = tempGame).syncAllTemporaryDecks
          gameData.clientReference ! CLMsg.TurnEnded(newGameData.game, gameData.turnLog)
          updateNewTurn(newGameData.game, newGameData.turnLog, newGameData)
      })
  }

  // NOT MY TURN

  private def notMyTurn(gameData: GameData): Behavior[GameCoordinatorMessage] = Behaviors.receivePartial {
    log.log(s"notMyTurn called")
    handleSendGameStatus(gameData, notMyTurn)
      .orElse({
        case (_, GCMsg.TurnTimeEnded()) => Behaviors.same
        case (_, GCMsg.EndTurn()) => Behaviors.same
      })
      .orElse(handleNewTurnOrEmptyTurn(gameData))
  }

  //
  private def gameEnded(gameData: GameData): Behavior[GameCoordinatorMessage] =
    log.log(s"gameEnded called")
    Behaviors.receivePartial {
      case (ctx, msg) =>
        log.log("gameEnded - Received $msg")
        Behaviors.same
    }

  // END of Behaviors - states

  // Handlers during player turn

  private def handleDrawCardFromDeck(
                                      gameData: GameData
                                    ): PartialFunction[(ActorContext[GameCoordinatorMessage], GameCoordinatorMessage), Behavior[GameCoordinatorMessage]] = {
    case (ctx, GCMsg.DrawCardFromDeck()) =>
      val (topCard, newDeck) = gameData.game.deckStack.drawFirstCard
      ctx.log.info(s"I draw $topCard from deck")
      gameData.turnLog.addEvent(TurnEvent.DrawCardFromDeck(topCard))
      gameData.viewReference ! DGVMsg.CardDrawn(topCard)
      val newTempGame = gameData.temporaryGame.copy(deckStack = newDeck)
      if topCard.power != Power.NoPower() then
        myTurnAfterDrawPower(gameData.copy(temporaryGame = newTempGame), cardInHand = topCard)
      else
        myTurnAfterDrawNoPower(gameData.copy(temporaryGame = newTempGame), cardInHand = topCard)
  }

  private def handleDrawCardFromDiscardStack(
                                              gameData: GameData
                                            ): PartialFunction[(ActorContext[GameCoordinatorMessage], GameCoordinatorMessage), Behavior[GameCoordinatorMessage]] = {
    case (ctx, GCMsg.DrawCardFromDiscardStack()) =>
      val (topCard, newDiscardStack) = gameData.game.discardDeckStack.drawFirstCard
      ctx.log.info(s"I draw $topCard from discard stack")
      gameData.turnLog.addEvent(TurnEvent.DrawCardFromDiscardStack(topCard))
      gameData.viewReference ! DGVMsg.CardDrawn(topCard)
      if newDiscardStack.cards.isEmpty then
        gameData.viewReference ! DGVMsg.EmptyDiscardStack()
      else
        gameData.viewReference ! DGVMsg.NewTopCardDiscardStack(newDiscardStack.cards.head)
      val newTempGame = gameData.temporaryGame.copy(discardDeckStack = newDiscardStack)
      myTurnAfterDrawFromDiscard(gameData.copy(temporaryGame = newTempGame), topCard)
  }

  private def handleDiscardCardDrawn(
                                      gameData: GameData,
                                      cardInHand: Card
                                    ): PartialFunction[(ActorContext[GameCoordinatorMessage], GameCoordinatorMessage), Behavior[GameCoordinatorMessage]] = {
    case (ctx, GCMsg.DiscardCardDrawn()) =>
      ctx.log.info(s"handleDiscardCardDrawn - I discard the drawn card $cardInHand")
      gameData.turnLog.addEvent(TurnEvent.CardDrawnDiscarded(cardInHand))
      val newTemporaryGame = gameData.temporaryGame.copy(discardDeckStack = gameData.temporaryGame.discardDeckStack.addTopCard(cardInHand))
      val newGameData = gameData.copy(temporaryGame = newTemporaryGame)
      gameData.viewReference ! DGVMsg.NewTopCardDiscardStack(cardInHand)
      myTurnAfterDiscard(newGameData)
  }

  private def handleDiscardOwnNthCard(
                                       gameData: GameData,
                                       cardInHand: Card
                                     ): PartialFunction[(ActorContext[GameCoordinatorMessage], GameCoordinatorMessage), Behavior[GameCoordinatorMessage]] = {
    case (ctx, GCMsg.DiscardYourNthCard(index)) =>
      val oldHand = gameData.getOurHand
      ctx.log.info(s"handleDiscardOwnNthCard - I discard the card with index $index")
      ctx.log.info(s"handleDiscardOwnNthCard - The card is ${oldHand.cards(index)}")
      gameData.turnLog.addEvent(TurnEvent.OwnCardDiscarded(oldHand.cards(index), index))
      val newHand = Hand(oldHand.cards.updated(index, cardInHand))
      val gameStateAfterReplace = gameData.temporaryGame.replaceHandOfPlayerWithID(gameData.playerOwnUserID, newHand)
      val newTempGameState = gameStateAfterReplace.copy(discardDeckStack = gameData.temporaryGame.discardDeckStack.addTopCard(oldHand.cards(index)))
      ctx.log.info(s"My new hand is ${newTempGameState.getPlayerWithID(gameData.playerOwnUserID).hand}")
      gameData.viewReference ! DGVMsg.NewTopCardDiscardStack(oldHand.cards(index))
      myTurnAfterDiscard(gameData.copy(temporaryGame = newTempGameState))
  }

  private def handleSendGameStatus(
                                    gameData: GameData,
                                    nextBehaviors: GameData => Behavior[GameCoordinatorMessage]
                                  ): PartialFunction[(ActorContext[GameCoordinatorMessage], GameCoordinatorMessage), Behavior[GameCoordinatorMessage]] = {
    case (ctx, GCMsg.SendGameStatus(ref)) =>
      // todo: which keep?
      //ref ! GCMsg.GameInformation(gameData.game)
      ref ! GCMsg.GameInformation(gameData.temporaryGame)
      nextBehaviors(gameData)
  }

  private def handleNewTurnOrEmptyTurn(gameData: GameData): PartialFunction[(ActorContext[GameCoordinatorMessage], GameCoordinatorMessage), Behavior[GameCoordinatorMessage]] = {
    case (ctx, GCMsg.NewTurn(game, turnLog)) =>
      ctx.log.info(s"${gameData.playerOwnUserID} - NewTurn received!")
      gameData.clientReference ! CLMsg.TurnUpdated()
      updateNewTurn(game, turnLog, gameData)
    case (ctx, GCMsg.GetEmptyTurn(userID)) =>
      ctx.log.info(s"GetEmptyTurn received")
      val actualTurn = gameData.game.currentRound
      val gameTurnUpdated = gameData.game.copy(currentRound = actualTurn)
      val log = new PlayCycleTurnLog(userID, actualTurn)
      log.addEvent(TurnEvent.JumpTurnForDisconnection())
      gameData.clientReference ! CLMsg.TurnEnded(gameTurnUpdated, log)
      Behaviors.same
  }

  // POWERS implementation

  private def updateNewTurn(game: GameInProgress, turnLog: TurnLog, gameData: GameData) = {
    val gameDataTempUpdated = gameData.copy(temporaryGame = game)
    val actualTurn = game.currentRound + 1
    val actualGame = game.copy(currentRound = actualTurn)
    val newGameData = gameDataTempUpdated.copy(
      game = actualGame,
      temporaryGame = actualGame,
      turnLog = new PlayCycleTurnLog(gameData.playerOwnUserID, actualTurn))

    val playerIDHaveToPlay = getPlayerIDWhoHasToPlay(actualGame)
    val isGameEnded = gameEnded(actualGame, playerIDHaveToPlay)
    val isMyTurnNext = playerIDHaveToPlay == gameData.playerOwnUserID
    println(s"Next player who play is $playerIDHaveToPlay, isMyTurnNext = $isMyTurnNext")
    isGameEnded match
      case NotEnded() =>
        newGameData.viewReference ! DGVMsg.LastTurnPlayed(turnLog, actualGame)
        newGameData.viewReference ! DGVMsg.StartTurnPlayer(playerIDHaveToPlay)
        if isMyTurnNext then
          myTurnBeforeDraw(newGameData)
        else
          notMyTurn(newGameData)
      case _ => transitionToShowingResults(isGameEnded, newGameData, turnLog)
  }

  private def handleShowOwnNthCard(
                                    gameData: GameData,
                                    nextBehaviors: GameData => Behavior[GameCoordinatorMessage]
                                  ): PartialFunction[(ActorContext[GameCoordinatorMessage], GameCoordinatorMessage), Behavior[GameCoordinatorMessage]] = {
    case (ctx, GCMsg.ShowYourNthCard(index)) =>
      log.log("GCActor - handleShowOwnNthCard - received ShowYourNthCard")
      ctx.log.info(s"I show the card with index $index")
      gameData.turnLog.addEvent(TurnEvent.SeeSelfCard(index))
      baseShowCard(gameData, gameData.getOurHand.cards(index), () => nextBehaviors(gameData))
  }

  private def handleShowAdversaryNthCard(
                                          gameData: GameData,
                                          nextBehaviors: GameData => Behavior[GameCoordinatorMessage]
                                        ): PartialFunction[(ActorContext[GameCoordinatorMessage], GameCoordinatorMessage), Behavior[GameCoordinatorMessage]] = {
    case (ctx, GCMsg.ShowAdversaryNthCard(playerID, cardIndex)) =>
      ctx.log.info(s"I show the card with index $cardIndex of player with index $playerID")
      gameData.turnLog.addEvent(TurnEvent.SeeAdversaryCard(playerID, cardIndex))
      baseShowCard(gameData, gameData.getHandOPlayerWithID(playerID).cards(cardIndex), () => nextBehaviors(gameData))
  }

  private def baseShowCard(gameData: GameData, card: Card, nextBehavior: () => Behavior[GameCoordinatorMessage]) = {
    gameData.viewReference ! DGVMsg.CardSeen(card)
    nextBehavior()
  }


  private def handleChangeAdversaryCardWithOwnNthCard(
                                                       gameData: GameData,
                                                       nextBehaviors: GameData => Behavior[GameCoordinatorMessage]
                                                     ): PartialFunction[(ActorContext[GameCoordinatorMessage], GameCoordinatorMessage), Behavior[GameCoordinatorMessage]] = {

    case (ctx, GCMsg.ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndex, adversaryID, adversaryCardIndex)) =>
      ctx.log.info(s"I change the card with index $ownCardIndex of player with index $adversaryID with my card with index $adversaryCardIndex")
      gameData.turnLog.addEvent(TurnEvent.ReplaceOwnCardWithAdversaryCard(ownCardIndex, adversaryID, adversaryCardIndex))
      val ownOldCard = gameData.getOurHand.cards(ownCardIndex)
      val ownNewCard = gameData.getHandOPlayerWithID(adversaryID).cards(adversaryCardIndex)

      ctx.log.info(s"ownOldCard $ownOldCard, ownNewCard $ownNewCard")

      val newGameState = gameData.temporaryGame
        .replaceNthCardOfPlayerWithID(gameData.playerOwnUserID, ownNewCard, ownCardIndex)
        .replaceNthCardOfPlayerWithID(adversaryID, ownOldCard, adversaryCardIndex)

      ctx.log.info(s"New game state: $newGameState")

      gameData.viewReference ! DGVMsg.ChangeCardWithAdversaryAck()

      nextBehaviors(gameData.copy(temporaryGame = newGameState))
  }

  // END POWERS implementation

  private def handleTurnTimeEnded(
                                   gameData: GameData
                                 ): PartialFunction[(ActorContext[GameCoordinatorMessage], GameCoordinatorMessage), Behavior[GameCoordinatorMessage]] = {
    case (ctx, GCMsg.TurnTimeEnded()) =>
      ctx.log.info(s"Turn time ended for player ${gameData.playerOwnUserID}, initializing game at before draw state")
      val newTurnLogJumping = PlayCycleTurnLog(gameData.turnLog.playerName, gameData.turnLog.round)
      newTurnLogJumping.addEvent(TurnEvent.JumpTurnForTimerEnded())
      val newGameData = gameData.copy(temporaryGame = gameData.game, turnLog = newTurnLogJumping)
      gameData.viewReference ! DGVMsg.EndTurnByTimeEnded()
      gameData.clientReference ! CLMsg.TurnEnded(newGameData.game, newGameData.turnLog)
      updateNewTurn(newGameData.game, newGameData.turnLog, newGameData)
  }

  private def transitionToShowingResults(gameEnd: EndingGame, gameData: GameData, lastTurnLog: TurnLog): Behavior[GameCoordinatorMessage] = {
    log.log(s"transitionToShowingResults called, gameData = $gameData")
    //    val finalGameState = gameData.temporaryGame
    gameData.viewReference ! DGVMsg.LastTurnPlayed(lastTurnLog, gameData.game)
    gameEnd match
      case EndedByCabo() => gameData.viewReference ! DGVMsg.GameEndedByCabo(gameData.game)
      case EndedByTurnsLimit() => gameData.viewReference ! DGVMsg.GameEndedByTurnsLimit(gameData.game)
      case EndedByEmptyDeck() => gameData.viewReference ! DGVMsg.GameEndedByEmptyDeck(gameData.game)
    gameEnded(gameData)
  }

  // SUPPORT FUNCTIONS

  private def getPlayerIDWhoHasToPlay(actualGame: GameInProgress): String = {
    val rankWhoPlay = ((actualGame.currentRound - 1) % actualGame.players.size) + 1
    actualGame.players.find(_.rank == rankWhoPlay) match {
      case Some(player) => player.userID
      case None => throw new IllegalArgumentException(s"No player found with rank $rankWhoPlay in the game players")
    }
  }

  private def startTurnTimer(value: ActorContext[GameCoordinatorMessage], data: GameCoordinatorActor.GameData) = {
    Behaviors.withTimers { timers =>
      timers.startTimerAtFixedRate(
        GCMsg.EndTurn(),
        scala.concurrent.duration.FiniteDuration(
          data.game.gameParameters.maxTimeRound,
          scala.concurrent.duration.SECONDS
        )
      )
      Behaviors.same
    }
  }

