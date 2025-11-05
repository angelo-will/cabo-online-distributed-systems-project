package controller

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import model.Suit.Spades
import model.{Card, DuringGameTurnLog, GameParameters, Hand, InitialPhaseTurnLog, PlayerPlaying, Power, TurnEvent, TurnLog}
import model.Game.{GameInConstruction, GameInProgress}
import model.TurnEvent.CardDiscarded
import utils.AppLogger
import utils.ClientMessages as CLMsg
import utils.ClientMessages.ClientCommand as CCommand
import utils.DuringGameViewMessages as DGVMsg
import utils.GameCoordinatorMessage as GCMsg
//import utils.InitialViewMessages.ViewCommand

object GameCoordinatorActor:

  import akka.actor.typed.ActorRef

  import utils.Message
  import model.CardStack
  import model.GameStatus
  import model.Game

  private case class GameData(
                               clientReference: ActorRef[CCommand],
                               // todo: change type in ViewCommand moving the messages of GameCoordinatorMessage in ViewMessages?
                               viewReference: ActorRef[Message],
                               playerOwnRank: Int,
                               playerOwnUserID: String,
                               game: GameInProgress,
                               temporaryGame: GameInProgress,
                               turnLog: TurnLog,
                               //                               temporaryDeck: CardStack,
                               //                               temporaryDiscardDeck: CardStack
                             ):
    def getOurHand: Hand = this.getSelfPlayer.hand

    def getHandOPlayerWithID(playerID: String): Hand = this.getPlayerWithID(playerID).hand

    def getSelfPlayer: PlayerPlaying = this.game.getPlayerWithID(this.playerOwnUserID)

    def getPlayerWithID(playerID: String): PlayerPlaying = this.game.getPlayerWithID(playerID)

    def syncAllTemporaryDecks: GameData =
      //      val newGameState = game.copy(deckStack = temporaryDeck, discardDeckStack = temporaryDiscardDeck)
      //      this.copy(game = newGameState)
      this.copy(game = temporaryGame)

    override def toString: String =
      "GameData: \n" +
        "whoToSendResponse=" + viewReference + "\n" +
        "playerRank=" + playerOwnRank + "\n" +
        "game=" + game + "\n" +
        //        "temporaryDeck=" + temporaryDeck + "\n" +
        //        "temporaryDiscardDeck=" + temporaryDiscardDeck + "\n"
        "temporaryGame=" + temporaryGame + "\n" +
        "turnLog=" + turnLog + "\n"

  private val log = AppLogger.Log(true)

  def apply(client: ActorRef[CCommand], viewToContact: ActorRef[Message], userId: String, gameToStart: GameInConstruction): Behavior[Message] = {

    val game = generateGameInProgressFromInConstruction(gameToStart)

    client ! CLMsg.TakeGetInProgressGame(game)

    apply(client, viewToContact, userId, game)
  }

  def apply(client: ActorRef[CCommand], viewToContact: ActorRef[Message], userId: String, gameInProgress: GameInProgress): Behavior[Message] = {

    val playerRank = gameInProgress.players.find(_.userID == userId) match {
      case Some(player) => player.rank
      //todo - remove exception
      case None => throw new IllegalArgumentException(s"User ID $userId not found in the game players")
    }

    val gameData = GameData(
      client,
      viewToContact,
      playerRank,
      userId,
      game = gameInProgress,
      temporaryGame = gameInProgress,
      new InitialPhaseTurnLog(userId),
      //      gameInProgress.deckStack,
      //      gameInProgress.discardDeckStack
    )

    waitingStart(gameData)
  }

  private def generateGameInProgressFromInConstruction(gameInConstruction: GameInConstruction): GameInProgress = {

    var fullDeckShuffled = CardStack.buildShuffledFullDeck
    val playersPlaying = gameInConstruction.players.zipWithIndex.map((p, index) => {
      //test if it works
      val (hand, remainingDeck) = fullDeckShuffled.drawNCards(4)
      fullDeckShuffled = remainingDeck
      log.log(s"player ${p.userID} has rank ${index + 1}")
      PlayerPlaying(p.userID, p.name, index + 1, Hand(hand))
    })
    val (firstCardDiscard, remainingDeck) = fullDeckShuffled.drawNCards(1)

    GameInProgress(
      gameInConstruction.code,
      gameInConstruction.gameParameters,
      GameStatus.InProgress(),
      playersPlaying,
      remainingDeck,
      firstCardDiscard,
      //todo: 0 or 1?
      currentRound = 1
    )
  }

  // Behaviors during player turn

  private def waitingStart(gameData: GameData): Behavior[Message] = {
    Behaviors.receivePartial {
      case (ctx, GCMsg.StartGame()) =>
        ctx.log.info("GameLogic Actor started")
        gameData.viewReference ! DGVMsg.StartGame(gameData.game, ctx.self)
        watchOwnCardsPhase(gameData, cardSeenRemaining = Game.cardsInitialVisible)
    }
  }

  // START of Behaviors - states

  // FIRST PHASE - player watch two of own cards
  private def watchOwnCardsPhase(gameData: GameData, cardSeenRemaining: Int): Behavior[Message] =
    log.log(s"GCoord actor of ${gameData.playerOwnUserID}, watchOwnCardsPhase called with cardSeenRemaining $cardSeenRemaining")

    if cardSeenRemaining <= 0 then
      println(s"GCoord actor of ${gameData.playerOwnUserID}, sending StartPlayPhase to ${gameData.viewReference}")
      gameData.clientReference ! CLMsg.RevealingCardsPhaseLog(gameData.turnLog)
      gameData.viewReference ! DGVMsg.WaitAfterRevealingSection()
      waitOtherHaveSeenOwnCard(gameData)
    else
      Behaviors.receivePartial {
        handleShowOwnNthCard(gameData, watchOwnCardsPhase(_, cardSeenRemaining - 1))
          .orElse(handleSendGameStatus(gameData, watchOwnCardsPhase(_, cardSeenRemaining)))
      }

  // WAIT OTHERS HAVE SEEN CARDS
  private def waitOtherHaveSeenOwnCard(gameData: GameData): Behavior[Message] = {
    Behaviors.receivePartial {
      handleSendGameStatus(gameData, waitOtherHaveSeenOwnCard)
        .orElse({
          case (ctx, GCMsg.StartPlayCycle()) =>
            if gameData.playerOwnRank == 1 then
              gameData.viewReference ! DGVMsg.FirstTurn()
              myTurnBeforeDraw(gameData.copy(turnLog = new DuringGameTurnLog(gameData.playerOwnUserID, 1)))
            else
              //TODO: insert player who play first
              gameData.viewReference ! DGVMsg.PlayerIsPlaying(null)
              notMyTurn(gameData.copy(turnLog = new DuringGameTurnLog(gameData.playerOwnUserID, 1)))
        })
    }
  }

  // BEFORE DRAW

  private def myTurnBeforeDraw(gameData: GameData): Behavior[Message] = Behaviors.receivePartial {
    handleDrawCardFromDeck(gameData)
      .orElse(handleDrawCardFromDiscardStack(gameData))
      //.orElse(handleShowOwnNthCard(gameData, myTurnBeforeDraw))
      .orElse(handleSendGameStatus(gameData, myTurnBeforeDraw))
  }

  // AFTER DRAW

  private def myTurnAfterDrawNoPower(gameData: GameData, cardInHand: Card): Behavior[Message] = Behaviors.receivePartial {
    handleDiscardCardDrawn(gameData, cardInHand)
      //.orElse(handleShowOwnNthCard(gameData, myTurnAfterDrawNoPower(_, cardInHand)))
      .orElse(handleDiscardOwnNthCard(gameData, cardInHand))
      .orElse(handleSendGameStatus(gameData, myTurnAfterDrawNoPower(_, cardInHand)))
  }

  private def myTurnAfterDrawWithPower(gameData: GameData, cardInHand: Card): Behavior[Message] = Behaviors.receivePartial {
    (cardInHand.power match
      case Power.SeeYourCard() => handleShowOwnNthCard(gameData, myTurnAfterDrawNoPower(_, cardInHand))
      case Power.SeeYourOpponentCard() => handleShowAdversaryNthCard(gameData, myTurnAfterDrawNoPower(_, cardInHand))
      case Power.ChangeOneOfYourCardWithOpponent() => handleChangeAdversaryCardWithOwnNthCard(gameData, myTurnAfterDrawNoPower(_, cardInHand)))
      .orElse(handleSendGameStatus(gameData, myTurnAfterDrawWithPower(_, cardInHand)))
  }

  private def myTurnAfterDrawFromDiscard(gameData: GameData, cardInHand: Card): Behavior[Message] = Behaviors.receivePartial {
    // TODO: siccome una volta che si è presa la carta dalla pila degli scarti bisogna usarla,
    //       allo scadere del tempo una carta a caso verrà sostituita.
    //       Implementare questa cosa.
    handleDiscardOwnNthCard(gameData, cardInHand)
      .orElse(handleSendGameStatus(gameData, myTurnAfterDrawFromDiscard(_, cardInHand)))
  }

  // AFTER DISCARD

  private def myTurnAfterDiscard(gameData: GameData): Behavior[Message] = Behaviors.receivePartial {
    handleSendGameStatus(gameData, myTurnAfterDiscard)
      //      .orElse(handleShowOwnNthCard(gameData, myTurnAfterDiscard))
      .orElse({ case (ctx, GCMsg.EndTurn()) =>
        ctx.log.info(s"myTurnAfterDiscard, gameData = $gameData")
        val newGameData = gameData.syncAllTemporaryDecks
        gameData.clientReference ! CLMsg.TurnEnded(newGameData.game, gameData.turnLog)
        notMyTurn(newGameData)
      })
  }

  // NOT MY TURN

  private def notMyTurn(gameData: GameData): Behavior[Message] = Behaviors.receivePartial {
    log.log(s"notMyTurn called")
    handleSendGameStatus(gameData, notMyTurn)
      .orElse(handleNewTurn(gameData))
  }

  // END of Behaviors - states

  // Handlers during player turn

  private def handleDrawCardFromDeck(
                                      gameData: GameData
                                    ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GCMsg.DrawCardFromDeck()) =>
      val (topCard, newDeck) = gameData.game.deckStack.drawFirstCard
      ctx.log.info(s"I draw $topCard from deck")
      gameData.turnLog.addEvent(TurnEvent.DrawCardFromDeck(topCard))
      gameData.viewReference ! DGVMsg.CardDrawn(topCard)
      //
      //      if topCard.power != Power.NoPower() then
      //        myTurnAfterDrawWithPower(gameData.copy(temporaryDeck = newDeck), cardInHand = topCard)
      //      else
      //        myTurnAfterDrawNoPower(gameData.copy(temporaryDeck = newDeck), cardInHand = topCard)
      val newTempGame = gameData.temporaryGame.copy(deckStack = newDeck)
      if topCard.power != Power.NoPower() then
        myTurnAfterDrawWithPower(gameData.copy(temporaryGame = newTempGame), cardInHand = topCard)
      else
        myTurnAfterDrawNoPower(gameData.copy(temporaryGame = newTempGame), cardInHand = topCard)

  private def handleDrawCardFromDiscardStack(
                                              gameData: GameData
                                            ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GCMsg.DrawCardFromDiscardStack()) =>
      ctx.log.info(s"I draw a card from discard stack")

      val (topCard, newDiscardStack) = gameData.game.discardDeckStack.drawFirstCard
      gameData.turnLog.addEvent(TurnEvent.DrawCardFromDiscardStack(topCard))
      gameData.viewReference ! DGVMsg.CardDrawn(topCard)
      //
      //      myTurnAfterDrawFromDiscard(gameData.copy(temporaryDiscardDeck = newDiscardStack), topCard)
      val newTempGame = gameData.temporaryGame.copy(discardDeckStack = newDiscardStack)
      myTurnAfterDrawFromDiscard(gameData.copy(temporaryGame = newTempGame), topCard)

  private def handleDiscardCardDrawn(
                                      gameData: GameData,
                                      cardInHand: Card
                                    ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GCMsg.DiscardCardDrawn()) =>
      gameData.turnLog.addEvent(TurnEvent.CardDiscarded(cardInHand))
      //      val newGameState = gameData.game.copy(
      //        deckStack = gameData.temporaryDeck,
      //        discardDeckStack = gameData.temporaryDiscardDeck.addTopCard(cardInHand)
      //      )
      val newTemporaryGame = gameData.temporaryGame.copy(
        discardDeckStack = gameData.temporaryGame.discardDeckStack.addTopCard(cardInHand)
      )

      val newGameData = gameData.copy(temporaryGame = newTemporaryGame)
      ctx.log.info(s"handleDiscardCardDrawn - new game data = $newGameData")

      gameData.viewReference ! DGVMsg.NewTopCardDiscardStack(cardInHand)
      //      myTurnAfterDiscard(gameData.copy(game = newGameState, temporaryDiscardDeck = newGameState.discardDeckStack))
      myTurnAfterDiscard(newGameData)

  private def handleDiscardOwnNthCard(
                                       gameData: GameData,
                                       cardInHand: Card
                                     ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GCMsg.DiscardYourNthCard(index)) =>
      val oldHand = gameData.getOurHand
      ctx.log.info(s"handleDiscardOwnNthCard - I discard the card with index $index")
      ctx.log.info(s"handleDiscardOwnNthCard - The card is ${oldHand.cards(index)}")
      gameData.turnLog.addEvent(TurnEvent.CardDiscarded(oldHand.cards(index)))
      val newHand = Hand(oldHand.cards.updated(index, cardInHand))
      //
      //      val gameStateAfterReplace = gameData.game.replaceHandOfPlayerWithID(gameData.playerOwnUserID, newHand)
      val gameStateAfterReplace = gameData.temporaryGame.replaceHandOfPlayerWithID(gameData.playerOwnUserID, newHand)

      val newTempGameState = gameStateAfterReplace.copy(
        //        discardDeckStack = gameData.temporaryDiscardDeck.addTopCard(oldHand.cards(index)),
        discardDeckStack = gameData.temporaryGame.discardDeckStack.addTopCard(oldHand.cards(index)),
      )
      ctx.log.info(s"handleDiscardOwnNthCard - new temporary game: $newTempGameState")

      gameData.viewReference ! DGVMsg.NewTopCardDiscardStack(oldHand.cards(index))

      myTurnAfterDiscard(gameData.copy(temporaryGame = newTempGameState))

  private def handleSendGameStatus(
                                    gameData: GameData,
                                    nextBehaviors: GameData => Behavior[Message]
                                  ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GCMsg.SendGameStatus(ref)) =>
      ref ! DGVMsg.GameInformation(gameData.game)
      nextBehaviors(gameData)

  private def handleNewTurn(gameData: GameData): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    // TODO: implementare l'arrivo delle nuove informazioni e la sequenza dei passaggi fatti in un turno.
    //       Se un giocatore per problemi o altro non gioca non fa andare avanti il mazzo, quindi può arrivarmi un messaggio con niente
    case (ctx, GCMsg.NewTurn(game, turnLog)) =>
      ctx.log.info(s"NewTurn received, \ngameData = $gameData \ngameReceived = $game")
      //todo - send ack beck to client and update the view
      val gameDataTempUpdated = gameData.copy(temporaryGame = game)
      val actualTurn = game.currentRound + 1
      val actualGame = game.copy(currentRound = actualTurn)
      val x = gameDataTempUpdated.copy(temporaryGame = actualGame, turnLog = new DuringGameTurnLog(gameData.playerOwnUserID, actualTurn))
      val newGameData = x.syncAllTemporaryDecks
      if isMyTurn(newGameData, actualGame) then
        newGameData.viewReference ! DGVMsg.LastTurnPlayed(turnLog, actualGame, true)
        myTurnBeforeDraw(newGameData)
      else
        newGameData.viewReference ! DGVMsg.LastTurnPlayed(turnLog, actualGame, false)
        notMyTurn(newGameData)

  // POWERS implementation

  private def handleShowOwnNthCard(
                                    gameData: GameData,
                                    nextBehaviors: GameData => Behavior[Message]
                                  ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GCMsg.ShowYourNthCard(index)) =>
      log.log("GCActor - handleShowOwnNthCard - received ShowYourNthCard")
      ctx.log.info(s"I show the card with index $index")
      gameData.turnLog.addEvent(TurnEvent.SeeSelfCard(index))
      baseShowCard(gameData, gameData.getOurHand.cards(index), () => nextBehaviors(gameData))

  private def handleShowAdversaryNthCard(
                                          gameData: GameData,
                                          nextBehaviors: GameData => Behavior[Message]
                                        ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GCMsg.ShowAdversaryNthCard(playerID, cardIndex)) =>
      ctx.log.info(s"I show the card with index $cardIndex of player with index $playerID")
      gameData.turnLog.addEvent(TurnEvent.SeeAdversaryCard(playerID, cardIndex))
      baseShowCard(gameData, gameData.getHandOPlayerWithID(playerID).cards(cardIndex), () => nextBehaviors(gameData))

  private def baseShowCard(gameData: GameData, card: Card, nextBehavior: () => Behavior[Message]) =
    gameData.viewReference ! DGVMsg.CardSeen(card)
    nextBehavior()


  private def handleChangeAdversaryCardWithOwnNthCard(
                                                       gameData: GameData,
                                                       nextBehaviors: GameData => Behavior[Message]
                                                     ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {

    case (ctx, GCMsg.ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndex, adversaryID, adversaryCardIndex)) =>
      ctx.log.info(s"I change the card with index $ownCardIndex of player with index $adversaryID with my card with index $adversaryCardIndex")
      gameData.turnLog.addEvent(TurnEvent.ReplaceOwnCardWithAdversaryCard(ownCardIndex, adversaryID, adversaryCardIndex))
      val ownOldCard = gameData.getOurHand.cards(ownCardIndex)
      val ownNewCard = gameData.getHandOPlayerWithID(adversaryID).cards(adversaryCardIndex)

      ctx.log.info(s"ownOldCard $ownOldCard, ownNewCard $ownNewCard")

      val newGameState = gameData.game
        .replaceNthCardOfPlayerWithID(gameData.playerOwnUserID, ownNewCard, ownCardIndex)
        .replaceNthCardOfPlayerWithID(adversaryID, ownOldCard, adversaryCardIndex)

      ctx.log.info(s"New game state: $newGameState")

      gameData.viewReference ! DGVMsg.ChangeCardWithAdversaryAck()

      nextBehaviors(gameData.copy(game = newGameState))
  }

  // END POWERS implementation

  // SUPPORT FUNCTIONS
  private def isMyTurn(gameData: GameData, actualGame: GameInProgress): Boolean = {
    val rankWhoPlay = ((actualGame.currentRound - 1) % actualGame.players.size) + 1
    print(s"isMyTurn called, rankWhoPlay = $rankWhoPlay, gameData = $gameData, actualGame = $actualGame")
    gameData.playerOwnRank == rankWhoPlay
  }
      
