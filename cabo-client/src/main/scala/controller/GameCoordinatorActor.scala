package controller

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import model.Suit.Spades
import model.{Card, DuringGameTurnLog, GameParameters, Hand, InitialPhaseTurnLog, PlayerPlaying, Power, TurnEvent, TurnLog}
import model.Game.{GameInConstruction, GameInProgress}
import model.TurnEvent.CardDiscarded
import utils.ClientMessages
import utils.ClientMessages.*
import utils.InitialViewMessages.ViewCommand
import utils.DuringGameViewMessages

object GameCoordinatorActor:

  import akka.actor.typed.ActorRef

  import utils.Message
  import utils.GameCoordinatorMessage
  import model.CardStack
  import model.GameStatus
  import model.Game

  private case class GameData(
                               clientReference: ActorRef[ClientCommand],
                             // todo: change type in ViewCommand moving the messages of GameCoordinatorMessage in ViewMessages?
                               viewReference: ActorRef[Message],
                               playerOwnRank: Int,
                               playerOwnUserID: String,
                               game: GameInProgress,
                               turnLog: TurnLog,
                               temporaryDeck: CardStack,
                               temporaryDiscardDeck: CardStack
                              ):
    def getOurHand: Hand = this.getSelfPlayer.hand

    def getHandOPlayerWithID(playerID: String): Hand = this.getPlayerWithID(playerID).hand

    def getSelfPlayer: PlayerPlaying = this.game.getPlayerWithID(this.playerOwnUserID)

    def getPlayerWithID(playerID: String): PlayerPlaying = this.game.getPlayerWithID(playerID)

    def syncAllTemporaryDecks: GameData =
      val newGameState = game.copy(deckStack = temporaryDeck, discardDeckStack = temporaryDiscardDeck)
      this.copy(game = newGameState)

    override def toString: String =
      "GameData: \n" +
        "whoToSendResponse=" + viewReference + "\n" +
        "playerRank=" + playerOwnRank + "\n" +
        "game=" + game + "\n" +
        "temporaryDeck=" + temporaryDeck + "\n" +
        "temporaryDiscardDeck=" + temporaryDiscardDeck + "\n"

  /**
   * Creates the GameCoordinatorActor behavior.
   *
   * @param whoToSendResponse the ActorRef to send responses to
   * @param playerRank        the rank of the player (0-indexed)
   * @return the behavior of the GameCoordinatorActor
   * @throws IllegalArgumentException if 0 < playerRank < max player per game          
   */
  //todo - remove
  def apply(whoToSendResponse: ActorRef[Message], playerRank: Int): Behavior[Message] =
    if (playerRank < 0 || playerRank > Game.maxPlayersPerGame)
      throw new IllegalArgumentException(s"Player rank $playerRank is not valid, it must be between 0 and ${Game.maxPlayersPerGame}")
    else
      // TODO: implementare handshake fra i giocatori per iniziare la partita
      Behaviors.receivePartial {
        case (ctx, GameCoordinatorMessage.StartGame()) =>
          ctx.log.info("GameLogic Actor started")
          watchOwnCardsPhase(generateGameData(whoToSendResponse, playerRank), cardSeenRemaining = Game.cardsInitialVisible)
        //myTurnBeforeDraw(generateGameData(whoToSendResponse, playerRank))
      }

  def apply(client: ActorRef[ClientCommand], viewToContact: ActorRef[Message], userId: String, gameToStart: GameInConstruction): Behavior[Message] = {

    val game = generateGameInProgressFromInConstruction(gameToStart)

    client ! ClientMessages.TakeGetInProgressGame(game)
    
    apply(client, viewToContact, userId, game)
  }
  
  def apply(client: ActorRef[ClientCommand], viewToContact: ActorRef[Message], userId: String, gameInProgress: GameInProgress): Behavior[Message] = {

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
      gameInProgress,
      new InitialPhaseTurnLog(userId),
      gameInProgress.deckStack,
      gameInProgress.discardDeckStack
    )

    watchOwnCardsPhase(gameData, cardSeenRemaining = Game.cardsInitialVisible)
  }

  private def generateGameInProgressFromInConstruction(gameInConstruction: GameInConstruction): GameInProgress = {

    var fullDeckShuffled = CardStack.buildShuffledFullDeck

    GameInProgress(
      gameInConstruction.code,
      gameInConstruction.gameParameters,
      GameStatus.InProgress(),
      gameInConstruction.players.zipWithIndex.map((p, index) => {
        //test if it works
        val (hand, remainingDeck) = fullDeckShuffled.drawNCards(4)
        fullDeckShuffled = remainingDeck
        PlayerPlaying(p.userID, p.name, index, Hand(hand))
      }),
      fullDeckShuffled,
      CardStack.buildEmptyDeck,
      0
    )
  }

  // Behaviors during player turn

  //todo - remove
  private def generateGameData(whoToSendResponse: ActorRef[Message], playerRank: Int) = {
    // debug values, emulate shuffled deck and the use of the first card as firs of discard stack
    //    val fullDeckShuffled = CardStack.buildShuffledFullDeck
    val ownCode = "player01"
    val fullDeckShuffled = CardStack.buildSortedFullDeck
    val (handPlayer01, remainingDeck01) = fullDeckShuffled.drawNCards(4)
    val (handPlayer02, remainingDeck02) = remainingDeck01.drawNCards(4)
    val (topCardDiscardStack, deckToStartTheGame) = remainingDeck02.drawFirstCard
    val discardStack = CardStack(List(topCardDiscardStack))
    GameData(
      whoToSendResponse,
      whoToSendResponse,
      playerRank,
      ownCode,
      GameInProgress(
        "gameCode",
        GameParameters(maxTimeRound = 5),
        GameStatus.InProgress(),
        List(
          PlayerPlaying("player01", "name01", 0, Hand(handPlayer01)),
          PlayerPlaying("player02", "name02", 1, Hand(handPlayer02)),
        ),
        deckToStartTheGame,
        discardStack,
        0
      ),
      new InitialPhaseTurnLog(ownCode),
      deckToStartTheGame,
      discardStack
    )
  }

  // START of Behaviors - states

  // FIRST PHASE - player watch two of own cards
  private def watchOwnCardsPhase(gameData: GameData, cardSeenRemaining: Int): Behavior[Message] =
    if cardSeenRemaining <= 0 then myTurnBeforeDraw(gameData.copy(turnLog = new DuringGameTurnLog(gameData.playerOwnUserID)))
    else
      Behaviors.receivePartial {
        handleShowOwnNthCard(gameData, watchOwnCardsPhase(_, cardSeenRemaining - 1))
          .orElse(handleSendGameStatus(gameData, watchOwnCardsPhase(_, cardSeenRemaining)))
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
      .orElse({ case (ctx, GameCoordinatorMessage.EndTurn()) =>
        ctx.log.info(s"myTurnAfterDiscard, gameData = $gameData")
        // TODO: send to other players the new status of the game
        // send to other atcual status
        // [...]
        // to change then
//        gameData.whoToSendResponse ! GameCoordinatorMessage.GameInformation(gameData.game)
        gameData.clientReference ! ClientMessages.TurnEnded(gameData.syncAllTemporaryDecks.game, gameData.turnLog)
        notMyTurn(gameData)
      })
  }

  // NOT MY TURN

  private def notMyTurn(gameData: GameData): Behavior[Message] = Behaviors.receivePartial {
    //TODO: sistemare
    handleSendGameStatus(gameData, notMyTurn)
      .orElse(handleNewTurn(gameData))
  }

  // END of Behaviors - states

  // Handlers during player turn

  private def handleDrawCardFromDeck(
                                      gameData: GameData
                                    ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DrawCardFromDeck()) =>
      val (topCard, newDeck) = gameData.game.deckStack.drawFirstCard
      ctx.log.info(s"I draw $topCard from deck")
      gameData.turnLog.addEvent(TurnEvent.DrawCardFromDeck(topCard))
      gameData.viewReference ! DuringGameViewMessages.CardDrawn(topCard)
      if topCard.power != Power.NoPower() then
        myTurnAfterDrawWithPower(gameData.copy(temporaryDeck = newDeck), cardInHand = topCard)
      else
        myTurnAfterDrawNoPower(gameData.copy(temporaryDeck = newDeck), cardInHand = topCard)

  private def handleDrawCardFromDiscardStack(
                                              gameData: GameData
                                            ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DrawCardFromDiscardStack()) =>
      ctx.log.info(s"I draw a card from discard stack")

      val (topCard, newDiscardStack) = gameData.game.discardDeckStack.drawFirstCard
      gameData.turnLog.addEvent(TurnEvent.DrawCardFromDiscardStack(topCard))
      gameData.viewReference ! DuringGameViewMessages.CardDrawn(topCard)
      myTurnAfterDrawFromDiscard(gameData.copy(temporaryDiscardDeck = newDiscardStack), topCard)

  private def handleDiscardCardDrawn(
                                      gameData: GameData,
                                      cardInHand: Card
                                    ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DiscardCardDrawn()) =>
      ctx.log.info(s"I discard the card drawn")
      gameData.turnLog.addEvent(TurnEvent.CardDiscarded(cardInHand))
      val newGameState = gameData.game.copy(
        deckStack = gameData.temporaryDeck,
        discardDeckStack = gameData.temporaryDiscardDeck.addTopCard(cardInHand)
      )

      println(s"New game state: $newGameState")

      gameData.viewReference ! DuringGameViewMessages.NewTopCardDiscardStack(cardInHand)
      myTurnAfterDiscard(gameData.copy(game = newGameState, temporaryDiscardDeck = newGameState.discardDeckStack))

  private def handleDiscardOwnNthCard(
                                       gameData: GameData,
                                       cardInHand: Card
                                     ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DiscardYourNthCard(index)) =>
      val oldHand = gameData.getOurHand
      ctx.log.info(s"I discard the card with index $index")
      ctx.log.info(s"The card is ${oldHand.cards(index)}")
      gameData.turnLog.addEvent(TurnEvent.CardDiscarded(oldHand.cards(index)))
      val newHand = Hand(oldHand.cards.updated(index, cardInHand))
      val gameStateAfterReplace = gameData.game.replaceHandOfPlayerWithID(gameData.playerOwnUserID, newHand)

      val newGameState = gameStateAfterReplace.copy(
        deckStack = gameData.temporaryDeck,
        discardDeckStack = gameData.temporaryDiscardDeck.addTopCard(oldHand.cards(index)),
      )
      ctx.log.info(s"New game state: $newGameState")

      gameData.viewReference ! DuringGameViewMessages.NewTopCardDiscardStack(oldHand.cards(index))

      myTurnAfterDiscard(gameData.copy(game = newGameState))

  private def handleSendGameStatus(
                                    gameData: GameData,
                                    nextBehaviors: GameData => Behavior[Message]
                                  ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.SendGameStatus(ref)) =>
      ref ! DuringGameViewMessages.GameInformation(gameData.game)
      nextBehaviors(gameData)

  private def handleNewTurn(gameData: GameData): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    // TODO: implementare l'arrivo delle nuove informazioni e la sequenza dei passaggi fatti in un turno.
    //       Se un giocatore per problemi o altro non gioca non fa andare avanti il mazzo, quindi può arrivarmi un messaggio con niente
    case (ctx, GameCoordinatorMessage.NewTurn(game)) =>
      //todo - check if it's my turn or wait another one
      //todo - send ack beck to client and update the view
      ctx.log.info(s"New turn arrived, game = $game")
      gameData.clientReference ! ClientMessages.TurnUpdated()
      myTurnBeforeDraw(gameData.copy(game = game, turnLog = new DuringGameTurnLog(gameData.playerOwnUserID)))

  // POWERS implementation

  private def handleShowOwnNthCard(
                                    gameData: GameData,
                                    nextBehaviors: GameData => Behavior[Message]
                                  ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.ShowYourNthCard(index)) =>
      ctx.log.info(s"I show the card with index $index")
      gameData.turnLog.addEvent(TurnEvent.SeeSelfCard(index))
      baseShowCard(gameData, gameData.getOurHand.cards(index), nextBehaviors(gameData))

  private def handleShowAdversaryNthCard(
                                          gameData: GameData,
                                          nextBehaviors: GameData => Behavior[Message]
                                        ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.ShowAdversaryNthCard(playerID, cardIndex)) =>
      ctx.log.info(s"I show the card with index $cardIndex of player with index $playerID")
      gameData.turnLog.addEvent(TurnEvent.SeeAdversaryCard(playerID, cardIndex))
      baseShowCard(gameData, gameData.getHandOPlayerWithID(playerID).cards(cardIndex), nextBehaviors(gameData))

  private def baseShowCard(gameData: GameData, card: Card, behavior: Behavior[Message]) =
    gameData.viewReference ! DuringGameViewMessages.CardSeen(card)
    behavior


  private def handleChangeAdversaryCardWithOwnNthCard(
                                                       gameData: GameData,
                                                       nextBehaviors: GameData => Behavior[Message]
                                                     ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndex, adversaryID, adversaryCardIndex)) =>
      ctx.log.info(s"I change the card with index $ownCardIndex of player with index $adversaryID with my card with index $adversaryCardIndex")
      gameData.turnLog.addEvent(TurnEvent.ReplaceOwnCardWithAdversaryCard(ownCardIndex, adversaryID, adversaryCardIndex))
      val ownOldCard = gameData.getOurHand.cards(ownCardIndex)
      val ownNewCard = gameData.getHandOPlayerWithID(adversaryID).cards(adversaryCardIndex)

      ctx.log.info(s"ownOldCard $ownOldCard, ownNewCard $ownNewCard")

      val newGameState = gameData.game
        .replaceNthCardOfPlayerWithID(gameData.playerOwnUserID, ownNewCard, ownCardIndex)
        .replaceNthCardOfPlayerWithID(adversaryID, ownOldCard, adversaryCardIndex)

      ctx.log.info(s"New game state: $newGameState")


      nextBehaviors(gameData.copy(game = newGameState))

// END POWERS implementation
