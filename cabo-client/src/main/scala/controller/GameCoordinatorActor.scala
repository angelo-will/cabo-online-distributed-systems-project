package controller

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import model.Suit.Spades
import model.{Card, GameParameters, Hand, PlayerPlaying, Power}
import model.Game.GameInProgress

object GameCoordinatorActor:

  import akka.actor.typed.ActorRef

  import utils.Message
  import utils.GameCoordinatorMessage
  import model.CardStack
  import model.GameStatus
  import model.Game

  private case class GameData(
                               whoToSendResponse: ActorRef[Message],
                               playerRank: Int,
                               game: GameInProgress,
                               temporaryDeck: CardStack,
                               temporaryDiscardDeck: CardStack
                             ):
    def getOurHand: Hand = getHandOfNthPlayer(playerRank)

    def getHandOfNthPlayer(index: Int): Hand = game.players(index).hand

    def syncAllTemporaryDecks: GameData =
      val newGameState = game.copy(deckStack = temporaryDeck, discardDeckStack = temporaryDiscardDeck)
      this.copy(game = newGameState)

    override def toString: String =
      "GameData: \n" +
        "whoToSendResponse=" + whoToSendResponse + "\n" +
        "playerRank=" + playerRank + "\n" +
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

  // Behaviors during player turn

  private def generateGameData(whoToSendResponse: ActorRef[Message], playerRank: Int) = {
    // debug values, emulate shuffled deck and the use of the first card as firs of discard stack
    //    val fullDeckShuffled = CardStack.buildShuffledFullDeck
    val fullDeckShuffled = CardStack.buildSortedFullDeck
    val (handPlayer01, remainingDeck01) = fullDeckShuffled.drawNCards(4)
    val (handPlayer02, remainingDeck02) = remainingDeck01.drawNCards(4)
    val (topCardDiscardStack, deckToStartTheGame) = remainingDeck02.drawFirstCard
    val discardStack = CardStack(List(topCardDiscardStack))
    GameData(
      whoToSendResponse,
      playerRank,
      GameInProgress(
        "gameCode",
        GameParameters(maxTimeRound = 5),
        GameStatus.InProgress(),
        List(
          PlayerPlaying("player01", "name01", "not-valid-address", Hand(handPlayer01)),
          PlayerPlaying("player02", "name02", "not-valid-address", Hand(handPlayer02)),
        ),
        deckToStartTheGame,
        discardStack,
        0
      ),
      deckToStartTheGame,
      discardStack
    )
  }

  // START of Behaviors - states

  // FIRST PHASE - player watch two of own cards
  private def watchOwnCardsPhase(gameData: GameData, cardSeenRemaining: Int): Behavior[Message] =
    if cardSeenRemaining <= 0 then myTurnBeforeDraw(gameData)
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
    handleDiscardCard(gameData, cardInHand)
      //.orElse(handleShowOwnNthCard(gameData, myTurnAfterDrawNoPower(_, cardInHand)))
      .orElse(handleDiscardNthCard(gameData, cardInHand))
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
    handleDiscardNthCard(gameData, cardInHand)
      .orElse(handleSendGameStatus(gameData, myTurnAfterDrawFromDiscard(_, cardInHand)))
  }

  // AFTER DISCARD

  private def myTurnAfterDiscard(gameData: GameData): Behavior[Message] = Behaviors.receivePartial {
    handleSendGameStatus(gameData, myTurnAfterDiscard)
      .orElse(handleShowOwnNthCard(gameData, myTurnAfterDiscard))
      .orElse({ case (ctx, GameCoordinatorMessage.EndTurn()) =>
        ctx.log.info(s"myTurnAfterDiscard, gameData = $gameData")
        // TODO: send to other players the new status of the game
        // send to other atcual status
        // [...]
        // to change then
        gameData.whoToSendResponse ! GameCoordinatorMessage.GameInformation(gameData.game)
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
                                      //                                      nextBehaviors: (ActorRef[Message], GameInProgress) => Behavior[Message]
                                    ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DrawCardFromDeck()) =>
      val (topCard, newDeck) = gameData.game.deckStack.drawFirstCard
      ctx.log.info(s"I draw $topCard from deck")


      gameData.whoToSendResponse ! GameCoordinatorMessage.CardDrawn(topCard)
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

      gameData.whoToSendResponse ! GameCoordinatorMessage.CardDrawn(topCard)
      //      val newGameState = gameData.game.copy(discardDeckStack = newDiscardStack)
      myTurnAfterDrawFromDiscard(gameData.copy(temporaryDiscardDeck = newDiscardStack), topCard)

  private def handleDiscardCard(
                                 gameData: GameData,
                                 cardInHand: Card
                               ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DiscardCardDrawn()) =>
      ctx.log.info(s"I discard the card drawn")

      val newGameState = gameData.game.copy(
        deckStack = gameData.temporaryDeck,
        discardDeckStack = gameData.temporaryDiscardDeck.addTopCard(cardInHand)
      )

      println(s"New game state: $newGameState")

      gameData.whoToSendResponse ! GameCoordinatorMessage.NewTopCardDiscardStack(cardInHand)
      myTurnAfterDiscard(gameData.copy(game = newGameState, temporaryDiscardDeck = newGameState.discardDeckStack))

  private def handleDiscardNthCard(
                                    gameData: GameData,
                                    cardInHand: Card
                                  ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DiscardYourNthCard(index)) =>
      val oldHand = gameData.getOurHand

      ctx.log.info(s"I discard the card with index $index")
      ctx.log.info(s"The card is ${oldHand.cards(index)}")

      val newPlayerState = replaceCardNthOfPlayerWithNewCard(cardInHand, index, gameData.game.players(gameData.playerRank))
      val newPlayers = gameData.game.players.updated(gameData.playerRank, newPlayerState)
      val newDeck = gameData.temporaryDeck
      val newDiscardStack = gameData.temporaryDiscardDeck.addTopCard(oldHand.cards(index))

      val newGameState = gameData.game.copy(
        deckStack = newDeck,
        discardDeckStack = newDiscardStack,
        players = newPlayers
      )
      ctx.log.info(s"New game state: $newGameState")

      gameData.whoToSendResponse ! GameCoordinatorMessage.NewTopCardDiscardStack(oldHand.cards(index))

      myTurnAfterDiscard(gameData.copy(game = newGameState))

  private def handleSendGameStatus(
                                    gameData: GameData,
                                    nextBehaviors: GameData => Behavior[Message]
                                  ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.SendGameStatus(ref)) =>
      ref ! GameCoordinatorMessage.GameInformation(gameData.game)
      nextBehaviors(gameData)

  private def handleNewTurn(gameData: GameData): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    // TODO: implementare l'arrivo delle nuove informazioni e la sequenza dei passaggi fatti in un turno.
    //       Se un giocatore per problemi o altro non gioca non fa andare avanti il mazzo, quindi può arrivarmi un messaggio con niente
    case (ctx, GameCoordinatorMessage.NewTurn(game)) =>
      myTurnBeforeDraw(gameData.copy(game = game))

  // POWERS implementation

  private def handleShowOwnNthCard(
                                    gameData: GameData,
                                    nextBehaviors: GameData => Behavior[Message]
                                  ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.ShowYourNthCard(index)) =>
      ctx.log.info(s"I show the card with index $index")
      baseShowCard(gameData, gameData.getOurHand.cards(index), nextBehaviors(gameData))

  private def handleShowAdversaryNthCard(
                                          gameData: GameData,
                                          nextBehaviors: GameData => Behavior[Message]
                                        ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.ShowAdversaryNthCard(playerIndex, cardIndex)) =>
      ctx.log.info(s"I show the card with index $cardIndex of player with index $playerIndex")
      baseShowCard(gameData, gameData.getHandOfNthPlayer(playerIndex).cards(cardIndex), nextBehaviors(gameData))

  private def baseShowCard(gameData: GameData, card: Card, behavior: Behavior[Message]) =
    gameData.whoToSendResponse ! GameCoordinatorMessage.CardSeen(card)
    behavior


  private def handleChangeAdversaryCardWithOwnNthCard(
                                                       gameData: GameData,
                                                       nextBehaviors: GameData => Behavior[Message]
                                                     ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndex, adversaryIndex, adversaryCardIndex)) =>
      ctx.log.info(s"I change the card with index $ownCardIndex of player with index $adversaryIndex with my card with index $adversaryCardIndex")

      val ownOldCard = gameData.getOurHand.cards(ownCardIndex)
      val ownNewCard = gameData.getHandOfNthPlayer(adversaryIndex).cards(adversaryCardIndex)
      val stateAfterFirstChange = replaceCardNthOfNthPlayerWithNewCard(ownNewCard, ownCardIndex, gameData.playerRank, gameData.game.players)
      val lastChange = replaceCardNthOfNthPlayerWithNewCard(ownOldCard, adversaryCardIndex, adversaryIndex, stateAfterFirstChange)
      val newGameState = gameData.game.copy(
        players = lastChange
      )

      ctx.log.info(s"New game state: $newGameState")

      // TODO: inserire aggiornamento dei temporary deck
      //gameData.whoToSendResponse ! GameCoordinatorMessage
      nextBehaviors(gameData.copy(game = newGameState))

  // END POWERS implementation

  /**
   * Changes the card at the specified index in the player's hand with a new card.
   *
   * @param newCard           the new card to be placed at the specified index
   * @param cardToChangeIndex the index of the card to be replaced (0-indexed)
   * @param playerRank        the rank of the player whose hand is being modified
   * @param players           the list of players
   * @return a new list of players with the updated hand for the specified player
   */
  private def replaceCardNthOfNthPlayerWithNewCard(newCard: Card, cardToChangeIndex: Int, playerRank: Int, players: List[PlayerPlaying]) =
    val newPlayer = replaceCardNthOfPlayerWithNewCard(newCard, cardToChangeIndex, players(playerRank))
    players.updated(playerRank, newPlayer)

  /**
   * Replaces the card at the specified index in the player's hand with a new card.
   *
   * @param newCard           the new card to be placed at the specified index
   * @param cardIndexToChange the index of the card to be replaced (0-indexed)
   * @param player            the player whose hand is to be modified
   * @return a new PlayerPlaying with the updated hand
   */
  private def replaceCardNthOfPlayerWithNewCard(newCard: Card, cardIndexToChange: Int, player: PlayerPlaying) =
    player.copy(
      hand = player.hand.changeNthCard(cardIndexToChange, newCard)
    )

import model.*

@main
def test =
  val tenHearts = Card(Rank.Ten(), Suit.Hearts())
  println(tenHearts.power)