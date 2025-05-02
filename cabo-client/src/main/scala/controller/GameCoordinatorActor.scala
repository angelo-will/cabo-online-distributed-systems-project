package controller

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import model.Suit.Spades
import model.{Card, GameParameters, Hand, PlayerPlaying}
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
                             )

  def apply(whoToSendResponse: ActorRef[Message], playerRank: Int): Behavior[Message] =
    // debug values, emulate shuffled deck and the use of the first card as firs of discard stack
    val fullDeckShuffled = CardStack.buildShuffledFullDeck
    //    val fullDeckShuffled = CardStack.buildSortedFullDeck
    val (handPlayer01, remainingDeck01) = fullDeckShuffled.drawNCards(4)
    val (handPlayer02, remainingDeck02) = remainingDeck01.drawNCards(4)
    val (topCardDiscardStack, deckToStartTheGame) = remainingDeck02.drawFirstCard
    val discardStack = CardStack(List(topCardDiscardStack))
    if (playerRank < 0 || playerRank > Game.maxPlayersPerGame)
      throw new IllegalArgumentException(s"Player rank $playerRank is not valid, it must be between 0 and ${Game.maxPlayersPerGame}")
    else
      Behaviors.setup { ctx =>
        ctx.log.info("GameLogic Actor started")
        myTurnBeforeDraw(GameData(
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
        ))
      }

  // Behaviors during player turn

  private def myTurnBeforeDraw(gameData: GameData): Behavior[Message] = Behaviors.receivePartial {
    handleDrawCardFromDeck(gameData)
      .orElse(handleShowOwnNthCard(gameData, myTurnBeforeDraw))
      .orElse(handleDrawCardFromDiscardStack(gameData))
      .orElse(handleSendGameStatus(gameData, myTurnBeforeDraw))
  }

  private def myTurnAfterDraw(gameData: GameData, cardInHand: Card): Behavior[Message] = Behaviors.receivePartial {
    handleDiscardCard(gameData, cardInHand)
      .orElse(handleShowOwnNthCard(gameData, myTurnAfterDraw(_, cardInHand)))
      .orElse(handleDiscardNthCard(gameData, cardInHand))
      .orElse(handleSendGameStatus(gameData, myTurnAfterDraw(_, cardInHand)))
  }

  private def myTurnAfterDiscard(gameData: GameData): Behavior[Message] = Behaviors.receivePartial {
    handleSendGameStatus(gameData, myTurnAfterDiscard)
      .orElse(handleShowOwnNthCard(gameData, myTurnAfterDiscard))
      .orElse({ case (ctx, GameCoordinatorMessage.EndTurn()) =>
        // TODO: send to other players the new status of the game
        // send to other atcual status
        // [...]
        // to change then
        gameData.whoToSendResponse ! GameCoordinatorMessage.GameInformation(gameData.game)
        myTurnBeforeDraw(gameData)
      })
  }

  // Handlers during player turn

  private def handleDrawCardFromDeck(
                                      gameData: GameData
                                      //                                      nextBehaviors: (ActorRef[Message], GameInProgress) => Behavior[Message]
                                    ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DrawCardFromDeck()) =>
      ctx.log.info(s"I draw a card from deck")

      val (topCard, newDeck) = gameData.game.deckStack.drawFirstCard

      gameData.whoToSendResponse ! GameCoordinatorMessage.CardDrawn(topCard)
      //      val newGameState = gameData.copy(temporaryDeck = newDeck)
      myTurnAfterDraw(
        gameData.copy(temporaryDeck = newDeck),
        topCard
      )

  private def handleDrawCardFromDiscardStack(
                                              gameData: GameData
                                            ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DrawCardFromDiscardStack()) =>
      ctx.log.info(s"I draw a card from discard stack")

      val (topCard, newDiscardStack) = gameData.game.discardDeckStack.drawFirstCard

      gameData.whoToSendResponse ! GameCoordinatorMessage.CardDrawn(topCard)
      //      val newGameState = gameData.game.copy(discardDeckStack = newDiscardStack)
      myTurnAfterDraw(
        gameData.copy(temporaryDiscardDeck = newDiscardStack),
        topCard
      )

  private def handleDiscardCard(
                                 gameData: GameData,
                                 cardInHand: Card
                               ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DiscardCardDrawn()) =>
      ctx.log.info(s"I discard the card drawn")
      val newDeck = gameData.temporaryDeck
      val newDiscardStack = gameData.temporaryDiscardDeck.addTopCard(cardInHand)
      val newGameState = gameData.game.copy(
        deckStack = newDeck,
        discardDeckStack = newDiscardStack
      )
      gameData.whoToSendResponse ! GameCoordinatorMessage.NewTopCardDiscardStack(cardInHand)
      myTurnAfterDiscard(gameData.copy(game = newGameState))

  private def handleDiscardNthCard(
                                    gameData: GameData,
                                    cardInHand: Card
                                  ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DiscardYourNthCard(index)) =>
      ctx.log.info(s"I discard the card with index $index")
      val oldHand = getOurHand(gameData)
      ctx.log.info(s"The card is ${oldHand.cards(index)}")
      val newPlayersState = changeCardNthOfNthPlayer(
        gameData.playerRank,
        gameData.game.players,
        index,
        cardInHand
      )
      val newDeck = gameData.temporaryDeck
      val newDiscardStack = gameData.temporaryDiscardDeck.addTopCard(oldHand.cards(index))
      val newGameState = gameData.game.copy(
        deckStack = newDeck,
        discardDeckStack = newDiscardStack,
        players = newPlayersState
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

  private def handleShowOwnNthCard(
                                    gameData: GameData,
                                    nextBehaviors: GameData => Behavior[Message]
                                  ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.ShowYourNthCard(index)) =>
      ctx.log.info(s"I show the card with index $index")
      val hand = getOurHand(gameData)
      ctx.log.info(s"The card is ${hand.cards(index)}")
      gameData.whoToSendResponse ! GameCoordinatorMessage.CardSeen(hand.cards(index))
      nextBehaviors(gameData)

  private def getOurHand(gameData: GameData) = gameData.game.players(gameData.playerRank).hand

  private def changeCardNthOfNthPlayer(
                                        playerRank: Int,
                                        players: List[PlayerPlaying],
                                        cardIndexToChange: Int,
                                        newCard: Card
                                      ): List[PlayerPlaying] =
    val newPlayer = changeCardOfPlayer(players(playerRank), cardIndexToChange, newCard)
    players.updated(playerRank, newPlayer)


  private def changeCardOfPlayer(
                                  player: PlayerPlaying,
                                  cardIndexToChange: Int,
                                  newCard: Card
                                ): PlayerPlaying =
    player.copy(
      hand = player.hand.changeNthCard(cardIndexToChange, newCard)
    )
