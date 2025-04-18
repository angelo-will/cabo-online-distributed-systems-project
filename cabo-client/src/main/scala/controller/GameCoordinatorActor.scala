package controller

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import model.Suit.Spades
import model.{Card, GameParameters}
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
    val (topCardDiscardStack, deckOftheGame) = CardStack.buildShuffledFullDeck.drawFirstCard
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
            List.empty,
            deckOftheGame,
            discardStack,
            0
          ),
          deckOftheGame,
          discardStack
        ))
      }

  // Behaviors during player turn

  private def myTurnBeforeDraw(gameData: GameData): Behavior[Message] = Behaviors.receivePartial {
    handleDrawCardFromDeck(gameData)
      .orElse(handleDrawCardFromDiscardStack(gameData))
      .orElse(handleSendGameStatus(gameData, myTurnBeforeDraw))
  }

  private def myTurnAfterDraw(gameData: GameData, cardInHand: Card): Behavior[Message] = Behaviors.receivePartial {
    handleDiscardCard(gameData, cardInHand)
      .orElse(handleSendGameStatus(gameData, myTurnAfterDraw(_, cardInHand)))
  }

  private def myTurnAfterDiscard(gameData: GameData): Behavior[Message] = Behaviors.receivePartial {
    handleSendGameStatus(gameData, myTurnAfterDiscard)
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

  private def handleSendGameStatus(
                                    gameData: GameData,
                                    nextBehaviors: GameData => Behavior[Message]
                                  ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.SendGameStatus(ref)) =>
      ref ! GameCoordinatorMessage.GameInformation(gameData.game)
      nextBehaviors(gameData)
