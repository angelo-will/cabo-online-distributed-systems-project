package controller

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import model.Suit.Spades
import model.{Card, GameInProgress, GameParameters}

object GameCoordinatorActor:

  import akka.actor.typed.ActorRef

  import utils.Message
  import utils.GameCoordinatorMessage
  import model.CardStack
  import model.GameStatus

  //  val debugValue: GameInProgress = GameInProgress(
  //    "gameCode",
  //    GameParameters(maxTimeRound = 5),
  //    GameStatus.InProgress(),
  //    List.empty,
  //    CardStack.buildSortedFullDeck,
  //    CardStack(List(Card("A", Spades()))),
  //    0
  //  )

  def apply(whoToSendResponse: ActorRef[Message]): Behavior[Message] = Behaviors.setup { ctx =>
    ctx.log.info("GameLogic Actor started")
    myTurnBeforeDraw(whoToSendResponse, GameInProgress(
      "gameCode",
      GameParameters(maxTimeRound = 5),
      GameStatus.InProgress(),
      List.empty,
      CardStack.buildSortedFullDeck,
      CardStack(List(Card("A", Spades()))),
      0
    ))
  }
  
  // Behaviors during player turn

  private def myTurnBeforeDraw(whoToSendResponse: ActorRef[Message], game: GameInProgress): Behavior[Message] = Behaviors.receivePartial {
    handleDrawCardFromDeck(whoToSendResponse, game)
      .orElse(handleDrawCardFromDiscardStack(whoToSendResponse, game))
  }

  private def myTurnAfterDraw(whoToSendResponse: ActorRef[Message], game: GameInProgress, cardInHand: Card): Behavior[Message] = Behaviors.receivePartial {
    handleDiscardCard(whoToSendResponse, game, cardInHand)
  }
  
  private def myTurnAfterDiscard(value: ActorRef[Message], game: GameInProgress): Behavior[Message] = Behaviors.receivePartial {
    case (ctx, GameCoordinatorMessage.EndTurn()) =>
      // send to other atcual status
      // [...]
      // to change then
      myTurnBeforeDraw(value, game)
  }
  
  // Handlers during player turn

  private def handleDrawCardFromDeck(
                                      whoToSendResponse: ActorRef[Message],
                                      game: GameInProgress,
                                      //                                      nextBehaviors: (ActorRef[Message], GameInProgress) => Behavior[Message]
                                    ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DrawCardFromDeck()) =>
      ctx.log.info(s"I draw a card from deck")

      val (topCard, newDeck) = game.deckStack.drawFirstCard

      whoToSendResponse ! GameCoordinatorMessage.CardDrawn(topCard)
      myTurnAfterDraw(whoToSendResponse, game.copy(deckStack = newDeck), topCard)

  private def handleDrawCardFromDiscardStack(
                                              whoToSendResponse: ActorRef[Message],
                                              game: GameInProgress,
                                              //nextBehaviors: (ActorRef[Message], GameInProgress) => Behavior[Message]
                                            ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DrawCardFromDiscardStack()) =>
      ctx.log.info(s"I draw a card from discard stack")

      val (topCard, newDiscardStack) = game.discardDeckStack.drawFirstCard

      whoToSendResponse ! GameCoordinatorMessage.CardDrawn(topCard)
      myTurnAfterDraw(whoToSendResponse, game.copy(discardDeckStack = newDiscardStack), topCard)

  private def handleDiscardCard(
                                 whoToSendResponse: ActorRef[Message],
                                 game: GameInProgress,
                                 cardInHand: Card
                                 //nextBehaviors: (ActorRef[Message], GameInProgress) => Behavior[Message]
                               ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GameCoordinatorMessage.DiscardCardDrawn()) =>
      ctx.log.info(s"I discard the card drawn")
      val g = game.copy(
        deckStack = game.deckStack,
        discardDeckStack = game.discardDeckStack.addCard(cardInHand)
      )
      whoToSendResponse ! GameCoordinatorMessage.NewTopCardDiscardStack(cardInHand)
      myTurnAfterDiscard(whoToSendResponse, game)
