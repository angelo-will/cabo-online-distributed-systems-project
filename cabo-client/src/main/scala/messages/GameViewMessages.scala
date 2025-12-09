package messages

import akka.actor.typed.ActorRef
import model.{Card, Game, PlayerPlaying, TurnLog}
import model.Game.GameInProgress

object GameViewMessages {

  /**
   * Notify to actor that game is ready to start and provide all necessary info. Revealing section expected.
   *
   * @param game               - the game in progress
   * @param gameCoordinatorRef - reference to the game coordinator actor
   */
  case class StartGame(game: GameInProgress, gameCoordinatorRef: ActorRef[GameCoordinatorMessage.GameCoordinatorMessage]) extends IGameViewMessage

  /**
   * Notify to actor a revealing section log of an adversary.
   *
   * @param revealingLog
   */
  case class RevealingCardsPhaseAdversaryLog(revealingLog: TurnLog) extends IGameViewMessage

  /**
   * Notify to actor that revealing section is ended and it's time to wait for others.
   */
  case class WaitAfterRevealingSection() extends IGameViewMessage

  /**
   * Notify to actor the last turn played, could be itself or an adversary.
   *
   * @param turnLog
   * @param game
   * @param isMyTurn - indicates if now is my turn
   */
  case class LastTurnPlayed(turnLog: TurnLog, game: GameInProgress) extends IGameViewMessage

  /**
   * Notify to actor which player's turn is started, could be itself or an adversary.
   *
   * @param playerID
   */
  case class StartTurnPlayer(playerID: String) extends IGameViewMessage

  /**
   * Notify to actor that the change card with adversary action is done.
   */
  case class ChangeCardWithAdversaryAck() extends IGameViewMessage

  /**
   * Notify to actor the card drawn.
   *
   * @param card
   */
  case class CardDrawn(card: Card) extends IGameViewMessage

  /**
   * Notify to actor the new top card of the discard stack.
   *
   * @param card
   */
  case class NewTopCardDiscardStack(card: Card) extends IGameViewMessage

  /**
   * Notify to actor that the discard stack is empty.
   */
  case class EmptyDiscardStack() extends IGameViewMessage

  /**
   * Notify to actor the previously card requested.
   *
   * @param card
   */
  case class CardSeen(card: Card) extends IGameViewMessage

  /**
   * Notify to actor that the game has ended because someone has called cabo.
   *
   * @param game
   */
  case class GameEndedByCabo(game: GameInProgress) extends IGameViewMessage

  /**
   * Notify to actor that the game has ended because the turns limit has been reached.
   *
   * @param game
   */
  case class GameEndedByTurnsLimit(game: GameInProgress) extends IGameViewMessage

  /**
   * Notify to actor that the game has ended because the deck is empty.
   *
   * @param game
   */
  case class GameEndedByEmptyDeck(game: GameInProgress) extends IGameViewMessage

  case class EndTurnByTimeEnded() extends IGameViewMessage

  case class OpponentImpossibleToReach(player: PlayerPlaying) extends IGameViewMessage

  case class OpponentDisconnected(player: PlayerPlaying) extends IGameViewMessage
  
  case class GameDeleted() extends IGameViewMessage

}
