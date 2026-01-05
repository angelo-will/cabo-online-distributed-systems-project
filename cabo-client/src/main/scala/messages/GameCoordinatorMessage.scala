package messages

import akka.actor.typed.ActorRef
import model.{Card, Game, TurnLog}

object GameCoordinatorMessage:
  /**
   * Represents a command to draw a card from the deck.
   */
  case class DrawCardFromDeck() extends IGameCoordinatorPlayerCommand

  /**
   * Represents a command to draw a card from the discard stack.
   */
  case class DrawCardFromDiscardStack() extends IGameCoordinatorPlayerCommand

  /**
   * Represents a command to discard the drawn card.
   */
  case class DiscardCardDrawn() extends IGameCoordinatorPlayerCommand

  /**
   * Represents a command to discard the index-nth card from the player's hand and keep the drawn card.
   * @param index
   */
  case class DiscardOwnNthCard(index: Int) extends IGameCoordinatorPlayerCommand

  /**
   * Represents a command to show the index-nth card from the player's own hand.
   * @param index
   */
  case class ShowOwnNthCard(index: Int) extends IGameCoordinatorPlayerCommand

  /**
   * Represents a command to show the index-nth card from an adversary's hand.
   * @param adversaryID
   * @param cardIndex
   */
  case class ShowAdversaryNthCard(adversaryID: String, cardIndex: Int) extends IGameCoordinatorPlayerCommand

  /**
   * Represents a command to replace the player's own index-nth card with an adversary's index-nth card.
   * @param ownCardIndex
   * @param adversaryID
   * @param adversaryCardIndex
   */
  case class ReplaceOwnNthCardWithAdversaryNthOne(ownCardIndex: Int, adversaryID: String, adversaryCardIndex: Int) extends IGameCoordinatorPlayerCommand

  /**
   * Represents a command to end the player's turn.
   */
  case class EndTurn() extends IGameCoordinatorPlayerCommand

  /**
   * Represents a command to call "Cabo", trigger turn's end.
   */
  case class CallCabo() extends IGameCoordinatorPlayerCommand

  // SYNC MESSAGES

  /**
   * Notify to start the revealing section of the game.
   */
  case class StartRevealingSection() extends IGameCoordinatorSyncMessage

  /**
   * Notify to start the play cycle of the game.
   */
  case class StartPlayCycle() extends IGameCoordinatorSyncMessage
  
  /**
   * Notify that the last turn has been played, providing the updated game state and the turn log.
   * @param game
   * @param turnLog
   */
  case class LastTurnPlayed(game: Game.GameInProgress, turnLog: TurnLog) extends IGameCoordinatorSyncMessage

  /**
   * Request an empty turn (no actions) for player with userID
   * @param userID
   */
  case class GetEmptyTurn(userID: String) extends IGameCoordinatorSyncMessage
  
  /**
   * Notify that the turn time has ended for the current player.
   */
  case class TurnTimeEnded() extends IGameCoordinatorSyncMessage

  /**
   * Request to know who is playing the turn currently
   */
  case class WhoIsPlayingRequest() extends IGameCoordinatorSyncMessage

  // FOR TESTING

  case class SendGameStatus(toWhoSend: ActorRef[Message]) extends IGameCoordinatorSyncMessage

  case class GameInformation(game: Game.GameInProgress) extends IGameCoordinatorSyncMessage
