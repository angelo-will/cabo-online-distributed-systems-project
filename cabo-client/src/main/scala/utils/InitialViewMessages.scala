package utils

import akka.actor.typed.ActorRef
import model.Game.GameInConstruction
import model.PlayerInLobby
import utils.GameCoordinatorMessage.GameCoordinatorMessage

object InitialViewMessages {

  trait ViewCommand extends Message

  case class WhoToSendResponse(ref: ActorRef[Message]) extends ViewCommand
  /**
   * Message sent by the client to the view if it's not possible to share the game with the server
   *
   * @param game
   */
  case class FailedToPublishToServer() extends ViewCommand

  /**
   * Answer to the previous message
   * @param game
   */
  case class GameCreated(game: GameInConstruction) extends ViewCommand

  /**
   * Message sent to the view containing the list of games
   *
   * @param games
   */
  case class GameList(games: List[GameInConstruction]) extends ViewCommand

  /**
   * Answer to the previous message if managed to join the game
   * @param game
   */
  case class GameJoined(game: GameInConstruction) extends ViewCommand

  /**
   * Answer to the previous message if failed to join the game
   * @param game
   */
  case class GameJoinedFailed(game: GameInConstruction) extends ViewCommand

  /**
   * Message sent by the client to the view when the information about the game has changed, 
   * for example, a new player has joined
   *
   * @param game
   */
  case class GameInfoUpdate(game: GameInConstruction) extends ViewCommand

  /**
   * Reply to the previous message sent by the client when the game has been started
   */
  case class GameStarted() extends ViewCommand

  /**
   * Message sent by the client to the view when the game has been aborted
   */
  case class GameAborted() extends ViewCommand

  /**
   * Message sent to the view by the client when a player crashed or left the game
   * @param player
   */
  case class PlayerLeftGame(player: PlayerInLobby) extends ViewCommand

  /**
   * Message sent to the view by the client when all the players a ready to play
   * @param gameCoordinator
   */
  case class ReadyToPlay(gameCoordinator: ActorRef[GameCoordinatorMessage]) extends ViewCommand

}
