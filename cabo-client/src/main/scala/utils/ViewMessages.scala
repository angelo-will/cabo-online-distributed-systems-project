package utils

import model.Game.GameInConstruction
import model.PlayerInLobby

object ViewMessages {

  trait ViewCommand extends Message

  /**
   * Message sent by the view to the client to create a new game
   *
   * @param makePublic
   * @param maxTimeRound
   * @param maxNumRound
   * @param maxPlayers
   */
  case class CreateNewGame(makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int) extends ViewCommand

  /**
   * Answer to the previous message
   * @param game
   */
  case class GameCreated(game: GameInConstruction) extends ViewCommand

  /**
   * Message sent by the view if the player wants to join a game
   */
  case class JoinAGame() extends ViewCommand

  /**
   * Message sent to the view containing the list of games
   *
   * @param games
   */
  case class GameList(games: List[GameInConstruction]) extends ViewCommand

  /**
   * Message sent by the view to the client to join the specific game
   *
   * @param game
   */
  case class JoinGame(game: GameInConstruction) extends ViewCommand

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
   * Message sent by the view to the client to start the game
   *
   * @param game
   */
  case class StartTheGame() extends ViewCommand

  /**
   * Reply to the previous message sent by the client when the game has been started
   */
  case class GameStarted() extends ViewCommand

  /**
   * Message sent to the view by the client when a player crashed or left the game
   * @param player
   */
  case class PlayerLeftGame(player: PlayerInLobby) extends ViewCommand

}
