package utils

import model.Game.GameInConstruction

object ClientMessages {

  trait ClientCommand extends Message

  /**
   * Message sent by the view to the client to create a new game
   *
   * @param makePublic
   * @param maxTimeRound
   * @param maxNumRound
   * @param maxPlayers
   */
  case class CreateNewGame(makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int) extends ClientCommand

  /**
   * Message sent by the view if the player wants to join a game
   */
  case class JoinAGame() extends ClientCommand

  /**
   * Message sent by the view to the client to join the specific game
   *
   * @param game
   */
  case class JoinGame(game: GameInConstruction) extends ClientCommand

  /**
   * Message sent by the view to the client to join a game by its address
   *
   * @param address
   */
  case class JoinAddress(address: String) extends ClientCommand

  /**
   * Message sent by the view to the client to start the game
   *
   * @param game
   */
  case class StartTheGame() extends ClientCommand

  /**
   * Message sent by the view to the client to leave the game
   */
  case class LeaveTheGame() extends ClientCommand

}
