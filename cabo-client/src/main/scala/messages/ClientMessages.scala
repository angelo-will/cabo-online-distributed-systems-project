package messages

import akka.actor.typed.ActorRef
import model.Game.{GameInConstruction, GameInProgress}
import model.{GameParameters, PlayerInLobby, TurnLog}

object ClientMessages {

  trait ClientCommand extends Message

  /**
   * Message sent by the view to the client to create a new game
   *
   * @param makePublic
   * @param maxTimeRound
   * @param maxNumRound
   * @param maxPlayers
   * @param gameCode
   */
  //todo: reflect about gameCode
  case class CreateNewGame(makePublic: Boolean = GameParameters.defaultIsPublic,
                           maxTimeRound: Int = GameParameters.defaultMaxTimeRound,
                           maxNumRound: Int = GameParameters.defaultRoundLimitation,
                           maxPlayers: Int = GameParameters.defaultMaxPlayers,
                           gameCode: Option[String] = None
                          ) extends ClientCommand

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
   * Message sent by the view to the client to join a game with gameCode
   *
   * @param gameCode
   */
  case class JoinWithGameCode(gameCode: String) extends ClientCommand

  /**
   * Message sent by the view to the client to return to the start configuration
   */
  case class ReturnToStart() extends ClientCommand

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

  /**
   * Message sent by the view to the client to change the player name
   *
   * @param newName
   * @param replyTo
   */
  case class ChangePlayerName(newName: String, replyTo: ActorRef[ClientCommand]) extends ClientCommand

  /**
   * Message sent by the view to the client to get the player information
   * This is useful for displaying player details in the UI
   */
  case class GetPlayerInfo(replyTo: ActorRef[ClientCommand]) extends ClientCommand

  /**
   * Reply to provide player information
   */
  case class PlayerInfo(userID: String, name: String) extends ClientCommand

  /**
   * Message sent by the coordinator to the client to pass the game in progress to share
   *
   * @param game
   */
  case class TakeGameInProgress(game: GameInProgress) extends ClientCommand

  /**
   * Message sent by the coordinator when a turn has ended to share its log with the other players
   *
   * @param replyTo
   */
  case class TurnEnded(game: GameInProgress, turnLog: TurnLog) extends ClientCommand

  /**
   * Message sent by the coordinator when a turn has been updated to notify the client
   */
  case class TurnUpdated() extends ClientCommand

  /**
   * Message sent by the view to the client to notify that the during game view is ready
   *
   * @param viewRef
   */
  case class DuringGameViewReady(viewRef: ActorRef[IGameViewMessage]) extends ClientCommand

  /**
   * Message sent by the coordinator to the client to notify that the initial phase has been completed
   *
   * @param turnLog
   */
  case class InitialPhaseCompleted(turnLog: TurnLog) extends ClientCommand

  /**
   * Message sent by the view to the client to notify that the game has ended
   *
   * @param viewRef
   */
  case class GameEnded() extends ClientCommand

  /**
   * Message sent by the coordinator to the client to respond to the request to know who is playing in the active turn
   *
   * @param playerID
   */
  case class WhoIsPlaying(playerID: String) extends ClientCommand

}
