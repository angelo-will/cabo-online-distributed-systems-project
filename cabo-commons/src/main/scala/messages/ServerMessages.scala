package messages

import akka.actor.typed.receptionist.ServiceKey

object ServerMessages:
  import akka.actor.typed.ActorRef
  import model.Game.GameInConstruction
  
  private val ServerCode = "Server"
  
  val ServerKey: ServiceKey[Message] = akka.actor.typed.receptionist.ServiceKey[Message](ServerCode)
  
  trait ServerCommand extends Message

  /**
   * Message used to register a game to the server
   *
   * @param game - game to register
   * @param replyTo - ActorRef of the client that send the message
   */
  case class RegisterGame(game: GameInConstruction, replyTo: ActorRef[Message]) extends ServerCommand

  /**
   * Reply message for [[RegisterGame]] and [[UpdateGame]] to acknowledge the client that his game has been registered
   *
   * @param game - the game that has been registered
   * @param sender - the ActorRef of the server that served the request
   */
  case class GameRegistered(game: GameInConstruction, sender: ActorRef[Message]) extends ServerCommand

  /**
   * Message to indicate a failure when adding a game to the list of games
   *
   * @param game - game that has not been inserted
   * @param sender - ActorRef of the server that served the request
   */
  case class FailedToRegisterGame(game: GameInConstruction, sender: ActorRef[Message]) extends ServerCommand

  /**
   * Message to indicate that the game has started so it has to be removed from the list
   *
   * @param game - game that has started
   * @param replyTo - ActorRef of the client that started the game
   */
  case class StartGame(game: GameInConstruction, replyTo: ActorRef[Message]) extends ServerCommand

  /**
   * Message to indicate that the game has been aborted so it has to be removed from the list
   *
   * @param game - game that has started
   * @param replyTo - ActorRef of the client that started the game
   */
  case class AbortGame(game: GameInConstruction, replyTo: ActorRef[Message]) extends ServerCommand

  /**
   * Message used by a client to request the list of joinable games
   *
   * @param replyTo - ActorRef of the client that requested the list
   */
  case class GetGames(replyTo: ActorRef[Message]) extends ServerCommand

  /**
   * Reply message for [[GetGames]]
   *
   * @param games - the list of games
   */
  case class GamesList(games: Set[GameInConstruction]) extends ServerCommand

  /**
   * Update the information about an already registered game
   *
   * @param game - game that has to be updated, contains the new information
   * @param replyTo - ActorRef of the client that requested the update
   */
  case class UpdateGame(game: GameInConstruction, replyTo: ActorRef[Message]) extends ServerCommand

  /**
   * Message to indicate a failure when updating the information about an already registered game
   * 
   * @param game - game that has to be updated, contains the new information
   * @param sender - ActorRef of the server that served the request
   */
  case class FailedToUpdate(game: GameInConstruction, sender: ActorRef[Message]) extends ServerCommand

  /**
   * Message to clear the list of games on the server (for testing purposes)
   * 
   * @param replyTo - ActorRef of the client that requested the clearing of the games
   */
  case class ClearGames(replyTo: ActorRef[Message]) extends ServerCommand
  
  /**
   * Reply message for [[ClearGames]]
   *
   * @param sender - ActorRef of the server that served the request
   */
  case class GamesCleared(sender: ActorRef[Message]) extends ServerCommand