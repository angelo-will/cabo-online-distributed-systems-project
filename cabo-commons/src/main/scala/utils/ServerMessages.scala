package utils

object ServerMessages:
  import akka.actor.typed.ActorRef
  import model.Game.GameInConstruction
  
  trait ServerCommand extends Message
  
  type Games = Seq[GameInConstruction]
  
  case class RegisterGame(game: GameInConstruction, replyTo: ActorRef[Message]) extends ServerCommand
  
  case class GameRegistered(game: GameInConstruction, replyTo: ActorRef[Message]) extends ServerCommand
  
  case class StartGame(game: GameInConstruction, replyTo: ActorRef[Message]) extends ServerCommand
  
  case class AbortGame(game: GameInConstruction, replyTo: ActorRef[Message]) extends ServerCommand
  
  case class GetGames(replyTo: ActorRef[Message]) extends ServerCommand
  
  case class GamesList(games: Games) extends ServerCommand
