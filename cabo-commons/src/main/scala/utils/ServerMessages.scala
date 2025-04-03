package utils

import akka.actor.typed.ActorRef
import model.GameInConstruction

trait ServerCommand extends Message

case class RegisterGame(game: GameInConstruction, replyTo: ActorRef[Message]) extends ServerCommand
case class GameRegistered(game: GameInConstruction, replyTo: ActorRef[Message]) extends ServerCommand

case class StartGame(game: GameInConstruction, replyTo: ActorRef[Message]) extends ServerCommand

case class AbortGame(game: GameInConstruction, replyTo: ActorRef[Message]) extends ServerCommand

case class GetGames(replyTo: ActorRef[Message]) extends ServerCommand

case class GamesList(games: Seq[GameInConstruction]) extends ServerCommand
