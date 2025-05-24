package view.actors

import akka.actor.typed.ActorRef
import model.Game
import utils.{Message, ViewMessages}
import view.IViewListener

case class ViewActorListener(ref: ActorRef[Message]) extends IViewListener:
  override def createGame(makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit =
    ref ! ViewMessages.CreateNewGame(makePublic, maxTimeRound, maxNumRound, maxPlayers)

  override def requestGames(): Unit =
    ref ! ViewMessages.JoinAGame()

  override def joinGame(game: Game.GameInConstruction): Unit =
    ref ! ViewMessages.JoinGame(game)

  override def joinWithAddress(address: String): Unit =
    ref ! ViewMessages.JoinAGameWithAddress(address)
