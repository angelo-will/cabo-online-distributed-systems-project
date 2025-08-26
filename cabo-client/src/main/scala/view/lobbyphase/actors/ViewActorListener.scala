package view.lobbyphase.actors

import akka.actor.typed.ActorRef
import model.Game
import utils.{Message, ClientMessages}
import view.lobbyphase.IViewListener

case class ViewActorListener(ref: ActorRef[Message]) extends IViewListener:
  override def createGame(isPublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit =
    ref ! ClientMessages.CreateNewGame(isPublic, maxTimeRound, maxNumRound, maxPlayers)

  override def requestGames(): Unit =
    ref ! ClientMessages.JoinAGame()

  override def joinGame(game: Game.GameInConstruction): Unit =
    ref ! ClientMessages.JoinGame(game)

  override def joinWithAddress(address: String): Unit =
    ref ! ClientMessages.JoinAddress(address)

  override def startGame(): Unit =
    ref ! ClientMessages.StartTheGame()
    
//  override def playerCanJoinGame(player: model.PlayerInLobby): Unit =
//    ref ! ClientMessages.PlayerCanJoinGame(player)