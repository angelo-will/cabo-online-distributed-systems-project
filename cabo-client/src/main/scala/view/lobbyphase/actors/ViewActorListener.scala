package view.lobbyphase.actors

import akka.actor.typed.ActorRef
import model.Game
import utils.{Message, ViewMessages}
import view.lobbyphase.IViewListener

case class ViewActorListener(ref: ActorRef[Message]) extends IViewListener:
  override def createGame(isPubblic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit =
    ref ! ViewMessages.CreateNewGame(isPubblic, maxTimeRound, maxNumRound, maxPlayers)

  override def requestGames(): Unit =
    ref ! ViewMessages.JoinAGame()

  override def joinGame(game: Game.GameInConstruction): Unit =
    ref ! ViewMessages.JoinGame(game)

  override def joinWithAddress(address: String): Unit =
    ref ! ViewMessages.JoinAGameWithAddress(address)

  override def startGame(): Unit =
    ref ! ViewMessages.StartTheGame()
    
  override def playerCanJoinGame(player: model.PlayerInLobby): Unit =
    ref ! ViewMessages.PlayerCanJoinGame(player)