package view.pregamephase.actors

import akka.actor.typed.ActorRef
import messages.ClientMessages
import model.Game
import messages.ClientMessages.ClientCommand
import view.pregamephase.IPreGameViewListener

case class PreGameViewListener(ref: ActorRef[ClientCommand]) extends IPreGameViewListener:
  override def changeName(newName: String): Unit =
    ref ! ClientMessages.ChangePlayerName(newName, ref)

  override def createGame(isPublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit =
    ref ! ClientMessages.CreateNewGame(isPublic, maxTimeRound, maxNumRound, maxPlayers)

  override def requestGames(): Unit =
    ref ! ClientMessages.JoinAGame()

  override def joinGame(game: Game.GameInConstruction): Unit =
    ref ! ClientMessages.JoinGame(game)

  override def joinWithAddress(address: String): Unit =
    ref ! ClientMessages.JoinAGame()
    ref ! ClientMessages.JoinAddress(address)

  override def returnToStart(): Unit =
    ref ! ClientMessages.ReturnToStart()

  override def startGame(): Unit =
    ref ! ClientMessages.StartTheGame()

  override def exitFromTheGame(): Unit =
    ref ! ClientMessages.LeaveTheGame()