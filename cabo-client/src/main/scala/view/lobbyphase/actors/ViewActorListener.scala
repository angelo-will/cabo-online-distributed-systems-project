package view.lobbyphase.actors

import akka.actor.typed.ActorRef
import model.Game
import utils.{Message, ClientMessages}
import view.lobbyphase.ViewListener.IInitialViewListener

case class ViewActorListener(ref: ActorRef[Message]) extends IInitialViewListener:
  override def changeName(newName: String): Unit =
    // TODO: in questo caso il ref dentro il messaggio dovrebbe essere quello di chi invia,
    //       mentre ora invia lo stesso indirizzo del destinatario del messaggio.
    ref ! ClientMessages.ChangePlayerName(newName, ref)

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

  override def exitFromTheGame(): Unit =
    ref ! ClientMessages.LeaveTheGame()