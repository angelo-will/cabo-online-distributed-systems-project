package view

import akka.actor.typed.ActorRef
import utils.Message
import utils.ViewMessages

trait IViewListener:
  def createGame(makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit

  def joinAGame(): Unit

object ViewListener:

  case class CreateGameButtonListener(ref: ActorRef[Message]) extends IViewListener:
    override def createGame(makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit =
      ref ! ViewMessages.CreateNewGame(makePublic, maxTimeRound, maxNumRound, maxPlayers)

    override def joinAGame(): Unit =
      ref ! ViewMessages.JoinAGame()
