package view.lobbyphase

import akka.actor.typed.ActorRef
import model.Game.GameInConstruction
import utils.{Message, ViewMessages}

trait IViewListener:
  def createGame(makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit

  def requestGames(): Unit
  
  def joinGame(game: GameInConstruction): Unit

  def joinWithAddress(address: String): Unit

