package view.pregamephase

import model.Game.GameInConstruction

trait IPreGameViewListener {

  def changeName(newName: String): Unit

  def createGame(isPublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit

  def requestGames(): Unit

  def joinGame(game: GameInConstruction): Unit

  def joinWithGameCode(gameCode: String): Unit

  def returnToStart(): Unit

  def startGame(): Unit

  def exitFromTheGame(): Unit

}