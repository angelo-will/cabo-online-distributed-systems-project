package view.lobbyphase

import model.Game.GameInConstruction
import model.PlayerInLobby

object ViewListener:

  trait IPreGameViewListener:

    def changeName(newName: String): Unit

    def createGame(isPublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit

    def requestGames(): Unit

    def joinGame(game: GameInConstruction): Unit

    def joinWithAddress(address: String): Unit

    def returnToStart(): Unit

    def startGame(): Unit

    def exitFromTheGame(): Unit

