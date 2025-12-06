package view.lobbyphase

import model.Game.GameInConstruction
import model.PlayerInLobby

object ViewListener:

  trait IInitialViewListener:

    def changeName(newName: String): Unit

    def createGame(isPublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit

    def requestGames(): Unit

    def joinGame(game: GameInConstruction): Unit

    def joinWithAddress(address: String): Unit

    def returnToStart(): Unit

    def startGame(): Unit

    def exitFromTheGame(): Unit

  trait IDuringGameViewListener:

    def ownCardSelected(cardIndex: Int): Unit

    def adversaryCardSelected(adversaryID: String, cardIndex: Int): Unit

    def drawFromDeck(): Unit

    def drawFromDiscard(): Unit

    def discardCardDrawn(): Unit

    def endTurn(): Unit

    def callCabo(): Unit

    def exit(): Unit

    def consultingResultsEnded(): Unit
