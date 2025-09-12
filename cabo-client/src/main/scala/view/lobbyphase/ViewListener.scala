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

    def startGame(): Unit
    
    def exitFromTheGame(): Unit

  trait IDuringGameViewListener:
    
    def ownCardSelected(cardIndex: Int): Unit
    
    def adversaryCardSelected(adversaryID: String, cardIndex: Int): Unit
    
    def showCardNth(cardIndex: Int): Unit

    def drawFromDeck(): Unit

    def drawFromDiscard(): Unit

    def discardCardNth(carIndex: Int): Unit

    def discardCardDrawn(): Unit

    def showAdversaryNthCard(adversaryID: String, cardIndex: Int): Unit

    def swapCardWithAdversaryNthCard(ownCardIndex: Int, adversaryID: String, adversaryCardIndex: Int): Unit

    def endTurn(): Unit
    
    def callCabo(): Unit
