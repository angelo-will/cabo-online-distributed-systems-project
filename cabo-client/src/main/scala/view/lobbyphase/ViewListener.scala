package view.lobbyphase

import model.Game.GameInConstruction
import model.PlayerInLobby

object ViewListener:
    
  trait IViewListener:
    def createGame(isPubblic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit
  
    def requestGames(): Unit
    
    def joinGame(game: GameInConstruction): Unit
  
    def joinWithAddress(address: String): Unit
    
    def startGame(): Unit
    
    
    
  //  def playerCanJoinGame(player: PlayerInLobby): Unit

