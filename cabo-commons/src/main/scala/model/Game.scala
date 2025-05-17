package model

import model.Suit.Spades

object Game:
  // 0-index
  val maxPlayersPerGame = 4

  // 0-index
  val maxCardsPerGame = 4

  val cardsInitialVisible = 2

  case class GameInConstruction(
                                 code: String,
                                 gameParameters: IGameParameters,
                                 players: List[PlayerInLobby],
                               ):
    def gameStatus: GameStatus = GameStatus.WaitingForPlayers()

    def playersAddress: List[String] = players.map(_.address)

  case class GameInProgress(
                             code: String,
                             gameParameters: IGameParameters,
                             gameStatus: GameStatus,
                             players: List[PlayerPlaying],
                             deckStack: CardStack,
                             discardDeckStack: CardStack,
                             currentRound: Int
                           ):
    override def toString: String = "GameInProgress\n" +
      "\tcode=" + code + "\n" +
      "\tgameParameters=" + gameParameters + "\n" +
      "\tgameStatus=" + gameStatus + "\n" +
      "\tplayers=" + players + "\n" +
      "\tdeckStack=" + deckStack + "\n" +
      "\tdiscardDeckStack=" + discardDeckStack + "\n" +
      "\tcurrentRound=" + currentRound + ""

    def getPlayerWithID(userID: String) = PlayerPlaying.getPlayerWithID(userID, this.players)

    def getHandOfPlayerWithID(userID: String) = this.getPlayerWithID(userID).hand

    def replaceHandOfPlayerWithID(userID: String, hand: Hand) =
      this.copy(players = PlayerPlaying.replaceHandOfPlayerWithID(userID, hand, this.players))

    def replaceNthCardOfPlayerWithID(userID: String, card: Card, index: Int) =
      this.copy(players = PlayerPlaying.replaceNthCardOfPlayerWithID(userID, card, index, this.players))

abstract class GameStatus(val status: String)

object GameStatus:
  case class WaitingForPlayers() extends GameStatus("WaitingForPlayers")

  case class InProgress() extends GameStatus("InProgress")

  case class Finished() extends GameStatus("Finished")
