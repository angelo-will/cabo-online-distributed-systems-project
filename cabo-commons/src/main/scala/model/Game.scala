package model

import model.Suit.Spades

object Game:
  // 0-index
  val maxPlayersPerGame = 4

  case class GameInConstruction(
                                 code: String,
                                 gameParameters: IGameParameters,
                                 players: List[PlayerPlaying],
                               )

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

abstract class GameStatus(val status: String)

object GameStatus:
  case class WaitingForPlayers() extends GameStatus("WaitingForPlayers")

  case class InProgress() extends GameStatus("InProgress")

  case class Finished() extends GameStatus("Finished")

