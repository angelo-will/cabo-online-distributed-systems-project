package model

import model.Suit.Spades

trait WithDeck:
  def deck: List[Card]

trait WithStatus:
  def gameStatus: GameStatus

abstract class GameStatus(val status: String)

object GameStatus:
  case class WaitingForPlayers() extends GameStatus("WaitingForPlayers")

  case class InProgress() extends GameStatus("InProgress")

  case class Finished() extends GameStatus("Finished")

case class GameInConstruction(
                               code: String,
                               gameParameters: IGameParameters,
                               players: List[PlayerPlaying],
                             ) extends WithStatus:
  override def gameStatus: GameStatus = GameStatus.WaitingForPlayers()

  def playersAddress: List[String] = players.map(_.address)

case class GameInProgress(
                           code: String,
                           gameParameters: IGameParameters,
                           gameStatus: GameStatus,
                           players: List[PlayerPlaying],
                           deckStack: CardStack,
                           discardDeckStack: CardStack,
                           currentRound: Int
                         )