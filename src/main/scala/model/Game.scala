package model

trait WithDeck:
  def deck: List[Card]

abstract class GameStatus(status: String)

object GameStatus:
  case class WaitingForPlayers() extends GameStatus("WaitingForPlayers")

  case class InProgress() extends GameStatus("InProgress")

  case class Finished() extends GameStatus("Finished")

case class GameInConstruction(
                 gameParameters: GameParameters,
                 players: List[PlayerPlaying],
                 gameStatus: GameStatus = GameStatus.WaitingForPlayers(),
                 code: String
               )

case class GameInProgress(
                 gameParameters: GameParameters,
                 players: List[PlayerPlaying],
                 gameStatus: GameStatus,
                 code: String,
                 currentRound: Int
               ) extends WithDeck:
  override def deck: List[Card] = deck