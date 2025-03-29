package model

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
                               gameParameters: IGameParameters,
                               players: List[PlayerPlaying],
                               code: String
                             ) extends WithStatus:
  override def gameStatus: GameStatus = GameStatus.WaitingForPlayers()

case class GameInProgress(
                           gameParameters: IGameParameters,
                           players: List[PlayerPlaying],
                           code: String,
                           currentRound: Int
                         ) extends WithStatus with WithDeck:
  override def deck: List[Card] = deck

  override def gameStatus: GameStatus = GameStatus.InProgress()