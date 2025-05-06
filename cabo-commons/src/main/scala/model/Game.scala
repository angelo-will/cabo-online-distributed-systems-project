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
                               players: List[PlayerInLobby],
                             ) extends WithStatus:
  override def gameStatus: GameStatus = GameStatus.WaitingForPlayers()

  def playersAddress: List[String] = players.map(_.address)

case class GameInProgress(
                           gameParameters: IGameParameters,
                           players: List[PlayerPlaying],
                           code: String,
                           currentRound: Int
                         ) extends WithStatus with WithDeck:
  
  override def deck: List[Card] = List(Card("5", Spades()))

  override def gameStatus: GameStatus = GameStatus.InProgress()