package model

abstract class GameVisibility(val name: String)

object GameVisibility:
  case class Public() extends GameVisibility("Public")
  case class Private() extends GameVisibility("Private")

abstract class RoundLimitationParameter():
  def isRoundsEnded: Boolean

case class NoRoundLimitation() extends RoundLimitationParameter:
  override def isRoundsEnded: Boolean = false

case class RoundLimitation(maxRound: Int) extends RoundLimitationParameter:
  override def isRoundsEnded: Boolean = maxRound <= 0

case class GameParameters(
                           gameVisibility: GameVisibility = GameVisibility.Private(),
                           maxTimeRound: Int,
                           roundLimitation: RoundLimitationParameter = NoRoundLimitation(),
                           maxPlayers: Int = 5
                         )