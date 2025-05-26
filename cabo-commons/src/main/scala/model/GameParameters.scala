package model

abstract class RoundLimitationParameter():
  def isRoundsEnded: Boolean

case class NoRoundLimitation() extends RoundLimitationParameter:
  override def isRoundsEnded: Boolean = false

case class RoundLimitation(maxRound: Int) extends RoundLimitationParameter:
  override def isRoundsEnded: Boolean = maxRound <= 0

trait IGameParameters:
  def isPublic: Boolean
  def maxTimeRound: Int
  def roundLimitation: RoundLimitationParameter
  def maxPlayers: Int

// companion object with constructor for GameParameters
object GameParameters:
  def apply(
             isPublic: Boolean = false,
             maxTimeRound: Int = 10,
             roundLimitation: Int = 0,
             maxPlayers: Int = 5
           ): GameParameters =
    new GameParameters(
      isPublic,
      maxTimeRound,
      if roundLimitation > 0 then RoundLimitation(roundLimitation) else NoRoundLimitation(), 
      maxPlayers match {
        case p if p < 2 => 2 // minimum players
        case p => p
      })

case class GameParameters private (
                                    isPublic: Boolean,
                                    maxTimeRound: Int,
                                    roundLimitation: RoundLimitationParameter,
                                    maxPlayers: Int
                         ) extends IGameParameters