package model

abstract class RoundLimitationParameter():
  def isRoundsEnded: Boolean

case class NoRoundLimitation() extends RoundLimitationParameter:
  override def isRoundsEnded: Boolean = false

case class RoundLimitation(maxRound: Int) extends RoundLimitationParameter:
  override def isRoundsEnded: Boolean = maxRound <= 0

trait IGameParameters:
  def isPrivate: Boolean
  def maxTimeRound: Int
  def roundLimitation: RoundLimitationParameter
  def maxPlayers: Int

// companion object with constructor for GameParameters
object GameParameters:
  def apply(
             makePrivate: Boolean = false,
             maxTimeRound: Int = 10,
             roundLimitation: Int = 0,
             maxPlayers: Int = 5
           ): GameParameters =
    new GameParameters(
      makePrivate,
      maxTimeRound, 
      if roundLimitation > 0 then RoundLimitation(roundLimitation) else NoRoundLimitation(), 
      maxPlayers)

case class GameParameters private (
                                    isPrivate: Boolean,
                                    maxTimeRound: Int,
                                    roundLimitation: RoundLimitationParameter,
                                    maxPlayers: Int
                         ) extends IGameParameters