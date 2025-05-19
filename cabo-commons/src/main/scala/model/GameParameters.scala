package model

import model.GameVisibility.{Private, Public}

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

trait IGameParameters:
  def gameVisibility: GameVisibility
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
      if makePrivate then Private() else Public(), 
      maxTimeRound, 
      if roundLimitation > 0 then RoundLimitation(roundLimitation) else NoRoundLimitation(), 
      maxPlayers)

case class GameParameters private (
                           gameVisibility: GameVisibility,
                           maxTimeRound: Int,
                           roundLimitation: RoundLimitationParameter,
                           maxPlayers: Int
                         ) extends IGameParameters