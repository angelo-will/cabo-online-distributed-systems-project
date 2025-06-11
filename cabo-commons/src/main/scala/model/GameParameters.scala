package model

import akka.serialization.jackson.CborSerializable
import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(value = classOf[NoRoundLimitation], name = "noRoundLimitation"),
    new JsonSubTypes.Type(value = classOf[RoundLimitation], name = "roundLimitation")))
abstract class RoundLimitationParameter:
  def isRoundsEnded: Boolean

final case class NoRoundLimitation() extends RoundLimitationParameter:
  override def isRoundsEnded: Boolean = false

final case class RoundLimitation(maxRound: Int) extends RoundLimitationParameter:
  override def isRoundsEnded: Boolean = maxRound <= 0

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(value = classOf[GameParameters], name = "gameParameters")))
trait IGameParameters extends CborSerializable:
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