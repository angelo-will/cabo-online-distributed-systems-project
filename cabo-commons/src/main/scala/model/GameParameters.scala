package model

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}
import utils.CborSerializable

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(value = classOf[NoRoundLimitation], name = "noRoundLimitation"),
    new JsonSubTypes.Type(value = classOf[RoundLimitation], name = "roundLimitation")))
abstract class RoundLimitationParameter:
  def isRoundsEnded(currentRound: Int): Boolean

final case class NoRoundLimitation() extends RoundLimitationParameter:
  override def isRoundsEnded(currentRound: Int): Boolean = false

final case class RoundLimitation(maxRound: Int) extends RoundLimitationParameter:
  override def isRoundsEnded(currentRound: Int): Boolean = currentRound > maxRound

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

  final val defaultMaxTimeRound: Int = 10 // default max time for a round in seconds
  final val defaultRoundLimitation: Int = 0 // default round limitation, 0 means no limitation
  final val defaultMaxPlayers: Int = 5 // default maximum number of players in a game
  final val defaultIsPublic: Boolean = false // default game visibility

  def apply(
             isPublic: Boolean = defaultIsPublic,
             maxTimeRound: Int = defaultMaxTimeRound,
             roundLimitation: Int = defaultRoundLimitation,
             maxPlayers: Int = defaultMaxPlayers
           ): GameParameters =
    new GameParameters(
      isPublic,
      maxTimeRound,
      if roundLimitation > 0 then RoundLimitation(roundLimitation) else NoRoundLimitation(),
      maxPlayers match {
        case p if p < 2 => 2 // minimum players
        case p => p
      })

case class GameParameters private(
                                   isPublic: Boolean,
                                   maxTimeRound: Int,
                                   roundLimitation: RoundLimitationParameter,
                                   maxPlayers: Int
                                 ) extends IGameParameters