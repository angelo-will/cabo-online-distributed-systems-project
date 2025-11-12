package model

import model.Suit.Spades
import akka.serialization.jackson.CborSerializable
import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}
import model.GameStatus.{InProgress, WaitingForPlayers, Finished}

object Game:
  // 0-index
  val maxPlayersPerGame = 4

  // 0-index
  val maxCardsPerGame = 4

  val cardsInitialVisible = 2

  case class CaboState(whoCalledCabo: PlayerPlaying)
  
  case class GameInConstruction(
                                 code: String,
                                 gameParameters: IGameParameters,
                                 players: List[PlayerInLobby],
                               ):
    def gameStatus: GameStatus = GameStatus.WaitingForPlayers()

    def playersAddress: List[String] = players.map(_.address.path.toString)

  case class GameInProgress(
                             code: String,
                             gameParameters: IGameParameters,
                             gameStatus: GameStatus,
                             players: List[PlayerPlaying],
                             deckStack: CardStack,
                             discardDeckStack: CardStack,
                             currentRound: Int,
                             caboState: Option[CaboState] = None,
                           ):

    override def toString: String = "GameInProgress\n" +
      "\tcode=" + code + "\n" +
      "\tgameParameters=" + gameParameters + "\n" +
      "\tgameStatus=" + gameStatus + "\n" +
      "\tplayers=" + players + "\n" +
      "\tdeckStack=" + deckStack + "\n" +
      "\tdiscardDeckStack=" + discardDeckStack + "\n" +
      "\tcurrentRound=" + currentRound + "\n" +
      "\tcaboState=" + {if caboState.isEmpty then "Nobody've called cabo" else s"${caboState.get.whoCalledCabo.userID} has called cabo"} + "\n"

    def getPlayerWithID(userID: String): PlayerPlaying = PlayerPlaying.getPlayerWithID(userID, this.players)

    def getCardOfPlayerWithID(userID: String, index: Int): Card = this.getPlayerWithID(userID).getCard(index)

    def getHandOfPlayerWithID(userID: String): Hand = this.getPlayerWithID(userID).hand

    def replaceHandOfPlayerWithID(userID: String, hand: Hand): GameInProgress =
      this.copy(players = PlayerPlaying.replaceHandOfPlayerWithID(userID, hand, this.players))

    def replaceNthCardOfPlayerWithID(userID: String, card: Card, index: Int): GameInProgress =
      this.copy(players = PlayerPlaying.replaceNthCardOfPlayerWithID(userID, card, index, this.players))
      
    def isCaboCalled: Boolean = 
      this.caboState.isDefined  


@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(value = classOf[GameStatus.WaitingForPlayers], name = "waitingForPlayers"),
    new JsonSubTypes.Type(value = classOf[GameStatus.InProgress], name = "inProgress"),
    new JsonSubTypes.Type(value = classOf[GameStatus.Finished], name = "finished")))
abstract class GameStatus(val status: String)

object GameStatus:
  case class WaitingForPlayers() extends GameStatus("WaitingForPlayers")

  case class InProgress() extends GameStatus("InProgress")

  case class Finished() extends GameStatus("Finished")
