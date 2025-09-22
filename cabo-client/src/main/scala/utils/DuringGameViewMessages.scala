package utils

import akka.actor.typed.ActorRef
import model.{Card, Game, PlayerPlaying, TurnLog}
import model.Game.GameInProgress
import utils.InitialViewMessages.ViewCommand

object DuringGameViewMessages {

  trait DuringGameViewMessage extends Message

  trait DuringGameViewCommand extends DuringGameViewMessage

  case class StartGame(game: GameInProgress, gameCoordinatorRef: ActorRef[GameCoordinatorMessage.PlayerCommand]) extends DuringGameViewCommand
  
  case class StartPlayPhase() extends DuringGameViewMessage

  /**
   * Send when i have to play first turn of the game.
   */
  case class FirstTurn() extends DuringGameViewCommand

  case class LastTurnPlayed(turnLog: TurnLog, game: GameInProgress, isMyTurn: Boolean) extends DuringGameViewCommand
  
//  case class MyTurn(lastTurnPlayed: LastTurnPlayed) extends DuringGameViewCommand

  // To use when an adversary is unreachable, POSSIAMO USARLO O MENO 
  case class AdversaryIsDisconnected(playerPlaying: PlayerPlaying) extends DuringGameViewCommand

  // To use when i can't communicate with others
  case class LostMyConnection() extends DuringGameViewCommand

  // Da usare quando si esce dalla partita volontariamente o meno
  case class ExitFromTheGame() extends DuringGameViewCommand

  // Quando chiedo di scambiare una mia carta con l'avversario mi aspetto una risposta per sapere che è avvenuto
  case class ChangeCardWithAdversaryAck() extends DuringGameViewCommand

  case class CardDrawn(card: Card) extends DuringGameViewCommand

  case class NewTopCardDiscardStack(card: Card) extends DuringGameViewCommand

  case class GameInformation(game: Game.GameInProgress) extends DuringGameViewCommand

  case class CardSeen(card: Card) extends DuringGameViewCommand

  case class PlayThisTurn(viewRef: ActorRef[ViewCommand]) extends DuringGameViewCommand
  
  // TODO: create message of game ending with the winner, his points, ecc.
  // i parametri attuali sono solo indicativi
  // case class GameEnded(winner: PlayerPlaying, points: Int) extends DuringGameViewCommand
  
  trait DuringGameUserInterfaceCommand extends DuringGameViewMessage
  
  case class OwnCardSelected(index: Int) extends DuringGameUserInterfaceCommand
}
