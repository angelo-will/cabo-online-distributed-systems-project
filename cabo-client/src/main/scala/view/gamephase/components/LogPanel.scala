package view.gamephase.components

import model.{TurnEvent, TurnLog}

import java.awt.{Color, Font as AwtFont}
import javax.swing.BorderFactory
import scala.swing.{ScrollPane, TextArea}

class LogPanel extends ScrollPane {
  private val logTextArea = new TextArea {
    editable = false
    lineWrap = true
    wordWrap = true
    font = new AwtFont("Arial", AwtFont.PLAIN, 12)
    text = "Cards seen in revealing section by opponents:\n"
  }
  contents = logTextArea
  verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
  //  peer.setBorder(BorderFactory.createLineBorder(Color.MAGENTA, 3))

  def updateRevealingPhaseLog(log: TurnLog): Unit = {
    logTextArea.text += TurnLogsWriter.initialPhaseTurnLog(log)
  }

  def updateLastTurnLog(log: TurnLog): Unit = {
    this.logTextArea.text = "Last turn played:\n" + TurnLogsWriter.simpleTurnLog(log)
  }
}

private object TurnLogsWriter:
  def initialPhaseTurnLog(log: TurnLog): String = {
    var str = "Player " + log.playerName + " has seen cards: "
    log.events.foreach {
      case TurnEvent.SeeSelfCard(index) => str += s"$index, "
      case _ =>
    }
    str + "\n"
  }

  def simpleTurnLog(turnLog: TurnLog): String = {
    var string = s"Player ${turnLog.playerName} in round ${turnLog.round} has:\n"
    turnLog.events.foreach {
      case TurnEvent.DrawCardFromDeck(card) => string += s"- drawn $card from deck;\n"
      case TurnEvent.DrawCardFromDiscardStack(card) => string += s"- drawn $card from discard stack;\n"
      case TurnEvent.SeeSelfCard(index) => string += s"- seen its card $index;\n"
      case TurnEvent.SeeAdversaryCard(adversaryID, index) => string += s"- seen card $index of $adversaryID;\n"
      case TurnEvent.ReplaceOwnCardWithAdversaryCard(itsCardIndex, adversaryID, adversaryCardIndex) =>
        string += s"- changed its card $itsCardIndex with $adversaryID's $adversaryCardIndex one;\n"
      case TurnEvent.OwnCardDiscarded(card, index) => string += s"- kept card drawn and discarded $index-th card. That's $card;\n"
      case TurnEvent.CardDrawnDiscarded(card) => string += s"- discarded drawn card $card;\n"
      case TurnEvent.EndTurn() => ""
      case TurnEvent.CaboCalled() => string += s"- has called CABO!"
      case TurnEvent.JumpTurnForTimerEnded() => string += s"- ended its turn for timer ended."
      case TurnEvent.JumpTurnForDisconnection() => string += s"- jumped its turn for disconnection."
    }
    string
  }
