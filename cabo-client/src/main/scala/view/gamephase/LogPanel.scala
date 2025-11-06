package view.gamephase

import java.awt.{Color, Font as AwtFont}
import javax.swing.BorderFactory
import scala.swing.{ScrollPane, TextArea}
import model.TurnLog
import model.TurnEvent

private class LogPanel() extends ScrollPane {
  private val logTextArea = new TextArea {
    editable = false
    lineWrap = true
    wordWrap = true
    font = new AwtFont("Arial", AwtFont.PLAIN, 12)
    text = "Cards seen in initial phase:\n"
  }
  contents = logTextArea
  verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
  peer.setBorder(BorderFactory.createLineBorder(Color.MAGENTA, 3))

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
    str
  }

  def simpleTurnLog(turnLog: TurnLog): String = {
    var string = s"Player ${turnLog.playerName} in round ${turnLog.round} "
    turnLog.events.foreach {
      case TurnEvent.DrawCardFromDeck(card) => string += s"has drawn ${card} from deck, "
      case TurnEvent.DrawCardFromDiscardStack(card) => string += s"has drawn ${card} from discard stack, "
      case TurnEvent.SeeSelfCard(index) => string += s"has seen its card $index, "
      case TurnEvent.SeeAdversaryCard(adversaryID, index) => string += s"has seen card $index of $adversaryID, "
      case TurnEvent.ReplaceOwnCardWithAdversaryCard(itsCardIndex, adversaryID, adversaryCardIndex) =>
        string += s"has changed its card $itsCardIndex with $adversaryID's $adversaryCardIndex one, "
      case TurnEvent.OwnCardDiscarded(card, index) => string += s"has kept card drawn and discarded $index-th card.\nThat's $card.\n"
      case TurnEvent.CardDrawnDiscarded(card) => string += s"has discarded drawn card $card."
      case TurnEvent.JumpTurnForTimerEnded() => string += s"has ended its turn for timer ended."
    }
    string
  }
