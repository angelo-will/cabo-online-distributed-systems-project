package view.gamephase

import java.awt.{Color, Font as AwtFont}
import javax.swing.BorderFactory
import scala.swing.{ScrollPane, TextArea}

private class LogPanel() extends ScrollPane {
  private val logTextArea = new TextArea {
    editable = false
    lineWrap = true
    wordWrap = true
    font = new AwtFont("Arial", AwtFont.PLAIN, 12)
    text = ""
  }
  contents = logTextArea
  verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
  peer.setBorder(BorderFactory.createLineBorder(Color.MAGENTA, 3))

  def updateLastTurnLog(string: String): Unit = {
    this.logTextArea.text = "Last turn played: " + string
  }
}

private object TurnLogsWriter:

  import model.TurnLog
  import model.TurnEvent

  def simpleTurnLog(userID: String, roundOfTurn: Int, turnLog: TurnLog): String = {
    var string = s"Player $userID in round $roundOfTurn "
    turnLog.events.foreach {
      case TurnEvent.DrawCardFromDeck(card) => string += s"has drawn ${card} from deck, "
      case TurnEvent.DrawCardFromDiscardStack(card) => string += s"has drawn ${card} from discard stack, "
      case TurnEvent.SeeSelfCard(index) => string += s"has seen its card $index, "
      case TurnEvent.SeeAdversaryCard(adversaryID, index) => string += s"has seen card $index of $adversaryID, "
      case TurnEvent.ReplaceOwnCardWithAdversaryCard(itsCardIndex, adversaryID, adversaryCardIndex) => 
        string += s"has changed its card $itsCardIndex with $adversaryID's $adversaryCardIndex one, "
      case TurnEvent.CardDiscarded(card) => string += s"has discarded $card."
    }
    string
  }
