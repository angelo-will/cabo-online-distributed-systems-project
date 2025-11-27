package view.gamephase.components

import model.Game.GameInProgress
import DisplayEndingResultsDialog.*
import model.{EndGameReason, Hand}
import model.EndGameReason.*

import java.awt.Font
import scala.swing.*
import scala.swing.event.WindowClosing

object DisplayEndingResultsDialog {

  def apply(gameResult: GameInProgress)(ending: EndGameReason)(onClose: () => Unit): DisplayEndingResultsDialog =
    new DisplayEndingResultsDialog(gameResult)(ending)(onClose)
}

private class DisplayEndingResultsDialog(gameResult: GameInProgress)(ending: EndGameReason)(onClose: () => Unit) extends Dialog {


  private case class DataDisplay(playerName: String, score: Int, hand: String) {
    override def toString: String = s"$playerName,  $score points - $hand"
  }

  title = "Ending Results"
  //todo: set modal to true before deployment
  modal = false
  private val results = gameResult.players.sortBy(pl => pl.hand.score)
  private val x = results.map(pl => {
    val hand = pl.hand.cards.map(card => s"$card").mkString(", ")
    DataDisplay(pl.name, pl.hand.score, "Hand: " + hand)
  })

  contents = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(20, 20, 20, 20)
    contents += new Label(s"Game Ended!") {
      font = new Font("SansSerif", java.awt.Font.BOLD, 30)
    }

    contents += Swing.VStrut(10)
    private val howGameEnd = ending match
      case Cabo => s"Cabo called by ${gameResult.caboState.get.name}"
      case TurnsLimit => "Reached maximum turns number"
      case EmptyDeck => "Cards in deck are ended"
    contents += new Label(howGameEnd) {
      font = new Font("SansSerif", java.awt.Font.PLAIN, 25)
    }
    contents += Swing.VStrut(10)
    x.zipWithIndex.foreach {
      case (pl, index) =>
        val player = new Label(s"${index + 1}° - " + pl)
        player.font = if index == 0 then new Font("SansSerif", java.awt.Font.BOLD, 20)
        else new Font("SansSerif", java.awt.Font.PLAIN, 15)
        contents += player
        contents += Swing.VStrut(10)
    }
    contents += new Label("If you close this windows you'll return to initial frame")
  }
  pack()
  centerOnScreen()

  listenTo(this)
  reactions += {
    case WindowClosing(_) =>
      onClose()
      this.dispose()
  }
}
