package view.gamephase

import model.Game.GameInProgress
import model.Hand

import java.awt.Font
import scala.swing.{BoxPanel, Dialog, Label, Orientation, Swing}

class DisplayEndingResultsDialog(gameResult: GameInProgress) extends Dialog {

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
    x.zipWithIndex.foreach{
      case (pl,index) =>
        val player = new Label(s"${index+1}° - " + pl)
        player.font = if index == 0 then new Font("SansSerif", java.awt.Font.BOLD, 20)
        else new Font("SansSerif", java.awt.Font.PLAIN, 15)
        contents += player
        contents += Swing.VStrut(10)
    }
    contents += new Label("If you close this windows you'll return to initial frame")
  }
  pack()
  centerOnScreen()
}
