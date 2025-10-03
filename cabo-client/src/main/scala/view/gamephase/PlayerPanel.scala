package view.gamephase

import java.awt.{Color, Font as AwtFont}
import javax.swing.BorderFactory
import scala.swing.{Alignment, BoxPanel, Button, Label, Orientation, Swing}
import scala.swing.event.ButtonClicked

private class PlayerPanel(playerName: String, f: (index: Int) => Unit) extends BoxPanel(Orientation.Vertical) {
  //  border = Swing.EmptyBorder(10, 10, 10, 10)
  peer.setBorder(BorderFactory.createLineBorder(Color.BLUE, 3))
  private val nameLabel = new Label(playerName) {
    font = new AwtFont("Arial", AwtFont.BOLD, 14)
    horizontalAlignment = Alignment.Center
  }

  private val seqButtonCards: IndexedSeq[Button] = for (i <- 0 to 3) yield {
    //      contents += new Button(s"$i") {
    new Button(s"$i") {
      font = new AwtFont("Arial", AwtFont.PLAIN, 24)
      border = Swing.EmptyBorder(0, 5, 0, 5)
      enabled = false
      reactions += {
        case ButtonClicked(_) =>
          println(s"Player '$playerName' card $i clicked")
          f(i)
        //          f(i - 1) // Call the function with the index (0-based)
      }
    }
  }

  private val cards: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    seqButtonCards.foreach(b => contents += b)
  }

  def enableCardsButton(enable: Boolean): Unit = {
    seqButtonCards.foreach(b => b.enabled = enable)
  }

  contents += nameLabel
  contents += Swing.VStrut(5)
  contents += cards
}
