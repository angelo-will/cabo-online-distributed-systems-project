package view.gamephase.components

import java.awt.{Color, Font as AwtFont}
import javax.swing.BorderFactory
import scala.swing.event.ButtonClicked
import scala.swing.*

class PlayerPanel(val playerName: String,val f: (index: Int) => Unit) extends BoxPanel(Orientation.Vertical) {
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
      }
    }
  }

  private val cards: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    seqButtonCards.foreach(b => contents += b)
  }

  def enableCardsButton(enable: Boolean): Unit = {
    seqButtonCards.foreach(b => b.enabled = enable)
  }

  def setAsPlaying(isPlaying: Boolean): Unit = {
    if isPlaying then
      peer.setBorder(BorderFactory.createLineBorder(Color.GREEN, 3))
    else
      peer.setBorder(null)
    peer.getParent.revalidate()
    peer.repaint()
  }

  contents += nameLabel
  contents += Swing.VStrut(5)
  contents += cards
}
