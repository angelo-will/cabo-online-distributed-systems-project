package view.gamephase.components

import java.awt.Font as AwtFont
import scala.swing.event.ButtonClicked
import scala.swing.*

class DeckPanel(labelStack: String, stackValue: String, buttonAction: () => Unit) extends BoxPanel(Orientation.Vertical) {
  border = Swing.EmptyBorder(10, 10, 10, 10)
  private val discardLabel = new Label(labelStack) {
    font = new AwtFont("Arial", AwtFont.BOLD, 14)
    horizontalAlignment = Alignment.Center
  }

  val deckButton: Button = new Button(stackValue) {
    font = new AwtFont("Arial", AwtFont.PLAIN, 24)
    border = Swing.EmptyBorder(5, 5, 5, 5)
  }
  listenTo(deckButton)

  reactions += {
    case ButtonClicked(`deckButton`) =>
      println(s"$labelStack button clicked")
      buttonAction()
  }

  def updateStackValue(newValue: String): Unit = {
    deckButton.text = newValue
  }

  contents += discardLabel
  contents += Swing.VStrut(5)
  contents += deckButton
}
