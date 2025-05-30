package view.lobbyphase.components

import model.Game.GameInConstruction
import java.awt.Font
import javax.swing.SwingUtilities
import scala.swing.{BoxPanel, Dialog, Label, Orientation, Swing}

//class WaitingAccessToGameDialog(listener: IViewListener, game: GameInConstruction) extends Dialog {
class WaitingAccessToGameDialog extends Dialog {
  title = "Waiting for game host..."
  modal = true
  contents = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(20, 20, 20, 20)
    contents += new Label(s"Trying to enter in the game...") {
      font = new Font("SansSerif", java.awt.Font.BOLD, 14)
    }
    contents += Swing.VStrut(10)
    contents += new Label("Wait please...")
  }
  pack()
  centerOnScreen()
}