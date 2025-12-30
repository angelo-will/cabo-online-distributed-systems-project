package view.gamephase.dialogs

import scala.swing._
import scala.swing.event.WindowClosing


object AllOpponentsDisconnectedDialog {

  def apply(onClose: () => Unit): AllOpponentsDisconnectedDialog =
    new AllOpponentsDisconnectedDialog(onClose)
}

private case class AllOpponentsDisconnectedDialog(onClose: () => Unit) extends Dialog {


  private case class DataDisplay(playerName: String, score: Int, hand: String) {
    override def toString: String = s"$playerName,  $score points - $hand"
  }

  title = "You are the last player connected"
  //todo: set modal to true before deployment
  modal = false

  contents = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(20, 20, 20, 20)
    contents += new Label(s"Game Ended!") {
      font = new Font("SansSerif", java.awt.Font.BOLD, 30)
    }
    contents += Swing.VStrut(10)
    contents += new Label(s"All opponents are disconnected.") {
      font = new Font("SansSerif", java.awt.Font.PLAIN, 20)
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

