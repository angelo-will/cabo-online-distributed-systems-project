package view.ui.components

import model.Game.GameInConstruction
import view.IViewListener
import view.ui.ScreenNavigator

import java.awt.{Color, Font}
import javax.swing.SwingUtilities
import scala.swing.{Alignment, BorderPanel, BoxPanel, Button, Dialog, Label, Orientation, ScrollPane, Swing}
import scala.swing.event.ButtonClicked

class GameListPanel(navigator: ScreenNavigator, listener: IViewListener) extends ScrollPane {
  private val listContainer = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(10, 10, 10, 10)
    contents += new Label("Caricamento partite in corso...")
  }

  contents = listContainer

  def updateGameList(games: Seq[GameInConstruction]): Unit = {
    SwingUtilities.invokeLater(() => { 
      listContainer.contents.clear()

      if (games.isEmpty) {
        listContainer.contents += new Label("Nessuna partita disponibile al momento.") {
          horizontalAlignment = Alignment.Center
          font = new Font("SansSerif", java.awt.Font.ITALIC, 14)
        }
      } else {
        games.foreach { game =>
          listContainer.contents += new GameRowPanel(game, listener)
          listContainer.contents += Swing.VStrut(5) 
        }
      }
      listContainer.revalidate() 
      listContainer.repaint() 
    })
  }
}

class GameRowPanel(game: GameInConstruction, listener: IViewListener) extends BorderPanel {
  border = Swing.LineBorder(Color.LIGHT_GRAY, 1)
  background = Color.WHITE

  val playerCountLabel = new Label(s"${game.players.size}/${game.gameParameters.maxPlayers} Giocatori") {
    font = new Font("SansSerif", java.awt.Font.BOLD, 14)
  }

  val gameNameLabel = new Label(game.code) {
    font = new Font("SansSerif", java.awt.Font.PLAIN, 12)
    foreground = Color.DARK_GRAY
  }

  val infoButton = new Button("Info")
  val joinButton = new Button("Entra")

  layout(new BoxPanel(Orientation.Vertical) {
    contents += gameNameLabel
    contents += Swing.VStrut(5)
    contents += playerCountLabel
  }) = BorderPanel.Position.Center

  layout(new BoxPanel(Orientation.Horizontal) {
    contents += infoButton
    contents += Swing.HStrut(10) 
    contents += joinButton
  }) = BorderPanel.Position.East 

  listenTo(infoButton, joinButton)

  reactions += {
    case ButtonClicked(b) =>
      if b == infoButton then
        showInfoDialog()
      else if b == joinButton then
        showJoinConfirmationDialog()
        // TODO: inserire la chiamata al listener corretta per partecipare al game
        listener.joinAGame("")
  }

  private def showInfoDialog(): Unit = {
    val details =
      s"""<html>
         |<b>ID:</b> ${game.code}<br>
         |<b>Nome:</b> ${game.code}<br>
         |<b>Giocatori:</b> ${game.players.size}/${game.gameParameters.maxPlayers}<br>
         |</html>""".stripMargin

    Dialog.showMessage(this, details, "Dettagli Partita: " + game.code, Dialog.Message.Info)
  }

  // Metodo per mostrare il dialog di conferma per entrare in partita
  private def showJoinConfirmationDialog(): Unit = {
    val result = Dialog.showConfirmation(
      this,
      s"Vuoi davvero unirti alla partita '${game.code}'?",
      "Conferma Unione Partita",
      Dialog.Options.YesNo,
      Dialog.Message.Question
    )

    if (result == Dialog.Result.Yes) {
      val waitingDialog = new Dialog() {
        title = "Attesa"
        modal = true 
        contents = new BoxPanel(Orientation.Vertical) {
          border = Swing.EmptyBorder(20, 20, 20, 20)
          contents += new Label(s"Tentativo di unione alla partita '${game.code}'...") {
            font = new Font("SansSerif", java.awt.Font.BOLD, 14)
          }
          contents += Swing.VStrut(10)
          contents += new Label("Attendere prego...")
        }
        pack() 
        centerOnScreen()
      }


    }
  }
}
