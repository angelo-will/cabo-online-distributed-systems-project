package view.ui.components

import view.IViewListener
import view.ui.{InitialPhaseNamesEnum, ScreenNavigator}

import java.awt.Font
import scala.swing.{Alignment, BoxPanel, Button, Label, Orientation, Swing, TextField}
import scala.swing.event.ButtonClicked

class JoinGameWithLinkPanel(navigator: ScreenNavigator, viewListener: IViewListener) extends BoxPanel(Orientation.Vertical):
  border = Swing.EmptyBorder(30, 30, 30, 30) // Margine interno

  private val titleLabel = new Label("Unisciti a una partita mediante link") {
    font = new Font("Arial", java.awt.Font.BOLD, 20)
    horizontalAlignment = Alignment.Center
  }

  private val gameInsertLinkLabel = new Label("Inserire link partita")
  // TODO: in base a come si sarà scelto di fare (actorRef, adress, ecc.) modificare
  private val gameCodeField = new TextField("Link partita") {
    columns = 20
  }

  private val joinButton = new Button("Unisciti")
  private val backButton = new Button("Indietro")

  contents += titleLabel
  contents += Swing.VStrut(20)
  contents += gameInsertLinkLabel
  contents += gameCodeField
  contents += Swing.VStrut(10)
  contents += joinButton
  contents += backButton
  contents += Swing.VGlue

  listenTo(joinButton, backButton)

  reactions += {
    case ButtonClicked(b) =>
      if b == joinButton then
        println(s"Premuto bottone unisciti alla partita con codice: ${gameCodeField.text}")
        // TODO: delete remove this than -AAA- sostituire con il metodo corretto
        viewListener.joinWithAddress(gameCodeField.text)
      else if b == backButton then
        println("JoinGamePanel: Cliccato 'Indietro'. Chiedo al navigatore di mostrare 'welcomeScreen'.")
        navigator.showScreen(InitialPhaseNamesEnum.WelcomePanel) 
  }
