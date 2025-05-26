package view.lobbyphase.components

import view.lobbyphase.{IViewListener, InitialPhaseNamesEnum, ScreenNavigator}

import java.awt.Font
import scala.swing.{Alignment, BoxPanel, Button, Label, Orientation, Swing}
import scala.swing.event.ButtonClicked

class WelcomePanel(navigator: ScreenNavigator, viewListener: IViewListener) extends BoxPanel(Orientation.Vertical):
  border = Swing.EmptyBorder(30, 30, 30, 30) // Margine interno

  private val welcomeMessage = new Label("Benvenuto in Cabo Online!") {
    font = new Font("Arial", java.awt.Font.BOLD, 22)
    horizontalAlignment = Alignment.Center
  }
  private val chooseActionsLabel = new Label("Scegli come giocare:") {
    font = new Font("Arial", java.awt.Font.BOLD, 16)
    horizontalAlignment = Alignment.Center
  }

  private val createGameButton = new Button("Crea nuova partita")
  private val askToServerGameButton = new Button("Unisciti ad una partita")
  private val joinAGameWithLinkButton = new Button("Unisciti mediante link")

  contents += welcomeMessage
  contents += Swing.VStrut(20) // Spazio verticale
  contents += chooseActionsLabel
  contents += Swing.VStrut(30)
  contents += createGameButton
  contents += Swing.VStrut(15)
  contents += askToServerGameButton
  contents += Swing.VStrut(15)
  contents += joinAGameWithLinkButton
  contents += Swing.VGlue

  listenTo(createGameButton, askToServerGameButton, joinAGameWithLinkButton)

  reactions += {
    case ButtonClicked(b) =>
      if b == createGameButton then
        println("WelcomePanel: Cliccato 'Crea nuova partita'. Chiedo al navigatore di mostrare 'createGameScreen'.")
        navigator.showScreen(InitialPhaseNamesEnum.CreateGamePanel)
      else if b == askToServerGameButton then
        println("WelcomePanel: Cliccato 'Unisciti ad una partita'.")
        navigator.showScreen(InitialPhaseNamesEnum.JoinGamePanel)
        viewListener.requestGames()
      else if b == joinAGameWithLinkButton then
        println("WelcomePanel: Cliccato 'Unisciti mediante link'.")
        navigator.showScreen(InitialPhaseNamesEnum.JoinGameWithLinkPanel)
  }
