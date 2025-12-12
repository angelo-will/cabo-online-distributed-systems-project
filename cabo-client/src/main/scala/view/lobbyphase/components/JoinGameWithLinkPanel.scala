package view.lobbyphase.components

import view.lobbyphase.ViewListener.IPreGameViewListener
import view.lobbyphase.ScreenNavigator

import java.awt.Font
import scala.swing.{Alignment, BoxPanel, Button, Dimension, Label, Orientation, Swing, TextField}
import scala.swing.event.ButtonClicked

trait IJoinGameWithLinkListener:
  def joinWithAddress(address: String): Unit

  def returnToStart(): Unit

class JoinGameWithLinkPanel(navigator: ScreenNavigator, viewListener: IJoinGameWithLinkListener) extends BoxPanel(Orientation.Vertical):
  border = Swing.EmptyBorder(30, 30, 30, 30) // Margine interno

  private val titleLabel = new Label("Unisciti a una partita mediante link") {
    font = new Font("Arial", java.awt.Font.BOLD, 20)
    horizontalAlignment = Alignment.Center
  }

  private val gameInsertLinkLabel = new Label("Inserire link partita")
  // TODO: in base a come si sarà scelto di fare (actorRef, adress, ecc.) modificare
  private val gameCodeField = new TextField("Link partita") {
    columns = 20
    maximumSize = new Dimension(300, preferredSize.height)
  }

  private val pasteLinkButton = new Button("Paste game code")
  private val joinButton = new Button("Join")
  private val backButton = new Button("Go Back")

  contents += titleLabel
  contents += Swing.VStrut(20)
  contents += gameInsertLinkLabel
  contents += gameCodeField
  contents += Swing.VStrut(10)
  contents += pasteLinkButton
  contents += joinButton
  contents += backButton
  contents += Swing.VGlue

  listenTo(joinButton, backButton, pasteLinkButton)

  reactions += {
    case ButtonClicked(b) =>
      if b == joinButton then
        println(s"Premuto bottone unisciti alla partita con codice: ${gameCodeField.text}")
        // TODO: delete remove this than -AAA- sostituire con il metodo corretto
        viewListener.joinWithAddress(gameCodeField.text)
      else if b == backButton then
        println("JoinGamePanel: Cliccato 'Indietro'. Chiedo al navigatore di mostrare 'welcomeScreen'.")
        viewListener.returnToStart()
        navigator.goToPreviousPanel()
      else if b == pasteLinkButton then
        val clipboard = java.awt.Toolkit.getDefaultToolkit.getSystemClipboard
        val contents = clipboard.getContents(null)
        if contents != null && contents.isDataFlavorSupported(java.awt.datatransfer.DataFlavor.stringFlavor) then
          val clipboardText = contents.getTransferData(java.awt.datatransfer.DataFlavor.stringFlavor).asInstanceOf[String]
          gameCodeField.text = clipboardText
  }
