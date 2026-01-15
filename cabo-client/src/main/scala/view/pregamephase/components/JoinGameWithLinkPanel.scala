package view.pregamephase.components

import view.pregamephase.IPreGameViewListener
import view.pregamephase.ScreenNavigator

import java.awt.Font
import scala.swing._
import scala.swing.event.ButtonClicked

trait IJoinGameWithLinkListener:
  def joinWithGameCode(gameCode: String): Unit

  def returnToStart(): Unit

class JoinGameWithLinkPanel(navigator: ScreenNavigator, viewListener: IJoinGameWithLinkListener) extends BoxPanel(Orientation.Vertical):
  border = Swing.EmptyBorder(30, 30, 30, 30)

  private val titleLabel = new Label("Join a game with game code") {
    font = new Font("Arial", java.awt.Font.BOLD, 20)
    horizontalAlignment = Alignment.Center
  }

  private val gameInsertGameCodeLabel = new Label("Insert game code")
  private val gameCodeField = new TextField("Game code") {
    columns = 20
    maximumSize = new Dimension(300, preferredSize.height)
  }

  private val pasteLinkButton = new Button("Paste game code")
  private val joinButton = new Button("Join")
  private val backButton = new Button("Back")

  contents += titleLabel
  contents += Swing.VStrut(20)
  contents += gameInsertGameCodeLabel
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
        viewListener.joinWithGameCode(gameCodeField.text)
      else if b == backButton then
        viewListener.returnToStart()
        navigator.goToPreviousPanel()
      else if b == pasteLinkButton then
        val clipboard = java.awt.Toolkit.getDefaultToolkit.getSystemClipboard
        val contents = clipboard.getContents(null)
        if contents != null && contents.isDataFlavorSupported(java.awt.datatransfer.DataFlavor.stringFlavor) then
          val clipboardText = contents.getTransferData(java.awt.datatransfer.DataFlavor.stringFlavor).asInstanceOf[String]
          gameCodeField.text = clipboardText
  }
