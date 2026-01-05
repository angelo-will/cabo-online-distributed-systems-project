package view.pregamephase.components

import java.awt.{Dimension, Font}
import scala.swing._
import scala.swing.event.ButtonClicked

trait IShowPanels:
  def showCreateGame(): Unit

  def showListGamesFromServer(): Unit

  def showJoinGameWithLink(): Unit

trait IChangeNameBehavior:
  def changeName(newName: String): Unit

class WelcomePanel(showPanels: IShowPanels, playerName: String, changeNameBehavior: IChangeNameBehavior) extends BoxPanel(Orientation.Vertical):
  border = Swing.EmptyBorder(30, 30, 30, 30)

  private val welcomeMessage = new Label("Welcome in Cabo Online!") {
    font = new Font("Arial", java.awt.Font.BOLD, 22)
    horizontalAlignment = Alignment.Center
  }
  private val chooseActionsLabel = new Label("What do you want to do?") {
    font = new Font("Arial", java.awt.Font.BOLD, 16)
    horizontalAlignment = Alignment.Center
  }
  private val actualPlayerNameLabel = new Label("Player name:") {
    font = new Font("Arial", java.awt.Font.ITALIC, 12)
    horizontalAlignment = Alignment.Center
  }
  private val actualPlayerName = new Label(playerName)

  private val changeNameTextField = new swing.TextField(playerName, 12) {
    font = new Font("Arial", java.awt.Font.PLAIN, 12)
    horizontalAlignment = Alignment.Center
    maximumSize = new Dimension(400, 100)
  }

  private val errorLabel = new Label("") {
    foreground = new Color(255, 0, 0)
  }

  private val changeNameButton = new Button("Set name")
  private val createGameButton = new Button("Create new game")
  private val askToServerGameButton = new Button("View joinable game")
  private val joinAGameWithLinkButton = new Button("Join a game with game code")

  contents += welcomeMessage
  contents += Swing.VStrut(20)
  contents += actualPlayerNameLabel
  contents += actualPlayerName
  contents += changeNameTextField
  contents += changeNameButton
  contents += errorLabel
  contents += Swing.VStrut(20)
  contents += chooseActionsLabel
  contents += Swing.VStrut(30)
  contents += createGameButton
  contents += Swing.VStrut(15)
  contents += askToServerGameButton
  contents += Swing.VStrut(15)
  contents += joinAGameWithLinkButton
  contents += Swing.VGlue

  listenTo(createGameButton, askToServerGameButton, joinAGameWithLinkButton, changeNameButton)

  reactions += {
    case ButtonClicked(b) =>
      if b == createGameButton then
        showPanels.showCreateGame()
      else if b == askToServerGameButton then
        showPanels.showListGamesFromServer()
      else if b == joinAGameWithLinkButton then
        showPanels.showJoinGameWithLink()
      else if b == changeNameButton then
        if newNameIsValid(changeNameTextField.text) then
          errorLabel.text = ""
          actualPlayerName.text = changeNameTextField.text
          changeNameBehavior.changeName(actualPlayerName.text)
        else
          errorLabel.text = "<html>Name inserted is not valid." +
            "<br>It must have 5-12 characters:" +
            "<br>alphanumeric and underscore.</html>"

  }

  private def newNameIsValid(newName: String): Boolean =
    val regex = "^[a-zA-Z0-9_]{5,12}$".r
    newName match
      case regex(_*) => true
      case _ => false