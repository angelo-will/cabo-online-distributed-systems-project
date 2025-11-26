package view.lobbyphase.components

import model.Game.GameInConstruction
import model.{Game, PlayerInLobby}
import view.lobbyphase.ScreenNavigator
import view.lobbyphase.ViewListener.IInitialViewListener

import java.awt.{Font, GridBagConstraints, Insets, Toolkit}
import java.awt.GridBagConstraints.*
import java.awt.datatransfer.StringSelection
import javax.swing.SwingUtilities
import scala.swing.Dialog as result
import scala.swing.GridBagPanel.Fill
import scala.swing.event.ButtonClicked
//import scala.swing.{Alignment, BoxPanel, Button, Dialog, Dimension, Label, MainFrame, Orientation, ScrollPane, Swing}
import scala.swing._
import scala.util.Random

trait IWaitingToStartListener:
  def startGame(): Unit

  def exitFromTheGame(): Unit

class WaitingFrame(
                    //                    navigator: ScreenNavigator,
                    listener: IWaitingToStartListener,
                    private var game: GameInConstruction,
                    isHost: Boolean
                  ) extends Frame:
  title = "Waiting Lobby"
  preferredSize = new Dimension(600, 400)
  centerOnScreen()
  peer.setDefaultCloseOperation(
    javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
  )

  override def closeOperation(): Unit = onExitDuringWaitingLobbyPolicy()

  private val playersListContainer = new WaitingLobbyPlayersContainer(
    game.players,
    isHost
  )

  private val copyGameCodeButton = new Button {
    text = "Copy Game Code"
    tooltip = "Click here to copy game code in clipboard"
  }

  private val startGameButton = new Button("Start Game") {
    font = new Font("Arial", java.awt.Font.PLAIN, 16)
    enabled = game.players.size >= 2 && isHost
    // horizontalAlignment = Alignment.Center
  }

  private val exitButton = new Button("Exit") {
    font = new Font("Arial", java.awt.Font.PLAIN, 16)
    preferredSize = new Dimension(startGameButton.preferredSize.width, preferredSize.height)
    // enabled = players.size >= 2 && isHost
    // horizontalAlignment = Alignment.Center
  }

  listenTo(startGameButton, exitButton, copyGameCodeButton)

//  peer.addWindowListener(new java.awt.event.WindowAdapter() {
//    override def windowClosing(e: java.awt.event.WindowEvent): Unit =
//      println("Window closing, exiting from the game.")
//      listener.exitFromTheGame()
//      dispose()
//  })

  reactions += {
    case ButtonClicked(`startGameButton`) =>
      println("Start Game button clicked.")
      listener.startGame()
      this.dispose()
    case ButtonClicked(`exitButton`) =>
      println("Exit button clicked.")
      onExitDuringWaitingLobbyPolicy()
    //      this.dispose()
    case ButtonClicked(`copyGameCodeButton`) =>
      val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
      val selection = new StringSelection(game.code)
      clipboard.setContents(selection, selection)
      println(s"Game code '${game.code}' copied to clipboard.")
  }

  contents = new GridBagPanel {
    border = Swing.EmptyBorder(30, 30, 30, 30)

    // Definisci le componenti
    private val waitingMessage = new Label("Waiting host to start the game.") {
      font = new Font("Arial", java.awt.Font.BOLD, 22)
      horizontalAlignment = Alignment.Center
    }
    //    private val gameProperties = new TextField("<html>" +
    //      "<p>Game Properties</p>" +
    //      "<br> Game Code:" + game.code +
    //      "<br> Max players per game: " + game.gameParameters.maxPlayers +
    //      "<br> Max time per round: " + game.gameParameters.maxTimeRound +
    //      "<br> Max rounds per game: " + game.gameParameters.roundLimitation +
    //      "</html>")
    private val propertiesText: String =
      "Game Properties\n\n" +
        "Game Code: " + game.code + "\n" +
        "Max players per game: " + game.gameParameters.maxPlayers + "\n" +
        "Max time per round: " + game.gameParameters.maxTimeRound + "\n" +
        "Max rounds per game: " + game.gameParameters.roundLimitation
    private val gameProperties = new TextArea {
      text = propertiesText
      editable = false
      border = Swing.EmptyBorder(0)
      opaque = false
      lineWrap = true
    }

    private val introPlayersListLabel = new Label("Players in the game:") {
      font = new Font("Arial", java.awt.Font.BOLD, 16)
      horizontalAlignment = Alignment.Center
    }

    // Definisci i vincoli per la griglia
    val c = new Constraints
    private var row: Int = 0
    private var column: Int = 0

    private def resetColumn(): Unit = column = 0

    private def nextRow(): Unit =
      row += 1
      resetColumn()

    c.fill = GridBagPanel.Fill.Horizontal // I componenti si espanderanno per riempire la loro cella
    // c.insets = new Insets(10, 0, 10, 0) // Padding verticale

    c.gridy = row
    c.gridx = column
    c.gridwidth = GridBagConstraints.REMAINDER
    c.weighty = 1.0
    layout(waitingMessage) = c

    nextRow()

    c.gridy = row
    c.gridx = column
    c.gridwidth = GridBagConstraints.REMAINDER
    c.weighty = 0.0 // Non si espande verticalmente
    layout(gameProperties) = c

    nextRow()

    c.gridy = row
    c.gridx = column
    c.fill = Fill.Horizontal
    layout(copyGameCodeButton) = c
    c.fill = Fill.None

    nextRow()

    // introPlayersListLabel (seconda riga)
    c.gridy = row // Riga 1
    c.gridx = 0 // Colonna 0
    c.gridwidth = GridBagConstraints.REMAINDER
    c.weighty = 0.0 // Non si espande verticalmente
    layout(introPlayersListLabel) = c

    nextRow()

    // playersListContainer (terza riga)
    c.gridy = row // Riga 2
    c.gridx = 0
    c.gridwidth = GridBagConstraints.REMAINDER
    c.weighty = 1.0 // Si espande per assorbir
    c.fill = GridBagPanel.Fill.Both
    // e lo spazio
    layout(playersListContainer) = c

    nextRow()

    // Reimposta i vincoli per i pulsanti
    c.gridwidth = 2 // I pulsanti occuperanno solo una colonna
    c.weighty = 0.0 // Non si espandono verticalmente
    c.fill = GridBagPanel.Fill.Horizontal // Non si allungano per riempire lo spazio
    c.anchor = GridBagPanel.Anchor.Center // Allineali al centro della loro cella

    // exitButton
    c.gridx = 0
    c.gridy = row
    layout(exitButton) = c

    // startGameButton
    c.gridx = 1
    c.gridy = row
    layout(startGameButton) = c
  }

  def updatePlayersList(newPlayers: List[PlayerInLobby]): Unit =
    println(s"Inside updatePlayerList, newPlayers = ${newPlayers}, isHost = ${isHost}")
    game = game.copy(players = newPlayers)
    startGameButton.enabled = game.players.size >= 2 && isHost
    playersListContainer.updatePlayersList(newPlayers)
    playersListContainer.revalidate()
    playersListContainer.repaint()
    repaint()

  def openErrorPubOnServerDialog(): Unit = {
    SwingUtilities.invokeLater(() => {
      Dialog.showMessage(
        this,
        "<html>Impossible public the game on Server.<br>" +
          "Other players can still reach you with your link.</html>",
        title = "Error",
        messageType = Dialog.Message.Error
      )
    })
  }

  def onExitDuringWaitingLobbyPolicy(): Unit = {
    val message = s"Are you sure you want to close lobby?"
    + s"${if isHost then "Every participant will be expelled." else ""}"
    val title = "Confirm Closing"

    val options = List("Yes, Close", "No, Stay")

    val result: Dialog.Result.Value = Dialog.showConfirmation(
      parent = this, // La finestra corrente è il genitore
      message = message,
      title = title,
      optionType = Dialog.Options.YesNo,
    )

    result match {
      case Dialog.Result.Yes =>
        println("User confirmed exit. Initiating exit procedure...")
        listener.exitFromTheGame()

      case Dialog.Result.No | Dialog.Result.Cancel | Dialog.Result.Closed =>
        println("User cancelled exit.")
    }
  }


  def hostCancelledTheGame(f: () => Unit): Unit = {
    //    Swing.onEDT{
    //      this.dispose()
    //    }
    Swing.onEDT {
      Dialog.showMessage(
        parent = this,
        message = "La partita è stata chiusa dall'Host. Verrai riportato al Menu Principale.",
        title = "Partita Chiusa",
        messageType = Dialog.Message.Warning
      )
      f()
    }
  }

class WaitingLobbyPlayersContainer(
                                    players: List[PlayerInLobby],
                                    canKickOut: Boolean
                                  ) extends ScrollPane:
  private val playerListContainer = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(10, 10, 10, 10)
    players.foreach(p => contents += new PlayerRowPanel(p, canKickOut))
  }
  contents = playerListContainer

  def updatePlayersList(newPlayers: List[PlayerInLobby]): Unit =
    playerListContainer.contents.clear()
    newPlayers.foreach(p => playerListContainer.contents += new PlayerRowPanel(p, canKickOut))

class PlayerRowPanel(
                      player: PlayerInLobby,
                      canBeKickOut: Boolean
                    ) extends BoxPanel(Orientation.Horizontal):
  private val playerNameLabel = new Label(player.name) {
    font = new Font("Arial", java.awt.Font.PLAIN, 16)
    horizontalAlignment = Alignment.Left
  }

  contents += playerNameLabel
