package view.pregamephase.components

import model.Game.GameInConstruction
import model.{Game, PlayerInLobby}

import java.awt.{Font, GridBagConstraints, Toolkit}
import java.awt.GridBagConstraints.*
import java.awt.datatransfer.StringSelection
import javax.swing.SwingUtilities
import scala.swing.GridBagPanel.Fill
import scala.swing.event.ButtonClicked
import scala.swing._

trait IWaitingToStartListener:
  def startGame(): Unit

  def exitFromTheGame(): Unit

class LobbyWaitingFrame(
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

  reactions += {
    case ButtonClicked(`startGameButton`) =>
      listener.startGame()
      this.dispose()
    case ButtonClicked(`exitButton`) =>
      onExitDuringWaitingLobbyPolicy()
    case ButtonClicked(`copyGameCodeButton`) =>
      val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
      val selection = new StringSelection(game.code)
      clipboard.setContents(selection, selection)
  }

  contents = new GridBagPanel {
    border = Swing.EmptyBorder(30, 30, 30, 30)

    private val waitingMessage = new Label("Waiting host to start the game.") {
      font = new Font("Arial", java.awt.Font.BOLD, 22)
      horizontalAlignment = Alignment.Center
    }

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

    val c = new Constraints
    private var row: Int = 0
    private var column: Int = 0

    private def resetColumn(): Unit = column = 0

    private def nextRow(): Unit =
      row += 1
      resetColumn()

    c.fill = GridBagPanel.Fill.Horizontal

    c.gridy = row
    c.gridx = column
    c.gridwidth = GridBagConstraints.REMAINDER
    c.weighty = 1.0
    layout(waitingMessage) = c

    nextRow()

    c.gridy = row
    c.gridx = column
    c.gridwidth = GridBagConstraints.REMAINDER
    c.weighty = 0.0
    layout(gameProperties) = c

    nextRow()

    c.gridy = row
    c.gridx = column
    c.fill = Fill.Horizontal
    layout(copyGameCodeButton) = c
    c.fill = Fill.None

    nextRow()

    c.gridy = row
    c.gridx = 0
    c.gridwidth = GridBagConstraints.REMAINDER
    c.weighty = 0.0
    layout(introPlayersListLabel) = c

    nextRow()

    c.gridy = row
    c.gridx = 0
    c.gridwidth = GridBagConstraints.REMAINDER
    c.weighty = 1.0
    c.fill = GridBagPanel.Fill.Both
    layout(playersListContainer) = c

    nextRow()

    c.gridwidth = 2
    c.weighty = 0.0
    c.fill = GridBagPanel.Fill.Horizontal
    c.anchor = GridBagPanel.Anchor.Center

    c.gridx = 0
    c.gridy = row
    layout(exitButton) = c

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

  private def onExitDuringWaitingLobbyPolicy(): Unit = {
    val message = s"Are you sure you want to close lobby?"
    + s"${if isHost then " Every participant will be expelled." else ""}"
    val title = "Confirm Closing"

    val options = List("Yes, Close", "No, Stay")

    val result: Dialog.Result.Value = Dialog.showConfirmation(
      parent = this,
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


  def hostCancelledTheGame(onClose: () => Unit): Unit = {
    Swing.onEDT {
      Dialog.showMessage(
        parent = this,
        message = "Game was closed by the host. You'll bring back to main menu.",
        title = "Game Closed",
        messageType = Dialog.Message.Warning
      )
      onClose()
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
