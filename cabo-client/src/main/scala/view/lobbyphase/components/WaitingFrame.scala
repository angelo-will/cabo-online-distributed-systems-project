package view.lobbyphase.components

import model.Game.GameInConstruction
import model.{Game, PlayerInLobby}
import view.lobbyphase.ScreenNavigator
import view.lobbyphase.ViewListener.IInitialViewListener

import java.awt.GridBagConstraints
import java.awt.GridBagConstraints.*
import java.awt.Insets
import java.awt.Font
import javax.swing.SwingUtilities
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
                  ) extends MainFrame:
  title = "Waiting Lobby"
  preferredSize = new Dimension(600, 400)
  centerOnScreen()
  peer.setDefaultCloseOperation(
    //TODO: Insert message to actor view to reopen first frame
    javax.swing.WindowConstants.HIDE_ON_CLOSE
  )
  
  private val playersListContainer = new WaitingLobbyPlayersContainer(
    game.players,
    isHost
  )

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

  listenTo(startGameButton, exitButton)

  reactions += {
    case ButtonClicked(`startGameButton`) =>
      println("Start Game button clicked.")
      listener.startGame()
    case ButtonClicked(`exitButton`) =>
      println("Exit button clicked.")
      // TODO: implementare in actor view l'invio del messaggio di uscita
      listener.exitFromTheGame()
      // navigator.goToPreviousPanel()
      this.dispose()
  }

  contents = new GridBagPanel {
    border = Swing.EmptyBorder(30, 30, 30, 30)

    // Definisci le componenti
    private val waitingMessage = new Label("Waiting host to start the game.") {
      font = new Font("Arial", java.awt.Font.BOLD, 22)
      horizontalAlignment = Alignment.Center
    }
    private val gameProperties = new Label("<html>" +
      "<p>Game Properties</p>" +
      "<br> Max players per game: " + game.gameParameters.maxPlayers +
      "<br> Max time per round: " + game.gameParameters.maxTimeRound +
      "<br> Max rounds per game: " + game.gameParameters.roundLimitation +
      "</html>")
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

  def openErrorPubOnServerDialog(): Unit =
    SwingUtilities.invokeLater(() => {
      Dialog.showMessage(
        this,
        "<html>Impossible public the game on Server.<br>" +
          "Other players can still reach you with your link.</html>",
        title = "Error",
        messageType = Dialog.Message.Error
      )
    })

class WaitingLobbyPlayersContainer(
                                    //                                    listener: IInitialViewListener,
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
