package view.ui.components

import model.{Game, PlayerInLobby}
import view.IViewListener
import view.ui.{InitialPhaseNamesEnum, ScreenNavigator}

import scala.swing.{Alignment, BoxPanel, Button, Dimension, Font, Label, MainFrame, Orientation, ScrollPane, Swing}

class WaitingLobbyPanel(navigator: ScreenNavigator, listener: IViewListener, players: List[PlayerInLobby]) extends BoxPanel(Orientation.Vertical):
  border = Swing.EmptyBorder(30, 30, 30, 30) // Margine interno

  private val waitingMessage = new Label("Waiting host to start the game.") {
    font = new Font("Arial", java.awt.Font.BOLD, 22)
    horizontalAlignment = Alignment.Center
  }

  private val introPlayersListLabel = new Label("Players in the game:") {
    font = new Font("Arial", java.awt.Font.BOLD, 16)
    horizontalAlignment = Alignment.Center
  }

  val playersListContainer = new WaitingLobbyPlayersContainer(listener, players)

  contents += waitingMessage
  contents += Swing.VGlue
  contents += introPlayersListLabel
  contents += Swing.VGlue
  contents += playersListContainer

  def updatePlayersList(newPlayers: List[PlayerInLobby]): Unit =
    playersListContainer.updatePlayersList(newPlayers)
    revalidate()
    repaint()


// Aggiungi eventuali altri componenti o logica se necessario
class WaitingLobbyPlayersContainer(listener: IViewListener, players: List[PlayerInLobby]) extends ScrollPane:
  private val playerListContainer = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(10, 10, 10, 10)
    players.foreach(p => contents += new PlayerRowPanel(p))
  }
  contents = playerListContainer

  def updatePlayersList(newPlayers: List[PlayerInLobby]): Unit =
    playerListContainer.contents.clear()
    newPlayers.foreach(p => playerListContainer.contents += new PlayerRowPanel(p))

class PlayerRowPanel(player: PlayerInLobby) extends BoxPanel(Orientation.Horizontal):
  private val playerNameLabel = new Label(player.name) {
    font = new Font("Arial", java.awt.Font.PLAIN, 16)
    horizontalAlignment = Alignment.Left
  }

  private val kickOutButton = new Button("Kick Out") {
    font = new Font("Arial", java.awt.Font.PLAIN, 16)
    // TODO: create something enabled = player.isHost
    horizontalAlignment = Alignment.Right
  }

  contents += playerNameLabel
  contents += Swing.HGlue
  contents += kickOutButton

@main def testWaitingLobbyPanel(): Unit =
  val dummyListener = new IViewListener {
    override def createGame(makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit =
      println("DummyListener: createGame chiamato (non fa nulla in questo test)")

    override def requestGames(): Unit =
      println("DummyListener: requestGames chiamato (non fa nulla in questo test)")

    override def joinGame(game: Game.GameInConstruction): Unit =
      println("DummyListener: joinGame chiamato (non fa nulla in questo test)")

    override def joinWithAddress(address: String): Unit =
      println("DummyListener: joinWithAddress chiamato (non fa nulla in questo test)")
  }

  val dummyNavigator = new ScreenNavigator {

    override def showScreen(panelName: InitialPhaseNamesEnum): Unit = ???

    override def exitApplication(): Unit = ???
  }

  val dummyPlayers = List(
    PlayerInLobby("id1", "Alice", null),
    PlayerInLobby("id2", "Bob", null),
    PlayerInLobby("id3", "Charlie", null),
    PlayerInLobby("id4", "David", null),
    PlayerInLobby("id5", "Eve", null)
  )

  val waitingLobbyPanel = new WaitingLobbyPanel(dummyNavigator, dummyListener, dummyPlayers)

  val mainFrame: MainFrame = new MainFrame {
    title = "Waiting Lobby Panel Test (No Akka)"
    contents = waitingLobbyPanel
    size = new Dimension(600, 400)
    centerOnScreen()
    visible = true

    val updatedPlayers: List[PlayerInLobby] = List(
      PlayerInLobby("id1", "Alice", null),
      PlayerInLobby("id2", "Bob", null),
      PlayerInLobby("id6", "Frank", null),
      PlayerInLobby("id7", "Grace", null),
      PlayerInLobby("id8", "Heidi", null),
      PlayerInLobby("id9", "Ivan", null),
      PlayerInLobby("id10", "Judy", null)
    )

    scala.concurrent.ExecutionContext.global.execute(() => {
      Thread.sleep(5000)
      waitingLobbyPanel.updatePlayersList(updatedPlayers)
      println("Lista giocatori aggiornata dopo 10 secondi!")
    })
  }
