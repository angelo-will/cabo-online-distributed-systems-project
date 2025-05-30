package view.lobbyphase.components

import model.{Game, PlayerInLobby}
import view.lobbyphase.IViewListener

import javax.swing.SwingUtilities
import scala.swing.event.ButtonClicked
import scala.swing.{Alignment, BoxPanel, Button, Dialog, Dimension, Font, Label, MainFrame, Orientation, ScrollPane, Swing}

class WaitingFrame(
                    listener: IViewListener,
                    private var players: List[PlayerInLobby],
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
    listener,
    players,
    isHost
  )

  private val startGameButton = new Button("Start Game") {
    font = new Font("Arial", java.awt.Font.PLAIN, 16)
    enabled = players.size >= 2 && isHost
    horizontalAlignment = Alignment.Center
  }
  
  listenTo(startGameButton)

  reactions += {
    case ButtonClicked(`startGameButton`) =>
      println("Start Game button clicked.")
      listener.startGame()
  }

  contents = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(30, 30, 30, 30)
    private val waitingMessage = new Label("Waiting host to start the game.") {
      font = new Font("Arial", java.awt.Font.BOLD, 22)
      horizontalAlignment = Alignment.Center
    }
    private val introPlayersListLabel = new Label("Players in the game:") {
      font = new Font("Arial", java.awt.Font.BOLD, 16)
      horizontalAlignment = Alignment.Center
    }

    contents += waitingMessage
    contents += Swing.VGlue
    contents += introPlayersListLabel
    contents += Swing.VGlue
    contents += playersListContainer
    contents += Swing.VGlue
    contents += startGameButton
  }

  def updatePlayersList(newPlayers: List[PlayerInLobby]): Unit =
    println(s"Inside updatePlayerList, newPlayers = ${newPlayers}, isHost = ${isHost}")
    players = newPlayers
    startGameButton.enabled = players.size >= 2 && isHost
    playersListContainer.updatePlayersList(newPlayers)
    playersListContainer.revalidate()
    playersListContainer.repaint()
    repaint()

  def playerHasRequestedToJoinTheGame(player: PlayerInLobby): Unit = {
    SwingUtilities.invokeLater(() => {
      if Dialog.showConfirmation(
        this,
        s"Player ${player.name} has requested to join the game. Do you accept?",
        title = "Join Request",
        optionType = Dialog.Options.YesNo,
        Dialog.Message.Question
      ) == Dialog.Result.Yes then
        listener.playerCanJoinGame(player)
        this.updatePlayersList(players :+ player)
    })
  }

class WaitingLobbyPlayersContainer(
                                    listener: IViewListener,
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

@main def testWaitingLobbyPanel(): Unit =
  val dummyListener = new IViewListener {
    override def createGame(isPubblic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit =
      println("DummyListener: createGame chiamato (non fa nulla in questo test)")

    override def requestGames(): Unit =
      println("DummyListener: requestGames chiamato (non fa nulla in questo test)")

    override def joinGame(game: Game.GameInConstruction): Unit =
      println("DummyListener: joinGame chiamato (non fa nulla in questo test)")

    override def joinWithAddress(address: String): Unit =
      println("DummyListener: joinWithAddress chiamato (non fa nulla in questo test)")

    override def startGame(): Unit =
      println("DummyListener: startGame chiamato (non fa nulla in questo test)")

    override def playerCanJoinGame(player: PlayerInLobby): Unit =
      println(s"DummyListener: playerCanJoinGame chiamato per il giocatore ${player.name} (non fa nulla in questo test)")
  }

  val dummyPlayers = List(
    PlayerInLobby("id1", "Alice", null),
    PlayerInLobby("id2", "Bob", null),
    PlayerInLobby("id3", "Charlie", null),
    PlayerInLobby("id4", "David", null),
    PlayerInLobby("id5", "Eve", null)
  )

  val waitingLobbyPanel = new WaitingFrame(dummyListener, dummyPlayers, true)
  //  val waitingLobbyPanel = new WaitingFrame(dummyPlayers, true)
  waitingLobbyPanel.visible = true

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
    Thread.sleep(3000)
    waitingLobbyPanel.updatePlayersList(updatedPlayers)
    println("Lista giocatori aggiornata dopo 10 secondi!")

  })