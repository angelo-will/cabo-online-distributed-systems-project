package view.lobbyphase

import akka.actor.typed.{ActorRef, ActorSystem}
import model.{Game, GameParameters, PlayerInLobby}
import utils.Message
import view.*
import view.lobbyphase.components.{CreateGamePanel, GameListPanel, JoinGameWithLinkPanel, WelcomePanel}
import view.lobbyphase.components.*
import view.lobbyphase.ViewListener.IInitialViewListener

import java.awt
import java.awt.event.WindowAdapter
import java.awt.{Dimension, Toolkit}
import javax.swing.SwingUtilities
import scala.swing.*

trait ScreenNavigator:
  def goToPreviousPanel(): Unit

enum DialogType:
  case WaitingCreationGame extends DialogType
  case WaitingAccessToGameFromServer extends DialogType
  case WaitingAccessToGameOfUser extends DialogType

class InitialPhaseMainFrame(val viewListener: IInitialViewListener, val playerName: String) extends MainFrame:
  val screenSize: Dimension = Toolkit.getDefaultToolkit.getScreenSize
  val screenWidth: Int = screenSize.getWidth.toInt
  val screenHeight: Int = screenSize.getHeight.toInt
  val appWidth: Int = (screenWidth * 0.5).toInt
  val appHeight: Int = (screenHeight * 0.5).toInt
  val verticalPosition: Int = (screenHeight * 0.25).toInt
  val horizontalPosition: Int = (screenWidth * 0.25).toInt
  title = "Cabo Online"
  preferredSize = new Dimension(appWidth, appHeight)
  location = new Point(horizontalPosition, verticalPosition)
  //  centerOnScreen()
  peer.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
  peer.addWindowListener(new WindowAdapter {
    override def windowClosed(e: awt.event.WindowEvent): Unit = {
      super.windowClosed(e)
      println("The InitialPhaseMainFrame has been closed.")
      dialogsMap.foreach((dialogType, dialog) => dialog.dispose())
      dialogsMap.clear()
    }
  })

  private val dialogsMap: scala.collection.mutable.Map[DialogType, Dialog] = scala.collection.mutable.Map.empty


  private val containerPanel = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(30, 30, 30, 30) // Margine interno
  }

  private val createGamePanel: CreateGamePanel = new CreateGamePanel(() => setPanel(welcomePanel),
    (makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int) => {
      SwingUtilities.invokeLater(() => {
        viewListener.createGame(makePublic, maxTimeRound, maxNumRound, maxPlayers)
        dialogsMap(DialogType.WaitingCreationGame) = new WaitingCreationGameDialog()
        dialogsMap(DialogType.WaitingCreationGame).open()
      })
    }
  )

  private val askServerGamesPanel: GameListPanel = new GameListPanel(() => setPanel(welcomePanel),
    new IListGamesListener {
      override def joinGame(game: Game.GameInConstruction): Unit = {
        viewListener.joinGame(game)
        dialogsMap(DialogType.WaitingAccessToGameOfUser) = new WaitingAccessToGameDialog()
        dialogsMap(DialogType.WaitingAccessToGameOfUser).open()
      }

      override def updateGamesList(): Unit = {
        viewListener.requestGames()
      }
    })

  private val joinGameWithLinkPanel: JoinGameWithLinkPanel = new JoinGameWithLinkPanel(() => setPanel(welcomePanel),
    (address: String) => {
      SwingUtilities.invokeLater(() => {
        viewListener.joinWithAddress(address)
        dialogsMap(DialogType.WaitingAccessToGameFromServer) = new WaitingAccessToGameDialog()
        dialogsMap(DialogType.WaitingAccessToGameFromServer).open()
      })
    })

  def setPanel(panel: Component): Unit =
    containerPanel.contents.clear()
    containerPanel.contents += panel
    containerPanel.revalidate()
    containerPanel.repaint()

  private val welcomePanel: WelcomePanel = new WelcomePanel(
    new IShowPanels {
      override def showCreateGame(): Unit = setPanel(createGamePanel)

      override def showListGamesFromServer(): Unit = {
        viewListener.requestGames()
        setPanel(askServerGamesPanel)
      }

      override def showJoinGameWithLink(): Unit = setPanel(joinGameWithLinkPanel)
    },
    playerName,
    newName => viewListener.changeName(newName)
  )
  contents = containerPanel
  setPanel(welcomePanel)

  def gameCreated(game: Game.GameInConstruction): Unit =
    SwingUtilities.invokeLater(() => {
      throw new NotImplementedError("Game creation functionality not implemented yet.")
    })

  /**
   * Displays an error message when the game creation fails.
   */
  def failedToPublishToServer(): Unit =
    SwingUtilities.invokeLater(() =>
      Dialog.showMessage(
        parent = createGamePanel,
        message = "Error: Impossible publish game on server.\n" +
          "Other players can join only with your link and not using the server.",
        title = "Error creation game",
        messageType = Dialog.Message.Error
      )
    )

  /**
   * Updates the game list joinable.
   *
   * @param games List of games in wich user can enter.
   */
  def updateGameList(games: List[Game.GameInConstruction]): Unit =
    SwingUtilities.invokeLater(() =>
      askServerGamesPanel.updateGameList(games)
    )

  def userIsEnteredInTheGame(game: Game.GameInConstruction): Unit =
    SwingUtilities.invokeLater(() => {}
      // createWaitingLobbyPanel(screenNavigator, viewListener, game.players, false)
      // TODO: create frame/panel do display that
      // throw new NotImplementedError("Game started functionality not implemented yet.")
    )

  def userFailedToEnterInTheGame(game: Game.GameInConstruction): Unit =
    SwingUtilities.invokeLater(() =>
      // TODO: AAA adjust it
      Dialog.showMessage(
        parent = this,
        message = s"Error: Impossible to enter in the game with code ${game.code}.",
        title = "Error entering game",
        messageType = Dialog.Message.Error
      )
    )

  def updateGame(game: Game.GameInConstruction): Unit =
    SwingUtilities.invokeLater(() => {
      // TODO: implement after creation of lobby panel
      throw new NotImplementedError("Game started functionality not implemented yet.")
    })

  def gameStarted(): Unit =
    SwingUtilities.invokeLater(() => {
      throw new NotImplementedError("Game started functionality not implemented yet.")
    })

object ViewApplication:
  def startView(viewListener: IInitialViewListener, afterCreation: (frame: InitialPhaseMainFrame) => Unit): Unit =
    val playerName = "playerName"
    var mainFrame: InitialPhaseMainFrame = null
    SwingUtilities.invokeLater(() =>
      mainFrame = new InitialPhaseMainFrame(viewListener, playerName)
      mainFrame.open()
      mainFrame.visible = true
      afterCreation(mainFrame)
    )


object AppMultiplePanel extends SimpleSwingApplication:
  val playerName = "playerName"

  def top: MainFrame = new InitialPhaseMainFrame(new IInitialViewListener {
    override def changeName(newName: String): Unit =
      println(s"Listener finto: Change name to $newName")

    override def createGame(isPubblic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit =
      println(s"Listener finto: Create game with these parameters: makePublic: $isPubblic, maxTimeRound: $maxTimeRound, maxNumRound: $maxNumRound, maxPlayers: $maxPlayers")

    override def requestGames(): Unit =
      println("Listener finto: Requesting games from server...")

    override def joinGame(game: Game.GameInConstruction): Unit =
      println(s"Listener finto: Joining game with code: ${game.code}")

    override def joinWithAddress(address: String): Unit =
      println(s"Listener finto: JoinButton pressed to request to join game with address: $address")

    override def startGame(): Unit =
      println("Listener finto: Start game button pressed, but no action defined in this test.")

    override def exitFromTheGame(): Unit =
      println("Listener finto: Exit from the game button pressed, but no action defined in this test.")
  },
    playerName)
