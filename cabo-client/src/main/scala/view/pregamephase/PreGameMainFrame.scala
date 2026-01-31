package view.pregamephase

import model.Game
import view.*
import view.pregamephase.components.*

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

class PreGameMainFrame(val viewListener: IPreGameViewListener, val playerName: String) extends MainFrame:
  private val CODE_FOR_WAITING_CREATION_DIALOG = "WaitingCreationDialog"
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

  //  private val dialogsMap: scala.collection.mutable.Map[DialogType, Dialog] = scala.collection.mutable.Map.empty
  private val dialogsMap: scala.collection.mutable.Map[String, Dialog] = scala.collection.mutable.Map.empty


  private val containerPanel = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(30, 30, 30, 30) // Margine interno
  }

  private val createGamePanel: CreateGamePanel = new CreateGamePanel(() => setPanel(welcomePanel),
    (makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int) => {
      SwingUtilities.invokeLater(() => {
        viewListener.createGame(makePublic, maxTimeRound, maxNumRound, maxPlayers)
        dialogsMap(CODE_FOR_WAITING_CREATION_DIALOG) = new WaitingCreationGameDialog()
        dialogsMap(CODE_FOR_WAITING_CREATION_DIALOG).open()
      })
    }
  )

  private val askServerGamesPanel: GameListPanel = new GameListPanel(() => setPanel(welcomePanel),
    new IListGamesListener {
      override def joinGame(game: Game.GameInConstruction): Unit = {
        viewListener.joinGame(game)
        dialogsMap(game.code) = new WaitingAccessToGameDialog()
        dialogsMap(game.code).open()
      }

      override def updateGamesList(): Unit = {
        viewListener.requestGames()
      }

      override def returnToStart(): Unit = viewListener.returnToStart()
    })

  private val joinGameWithLinkPanel: JoinGameWithLinkPanel = new JoinGameWithLinkPanel(
    navigator = () => setPanel(welcomePanel),
    viewListener = new IJoinGameWithLinkListener {
      override def joinWithGameCode(gameCode: String): Unit = {
        SwingUtilities.invokeLater(() => {
          viewListener.joinWithGameCode(gameCode)
          dialogsMap(gameCode) = new WaitingAccessToGameDialog()
          dialogsMap(gameCode).open()
        })
      }

      override def returnToStart(): Unit = viewListener.returnToStart()
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
    SwingUtilities.invokeLater(() => askServerGamesPanel.updateGameList(games))

  def userFailedToEnterInTheGame(gameCode: String): Unit =
    SwingUtilities.invokeLater(() =>
      dialogsMap.get(gameCode) match
        case Some(dialog) =>
          dialog.dispose()
          dialogsMap.remove(gameCode)
          Dialog.showMessage(
            parent = this,
            message = s"Error: Impossible to enter in the game with code ${gameCode}.",
            title = "Error entering game",
            messageType = Dialog.Message.Error
          )
        case None => println(s"No dialog found for game code: ${gameCode}")
    )

  def gameStarted(): Unit =
    SwingUtilities.invokeLater(() => {
      throw new NotImplementedError("Game started functionality not implemented yet.")
    })

object ViewApplication {
  def startView(
                 viewListener: IPreGameViewListener,
                 playerName: String,
                 afterCreation: (frame: PreGameMainFrame) => Unit
               ): Unit =
    //    var mainFrame: InitialPhaseMainFrame = null
    SwingUtilities.invokeLater(() =>
      println("InitialPhaseMainFrame - ViewApplication.startView - Creating main frame")
      var mainFrame = new PreGameMainFrame(viewListener, playerName)
      mainFrame.open()
      mainFrame.visible = true
      afterCreation(mainFrame)
    )
}
