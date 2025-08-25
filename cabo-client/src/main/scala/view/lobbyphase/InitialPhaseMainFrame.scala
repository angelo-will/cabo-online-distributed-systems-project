package view.lobbyphase

import akka.actor.typed.{ActorRef, ActorSystem}
import model.{Game, GameParameters, PlayerInLobby}
import utils.Message
import view.*
import view.lobbyphase.components.{CreateGamePanel, GameListPanel, JoinGameWithLinkPanel, WelcomePanel}
import view.lobbyphase.components.*

import java.awt
import java.awt.event.WindowAdapter
import javax.swing.SwingUtilities
import scala.swing.*

trait ScreenNavigator:
  def goToPreviousPanel(): Unit

enum DialogType:
  case WaitingCreationGame extends DialogType
  case WaitingAccessToGameFromServer extends DialogType
  case WaitingAccessToGameOfUser extends DialogType

class InitialPhaseMainFrame(val viewListener: IViewListener) extends MainFrame:
  title = "Cabo Online"
  preferredSize = new Dimension(500, 400)
  centerOnScreen()
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
    }
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

//  ///////////////////// START PER TEST /////////////////////////////
//
//  //TODO: Remove this, used to emulate the arriving of data from server
//  scala.concurrent.ExecutionContext.global.execute(() => {
//    Thread.sleep(5000)
//    askServerGamesPanel.updateGameList(gamesInConstruction)
//  })
//  implicit val system: ActorSystem[Nothing] = akka.actor.typed.ActorSystem(akka.actor.typed.scaladsl.Behaviors.empty, "TestSystem")
//  private val dummyProbe1 = TestProbe[Message]()
//  private val dummyProbe2 = TestProbe[Message]()
//  private val dummyProbe3 = TestProbe[Message]()
//  private val dummyProbe4 = TestProbe[Message]()
//  private val dummyProbe5 = TestProbe[Message]()
//  private val dummyProbe6 = TestProbe[Message]()
//
//  val gamesInConstruction: List[Game.GameInConstruction] = List(
//    Game.GameInConstruction(
//      code = "ABC123",
//      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 60, roundLimitation = 5, maxPlayers = 4),
//      players = List(
//        PlayerInLobby("user_a", "Alice", dummyProbe1.ref),
//        PlayerInLobby("user_b", "Bob", dummyProbe2.ref)
//      )
//    ),
//    // Partita 2: Privata, 1/4 giocatori, Nessun limite di round
//    Game.GameInConstruction(
//      code = "XYZ456",
//      gameParameters = GameParameters(makePrivate = true, maxTimeRound = 30, roundLimitation = 0, maxPlayers = 4),
//      players = List(
//        PlayerInLobby("user_c", "Charlie", dummyProbe3.ref)
//      )
//    ),
//    Game.GameInConstruction(
//      code = "FULL789",
//      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 90, roundLimitation = 10, maxPlayers = 4),
//      players = List(
//        PlayerInLobby("user_d", "David", dummyProbe4.ref),
//        PlayerInLobby("user_e", "Eve", dummyProbe5.ref),
//        PlayerInLobby("user_f", "Frank", dummyProbe6.ref),
//        PlayerInLobby("user_g", "Grace", dummyProbe1.ref) // Riutilizzo probe per semplicità
//      )
//    ),
//    // Partita 4: Pubblica, Vuota, Nessun limite di round
//    Game.GameInConstruction(
//      code = "EMPTY001",
//      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 45, roundLimitation = 0, maxPlayers = 3),
//      players = List.empty
//    ),
//    Game.GameInConstruction(
//      code = "MAX5005",
//      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 120, roundLimitation = 15, maxPlayers = 5),
//      players = List(
//        PlayerInLobby("user_h", "Heidi", dummyProbe2.ref)
//      )
//    ),
//    Game.GameInConstruction(
//      code = "PRIV888",
//      gameParameters = GameParameters(makePrivate = true, maxTimeRound = 60, roundLimitation = 0, maxPlayers = 4),
//      players = List(
//        PlayerInLobby("user_i", "Ivan", dummyProbe3.ref),
//        PlayerInLobby("user_j", "Julia", dummyProbe4.ref),
//        PlayerInLobby("user_k", "Kevin", dummyProbe5.ref)
//      )
//    ),
//    Game.GameInConstruction(
//      code = "SHORTGAME",
//      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 20, roundLimitation = 7, maxPlayers = 3),
//      players = List(
//        PlayerInLobby("user_l", "Liam", dummyProbe6.ref),
//        PlayerInLobby("user_m", "Mia", dummyProbe1.ref)
//      )
//    ),
//    Game.GameInConstruction(
//      code = "FULL333",
//      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 75, roundLimitation = 10, maxPlayers = 3),
//      players = List(
//        PlayerInLobby("user_n", "Nora", dummyProbe2.ref),
//        PlayerInLobby("user_o", "Oscar", dummyProbe3.ref),
//        PlayerInLobby("user_p", "Pat", dummyProbe4.ref)
//      )
//    ),
//    Game.GameInConstruction(
//      code = "PRIVEMPTY",
//      gameParameters = GameParameters(makePrivate = true, maxTimeRound = 40, roundLimitation = 5, maxPlayers = 2),
//      players = List.empty
//    ),
//    Game.GameInConstruction(
//      code = "LONGPLAY",
//      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 180, roundLimitation = 20, maxPlayers = 4),
//      players = List(
//        PlayerInLobby("user_q", "Quinn", dummyProbe5.ref)
//      )
//    )
//  )

///////////////////// FINE  PER TEST /////////////////////////////

object ViewApplication:
  def startView(viewListener: IViewListener, afterCreation: (frame: InitialPhaseMainFrame) => Unit): Unit =
    var mainFrame: InitialPhaseMainFrame = null
    SwingUtilities.invokeLater(() =>
      mainFrame = new InitialPhaseMainFrame(viewListener)
      mainFrame.open()
      mainFrame.visible = true
      afterCreation(mainFrame)
    )


object AppMultiplePanel extends SimpleSwingApplication:
  def top: MainFrame = new InitialPhaseMainFrame(new IViewListener {
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
      
    override def playerCanJoinGame(player: PlayerInLobby): Unit =
      println(s"Listener finto: Player can join game: ${player.name}")
  })
