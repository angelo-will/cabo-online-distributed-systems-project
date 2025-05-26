package view.lobbyphase

import akka.actor.testkit.typed.scaladsl.TestProbe
import akka.actor.typed.{ActorRef, ActorSystem}
import model.{Game, GameParameters, PlayerInLobby}
import utils.Message
import view.*
import view.lobbyphase.components.{CreateGamePanel, GameListPanel, JoinGameWithLinkPanel, WaitingLobbyPanel, WelcomePanel}
import view.lobbyphase.components.*

import java.awt.{CardLayout, Color, Font}
import javax.swing.{JLabel, SwingConstants, SwingUtilities}
import scala.swing.*
import scala.swing.event.*
import scala.util.Try

trait ScreenNavigator:
  def showScreen(panelName: InitialPhaseNamesEnum): Unit

  def exitApplication(): Unit

class InitialPhaseMainFrame(viewListener: IViewListener) extends MainFrame:
  title = "Cabo Online"
  preferredSize = new Dimension(500, 400)
  centerOnScreen()
  peer.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)

  private val cardLayout = new CardLayout()
  private val cardPanelPeer = new javax.swing.JPanel(cardLayout)
  private val mainContentPanel = Component.wrap(cardPanelPeer)

  private var waitingLobbyPanelOption: Option[(Boolean, WaitingLobbyPanel)] = None

  private val screenMap: Map[InitialPhaseNamesEnum, String] = Map(
    InitialPhaseNamesEnum.WelcomePanel -> "WelcomePanelCard", // Assegna una stringa unica per CardLayout
    InitialPhaseNamesEnum.CreateGamePanel -> "CreateGamePanelCard",
    InitialPhaseNamesEnum.JoinGamePanel -> "JoinGamePanelCard",
    InitialPhaseNamesEnum.JoinGameWithLinkPanel -> "JoinGameWithLinkPanelCard"
  )

  private val screenNavigator = new ScreenNavigator {
    override def showScreen(screenName: InitialPhaseNamesEnum): Unit =
      println(s"Mostrato schermo: $screenName")
      SwingUtilities.invokeLater(() =>
        screenMap.get(screenName) match
          case Some(panelName) => cardLayout.show(cardPanelPeer, panelName)
          case None => throw new NoSuchElementException(s"Invalid screen name provided: $screenName")
      )

    override def exitApplication(): Unit =
      SwingUtilities.invokeLater(() =>
        System.exit(0)
      )
  }

  private val welcomeScreen = new WelcomePanel(screenNavigator, viewListener)

  private val createGameScreen = new CreateGamePanel(screenNavigator,
    (makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int) =>
      viewListener.createGame(makePublic, maxTimeRound, maxNumRound, maxPlayers)
      //createWaitingLobbyPanel(screenNavigator, viewListener, List(), true)
  )

  private def createWaitingLobbyPanel(screenNavigator: ScreenNavigator, viewListener: IViewListener, playersList: List[PlayerInLobby], isHost: Boolean): Unit = {
    val waitingLobbyPanel = new WaitingLobbyPanel(screenNavigator, viewListener, List.empty, isHost)
    waitingLobbyPanelOption = Some(isHost, waitingLobbyPanel)
    cardPanelPeer.add(waitingLobbyPanel.peer, screenMap(InitialPhaseNamesEnum.WaitingLobbyPanel))
    cardLayout.show(cardPanelPeer, screenMap(InitialPhaseNamesEnum.WaitingLobbyPanel))
  }

  private val joinGameScreen = new GameListPanel(screenNavigator, viewListener)
  private val joinGameWithLinkScreen = new JoinGameWithLinkPanel(screenNavigator, viewListener)


  cardPanelPeer.add(welcomeScreen.peer, screenMap(InitialPhaseNamesEnum.WelcomePanel))
  cardPanelPeer.add(createGameScreen.peer, screenMap(InitialPhaseNamesEnum.CreateGamePanel))
  cardPanelPeer.add(joinGameScreen.peer, screenMap(InitialPhaseNamesEnum.JoinGamePanel))
  cardPanelPeer.add(joinGameWithLinkScreen.peer, screenMap(InitialPhaseNamesEnum.JoinGameWithLinkPanel))

  contents = mainContentPanel

  cardLayout.show(cardPanelPeer, screenMap(InitialPhaseNamesEnum.WelcomePanel))

  // TODO: Remove this, used to emulate the arriving of data from server
  scala.concurrent.ExecutionContext.global.execute(() => {
    Thread.sleep(5000)
    joinGameScreen.updateGameList(gamesInConstruction)
  })

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
        parent = createGameScreen,
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
      joinGameScreen.updateGameList(games)
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

  ///////////////////// START PER TEST /////////////////////////////
  implicit val system: ActorSystem[Nothing] = akka.actor.typed.ActorSystem(akka.actor.typed.scaladsl.Behaviors.empty, "TestSystem")
  private val dummyProbe1 = TestProbe[Message]()
  private val dummyProbe2 = TestProbe[Message]()
  private val dummyProbe3 = TestProbe[Message]()
  private val dummyProbe4 = TestProbe[Message]()
  private val dummyProbe5 = TestProbe[Message]()
  private val dummyProbe6 = TestProbe[Message]()

  val gamesInConstruction: List[Game.GameInConstruction] = List(
    Game.GameInConstruction(
      code = "ABC123",
      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 60, roundLimitation = 5, maxPlayers = 4),
      players = List(
        PlayerInLobby("user_a", "Alice", dummyProbe1.ref),
        PlayerInLobby("user_b", "Bob", dummyProbe2.ref)
      )
    ),
    // Partita 2: Privata, 1/4 giocatori, Nessun limite di round
    Game.GameInConstruction(
      code = "XYZ456",
      gameParameters = GameParameters(makePrivate = true, maxTimeRound = 30, roundLimitation = 0, maxPlayers = 4),
      players = List(
        PlayerInLobby("user_c", "Charlie", dummyProbe3.ref)
      )
    ),
    Game.GameInConstruction(
      code = "FULL789",
      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 90, roundLimitation = 10, maxPlayers = 4),
      players = List(
        PlayerInLobby("user_d", "David", dummyProbe4.ref),
        PlayerInLobby("user_e", "Eve", dummyProbe5.ref),
        PlayerInLobby("user_f", "Frank", dummyProbe6.ref),
        PlayerInLobby("user_g", "Grace", dummyProbe1.ref) // Riutilizzo probe per semplicità
      )
    ),
    // Partita 4: Pubblica, Vuota, Nessun limite di round
    Game.GameInConstruction(
      code = "EMPTY001",
      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 45, roundLimitation = 0, maxPlayers = 3),
      players = List.empty
    ),
    Game.GameInConstruction(
      code = "MAX5005",
      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 120, roundLimitation = 15, maxPlayers = 5),
      players = List(
        PlayerInLobby("user_h", "Heidi", dummyProbe2.ref)
      )
    ),
    Game.GameInConstruction(
      code = "PRIV888",
      gameParameters = GameParameters(makePrivate = true, maxTimeRound = 60, roundLimitation = 0, maxPlayers = 4),
      players = List(
        PlayerInLobby("user_i", "Ivan", dummyProbe3.ref),
        PlayerInLobby("user_j", "Julia", dummyProbe4.ref),
        PlayerInLobby("user_k", "Kevin", dummyProbe5.ref)
      )
    ),
    Game.GameInConstruction(
      code = "SHORTGAME",
      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 20, roundLimitation = 7, maxPlayers = 3),
      players = List(
        PlayerInLobby("user_l", "Liam", dummyProbe6.ref),
        PlayerInLobby("user_m", "Mia", dummyProbe1.ref)
      )
    ),
    Game.GameInConstruction(
      code = "FULL333",
      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 75, roundLimitation = 10, maxPlayers = 3),
      players = List(
        PlayerInLobby("user_n", "Nora", dummyProbe2.ref),
        PlayerInLobby("user_o", "Oscar", dummyProbe3.ref),
        PlayerInLobby("user_p", "Pat", dummyProbe4.ref)
      )
    ),
    Game.GameInConstruction(
      code = "PRIVEMPTY",
      gameParameters = GameParameters(makePrivate = true, maxTimeRound = 40, roundLimitation = 5, maxPlayers = 2),
      players = List.empty
    ),
    Game.GameInConstruction(
      code = "LONGPLAY",
      gameParameters = GameParameters(makePrivate = false, maxTimeRound = 180, roundLimitation = 20, maxPlayers = 4),
      players = List(
        PlayerInLobby("user_q", "Quinn", dummyProbe5.ref)
      )
    )
  )

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
    override def createGame(makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit =
      println(s"Create game with these parameters: makePublic: $makePublic, maxTimeRound: $maxTimeRound, maxNumRound: $maxNumRound, maxPlayers: $maxPlayers")

    override def requestGames(): Unit = println("Requesting games from server...")

    override def joinGame(game: Game.GameInConstruction): Unit = println(s"Joining game with code: ${game.code}")

    override def joinWithAddress(address: String): Unit = println(s"JoinButton pressed to request to join game with address: $address")
  })
