package view.ui

import akka.actor.testkit.typed.scaladsl.TestProbe
import akka.actor.typed.{ActorRef, ActorSystem}
import model.{Game, GameParameters, PlayerInLobby}
import utils.Message
import view.*
import view.ui.components.{CreateGamePanel, GameListPanel, JoinGameWithLinkPanel, WelcomePanel}

import java.awt.{CardLayout, Color, Font}
import javax.swing.{JLabel, SwingConstants, SwingUtilities}
import scala.swing.*
import scala.swing.event.*
import scala.util.Try

trait ScreenNavigator:
  def showScreen(screenName: String): Unit

  def exitApplication(): Unit
  
class InitialPhaseMainFrame extends MainFrame with ScreenNavigator:
  title = "Cabo Online"
  preferredSize = new Dimension(500, 400)
  centerOnScreen()
  peer.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)

  private val viewListener = new IViewListener {
    override def createGame(makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit =
      println(s"Create game with these parameters: makePublic: $makePublic, maxTimeRound: $maxTimeRound, maxNumRound: $maxNumRound, maxPlayers: $maxPlayers")

    override def joinAGame(address: String): Unit = println(s"JoinButton pressed to request to join game with address: $address")
  }

  private val cardLayout = new CardLayout()
  private val cardPanelPeer = new javax.swing.JPanel(cardLayout)
  private val mainContentPanel = Component.wrap(cardPanelPeer)


  private val welcomeScreen = new WelcomePanel(this, viewListener)
  private val createGameScreen = new CreateGamePanel(this, viewListener)
  private val joinGameScreen = new GameListPanel(this, viewListener)
  private val joinGameWithLinkScreen = new JoinGameWithLinkPanel(this, viewListener)

  cardPanelPeer.add(welcomeScreen.peer, InitialPhaseNamesEnum.WelcomePanel.name)
  cardPanelPeer.add(createGameScreen.peer, InitialPhaseNamesEnum.CreateGamePanel.name)
  cardPanelPeer.add(joinGameScreen.peer, InitialPhaseNamesEnum.JoinGamePanel.name)
  cardPanelPeer.add(joinGameWithLinkScreen.peer, InitialPhaseNamesEnum.JoinGameWithLinkPanel.name)

  contents = mainContentPanel

  showScreen(InitialPhaseNamesEnum.WelcomePanel.name)

  // TODO: Remove this, used to emulate the arriving of data from server
  scala.concurrent.ExecutionContext.global.execute(() => {
    Thread.sleep(10000)
    joinGameScreen.updateGameList(gamesInConstruction)
  })

  override def showScreen(screenName: String): Unit = {
    SwingUtilities.invokeLater(() => {
      cardLayout.show(cardPanelPeer, screenName)
      println(s"Mostrato schermo: $screenName")
    })
  }

  override def exitApplication(): Unit = {
    SwingUtilities.invokeLater(() => {
      System.exit(0)
    })
  }
  
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

object AppMultiplePanel extends SimpleSwingApplication:
  def top: MainFrame = new InitialPhaseMainFrame()
