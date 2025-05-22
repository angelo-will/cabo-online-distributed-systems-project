package view

import scala.swing.*
import scala.swing.event.*
import java.awt.{CardLayout, Color, Font}
import javax.swing.{JLabel, SwingConstants, SwingUtilities}
import scala.util.Try

//trait GoToPreviousFrame(previousFrame: Frame):
//  def goToPreviousFrame(): Unit =
//    previousFrame.open()
//
//class View extends SimpleSwingApplication:
//  def top: Frame = new WelcomeFrame(new IViewListener {
//    override def createGame(makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit =
//      println(s"Create game with these parameters: makePublic: $makePublic, maxTimeRound: $maxTimeRound, maxNumRound: $maxNumRound, maxPlayers: $maxPlayers")
//
//    override def joinAGame(): Unit = println(s"Join button pressed")
//  })
//
//class WelcomeFrame(viewListener: IViewListener) extends MainFrame:
//  title = "Cabo Online"
//  preferredSize = new Dimension(400, 300)
//  centerOnScreen()
//
//  private val thisFrameRef = this
//  private val welcomeMessage = new Label("Benvenuto in Cabo Online!") {
//    font = new Font("Arial", java.awt.Font.BOLD, 18)
//  }
//  private val chooseActionsLabel = new Label("Scegli come giocare:") {
//    font = new Font("Arial", java.awt.Font.BOLD, 14)
//  }
//
//  private val createGameButton = new Button("Crea nuova partita")
//  private val askToServerGameButton = new Button("Unisciti ad una partita")
//  private val joinAGameWithLinkButton = new Button("Unisciti mediante link")
//
//  contents = new BoxPanel(Orientation.Vertical):
//    border = Swing.EmptyBorder(10, 10, 10, 10) // Margine interno (padding)
//    contents += welcomeMessage
//    contents += chooseActionsLabel
//    contents += createGameButton
//    contents += askToServerGameButton
//    contents += joinAGameWithLinkButton
//
//    listenTo(createGameButton, askToServerGameButton, joinAGameWithLinkButton)
//
//    reactions += {
//      case ButtonClicked(b) =>
//        if b == createGameButton then
//          println("Crea nuova partita")
//          new CreateGameFrame(thisFrameRef, viewListener).open()
//          close()
//        else if b == askToServerGameButton then
//          println("Unisciti ad una partita")
//        else if b == joinAGameWithLinkButton then
//          println("Unisciti mediante link")
//    }
//
//
//class CreateGameFrame(previousFrame: Frame, viewListener: IViewListener) extends MainFrame
//  with GoToPreviousFrame(previousFrame):
//
//  private val MIN_TIME_TURN_DURATION = 20
//  private val MAX_TIME_TURN_DURATION = 120
//
//  private val MAX_PLAYER_PER_GAME = 5
//  private val DECK_SIZE = 52
//
//  title = "Cabo Online - Crea nuova partita"
//  preferredSize = new Dimension(400, 300)
//  centerOnScreen()
//
//  private val goToPreviousFrameButton = new Button("Indietro")
//
//  private val visibilityLabel = new Label("Visibilità:")
//  private val visibilityPublicRadioButton = new RadioButton("Pubblica")
//  private val visibilityPrivateRadioButton = new RadioButton("Privata")
//  private val visibilityGroup = new ButtonGroup(visibilityPublicRadioButton, visibilityPrivateRadioButton)
//  visibilityPrivateRadioButton.selected = true
//
//  private val numPlayerPossibilities = Seq(2, 3, 4, 5)
//  private val numPlayersSelected = new ComboBox(numPlayerPossibilities)
//  private val maxPlayersLabel = new Label("Numero massimo di giocatori: " + numPlayersSelected.selection.item)
//
//  private var currentValidTurnDuration: Int = 60
//  private val durationLabel = new Label("Durata massima di un turno (secondi): ")
//  private val durationField = new TextField(currentValidTurnDuration.toString) {
//    columns = 5
//  }
//
//  private val createButton = new Button("Crea")
//
//  contents = new BoxPanel(Orientation.Vertical):
//    border = Swing.EmptyBorder(10, 10, 10, 10)
//    contents += goToPreviousFrameButton
//
//    contents += visibilityLabel
//    contents += visibilityPublicRadioButton
//    contents += visibilityPrivateRadioButton
//
//    contents += maxPlayersLabel
//    contents += numPlayersSelected
//
//    contents += new FlowPanel(FlowPanel.Alignment.Left)() {
//      contents += durationLabel
//      contents += Swing.HStrut(10)
//      contents += durationField
//    }
//    contents += Swing.VStrut(15)
//    contents += createButton
//
//  listenTo(goToPreviousFrameButton, durationField, createButton)
//
//  reactions += {
//    case ButtonClicked(`goToPreviousFrameButton`) =>
//      println("Indietro")
//      this.goToPreviousFrame()
//      close()
//    case EditDone(`durationField`) =>
//      println(s"Text field value changed: ${durationField.text}")
//      validateAndSetDuration()
//    case ButtonClicked(`createButton`) =>
//      println(s"Premuto bottone creazione partita")
//      viewListener.createGame(
//        visibilityPublicRadioButton.selected,
//        currentValidTurnDuration,
//        // TODO: remove this number
//        DECK_SIZE - (numPlayersSelected.selection.item * 4),
//        numPlayersSelected.selection.item
//      )
//  }
//
//  private def validateAndSetDuration(): Unit =
//    val inputText = durationField.text.trim
//
//    if checkIfInt(inputText) then
//      val newDuration = inputText.toInt
//      if newDuration >= MIN_TIME_TURN_DURATION && newDuration <= MAX_TIME_TURN_DURATION then
//        currentValidTurnDuration = newDuration
//        println(s"Durata del turno impostata a: $currentValidTurnDuration secondi")
//      else
//        Dialog.showMessage(
//          parent = this,
//          message = s"Errore: Il valore deve essere compreso tra $MIN_TIME_TURN_DURATION e $MAX_TIME_TURN_DURATION secondi. Hai inserito: $newDuration",
//          title = "Errore di Input",
//          messageType = Dialog.Message.Error
//        )
//        durationField.text = currentValidTurnDuration.toString
//    else
//      durationField.text = currentValidTurnDuration.toString
//
//
//  private def checkIfInt(s: String): Boolean =
//    Try(s.toInt) match
//      case scala.util.Success(_) => true
//      case scala.util.Failure(_) =>
//        Dialog.showMessage(
//          parent = this,
//          message = s"Errore: Inserire un numero intero valido. Hai inserito: '$s'",
//          title = "Errore di Formato",
//          messageType = Dialog.Message.Error
//        )
//        false

trait ScreenNavigator:
  def showScreen(screenName: String): Unit

  def exitApplication(): Unit

class MainAppFrame extends MainFrame with ScreenNavigator:
  title = "Cabo Online"
  preferredSize = new Dimension(500, 400)
  centerOnScreen()
  peer.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)

  private val cardLayout = new CardLayout()

  private val cardPanelPeer = new javax.swing.JPanel(cardLayout)

  private val mainContentPanel = Component.wrap(cardPanelPeer)

  private val viewListener = new IViewListener {
    override def createGame(makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit =
      println(s"Create game with these parameters: makePublic: $makePublic, maxTimeRound: $maxTimeRound, maxNumRound: $maxNumRound, maxPlayers: $maxPlayers")

    override def joinAGame(): Unit = println(s"Joinbutton pressed")
  }

  private val welcomeScreen = new WelcomePanel(this, viewListener)
  private val createGameScreen = new CreateGamePanel(this, viewListener)
  // private val joinGameScreen = new JoinGamePanel(this)

  cardPanelPeer.add(welcomeScreen.peer, "welcomeScreen")
  cardPanelPeer.add(createGameScreen.peer, "createGameScreen")
  // cardPanelPeer.add(joinGameScreen.peer, "joinGameScreen")

  contents = mainContentPanel

  showScreen("welcomeScreen")

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


class CreateGamePanel(navigator: ScreenNavigator, viewListener: IViewListener) extends BoxPanel(Orientation.Vertical):
  border = Swing.EmptyBorder(30, 30, 30, 30)

  private val MIN_TIME_TURN_DURATION = 20
  private val MAX_TIME_TURN_DURATION = 120

  private val MAX_PLAYER_PER_GAME = 5
  private val DECK_SIZE = 52

  private val titleLabel = new Label("Crea Nuova Partita") {
    font = new Font("Arial", java.awt.Font.BOLD, 20)
    horizontalAlignment = Alignment.Center
  }

  private val gameNameField = new TextField("Nome della partita")

  private val visibilityLabel = new Label("Visibilità:")
  private val visibilityPublicRadioButton = new RadioButton("Pubblica")
  private val visibilityPrivateRadioButton = new RadioButton("Privata")
  private val visibilityGroup = new ButtonGroup(visibilityPublicRadioButton, visibilityPrivateRadioButton)
  visibilityPrivateRadioButton.selected = true

  private val numPlayerPossibilities = Seq(2, 3, 4, 5)
  private val numPlayersSelected = new ComboBox(numPlayerPossibilities)
  private val maxPlayersLabel = new Label("Numero massimo di giocatori: " + numPlayersSelected.selection.item)

  private var currentValidTurnDuration: Int = 60
  private val durationLabel = new Label("Durata massima di un turno (secondi): ")
  private val durationField = new TextField(currentValidTurnDuration.toString) {
    columns = 5
  }

  private val createGameButton = new Button("Crea Partita")
  private val backButton = new Button("Indietro")

  contents += titleLabel
  contents += Swing.VStrut(20)

  contents += visibilityLabel
  contents += visibilityPublicRadioButton
  contents += visibilityPrivateRadioButton

  contents += Swing.VStrut(20)

  contents += maxPlayersLabel
  contents += numPlayersSelected

  contents += Swing.VStrut(10)

  contents += durationLabel
  contents += durationField

  contents += Swing.VStrut(10)
  contents += backButton
  contents += createGameButton
  contents += Swing.VGlue

  listenTo(createGameButton, backButton, durationField)

  reactions += {
    case ButtonClicked(b) =>
      if b == createGameButton then
        println(s"Premuto bottone creazione partita")
        viewListener.createGame(
          visibilityPublicRadioButton.selected,
          currentValidTurnDuration,
          // TODO: remove this number
          DECK_SIZE - (numPlayersSelected.selection.item * 4),
          numPlayersSelected.selection.item
        )
      else if b == backButton then
        println("CreateGamePanel: Cliccato 'Indietro'. Chiedo al navigatore di mostrare 'welcomeScreen'.")
        navigator.showScreen("welcomeScreen") // Torna alla schermata precedente
    case EditDone(`durationField`) =>
      println(s"Text field value changed: ${durationField.text}")
      validateAndSetDuration()
  }

  private def validateAndSetDuration(): Unit =
    val inputText = durationField.text.trim

    if checkIfInt(inputText) then
      val newDuration = inputText.toInt
      if newDuration >= MIN_TIME_TURN_DURATION && newDuration <= MAX_TIME_TURN_DURATION then
        currentValidTurnDuration = newDuration
        println(s"Durata del turno impostata a: $currentValidTurnDuration secondi")
      else
        Dialog.showMessage(
          parent = this,
          message = s"Errore: Il valore deve essere compreso tra $MIN_TIME_TURN_DURATION e $MAX_TIME_TURN_DURATION secondi. Hai inserito: $newDuration",
          title = "Errore di Input",
          messageType = Dialog.Message.Error
        )
        durationField.text = currentValidTurnDuration.toString
    else
      durationField.text = currentValidTurnDuration.toString

  private def checkIfInt(s: String): Boolean =
    Try(s.toInt) match
      case scala.util.Success(_) => true
      case scala.util.Failure(_) =>
        Dialog.showMessage(
          parent = this,
          message = s"Errore: Inserire un numero intero valido. Hai inserito: '$s'",
          title = "Errore di Formato",
          messageType = Dialog.Message.Error
        )
        false

class WelcomePanel(navigator: ScreenNavigator, viewListener: IViewListener) extends BoxPanel(Orientation.Vertical):
  border = Swing.EmptyBorder(30, 30, 30, 30) // Margine interno

  private val welcomeMessage = new Label("Benvenuto in Cabo Online!") {
    font = new Font("Arial", java.awt.Font.BOLD, 22)
    horizontalAlignment = Alignment.Center
  }
  private val chooseActionsLabel = new Label("Scegli come giocare:") {
    font = new Font("Arial", java.awt.Font.BOLD, 16)
    horizontalAlignment = Alignment.Center
  }

  private val createGameButton = new Button("Crea nuova partita")
  private val askToServerGameButton = new Button("Unisciti ad una partita")
  private val joinAGameWithLinkButton = new Button("Unisciti mediante link")

  contents += welcomeMessage
  contents += Swing.VStrut(20) // Spazio verticale
  contents += chooseActionsLabel
  contents += Swing.VStrut(30)
  contents += createGameButton
  contents += Swing.VStrut(15)
  contents += askToServerGameButton
  contents += Swing.VStrut(15)
  contents += joinAGameWithLinkButton
  contents += Swing.VGlue

  listenTo(createGameButton, askToServerGameButton, joinAGameWithLinkButton)

  reactions += {
    case ButtonClicked(b) =>
      if b == createGameButton then
        println("WelcomePanel: Cliccato 'Crea nuova partita'. Chiedo al navigatore di mostrare 'createGameScreen'.")
        navigator.showScreen("createGameScreen")
      else if b == askToServerGameButton then
        println("WelcomePanel: Cliccato 'Unisciti ad una partita'.")
      // navigator.showScreen("joinGameScreen")
      else if b == joinAGameWithLinkButton then
        println("WelcomePanel: Cliccato 'Unisciti mediante link'.")
    // navigator.showScreen("joinGameByLinkScreen")
  }


//@main
//def testView: Unit = new View().main(Array.empty)

object AppMultiplePanel extends SimpleSwingApplication:
  def top: MainFrame = new MainAppFrame()
