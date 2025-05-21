package view

import scala.swing.*
import scala.swing.event.*
import java.awt.{Color, Font}
import scala.util.Try

trait GoToPreviousFrame(previousFrame: Frame):
  def goToPreviousFrame(): Unit =
    previousFrame.open()

class View extends SimpleSwingApplication:
  def top: Frame = new WelcomeFrame(new IViewListener {
    override def createGame(makePublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit =
      println(s"Create game with these parameters: makePublic: $makePublic, maxTimeRound: $maxTimeRound, maxNumRound: $maxNumRound, maxPlayers: $maxPlayers")

    override def joinAGame(): Unit = println(s"Join button pressed")
  })

class WelcomeFrame(viewListener: IViewListener) extends MainFrame:
  title = "Cabo Online"
  preferredSize = new Dimension(400, 300)
  centerOnScreen()

  private val thisFrameRef = this
  private val welcomeMessage = new Label("Benvenuto in Cabo Online!") {
    font = new Font("Arial", java.awt.Font.BOLD, 18)
  }
  private val chooseActionsLabel = new Label("Scegli come giocare:") {
    font = new Font("Arial", java.awt.Font.BOLD, 14)
  }

  private val createGameButton = new Button("Crea nuova partita")
  private val askToServerGameButton = new Button("Unisciti ad una partita")
  private val joinAGameWithLinkButton = new Button("Unisciti mediante link")

  contents = new BoxPanel(Orientation.Vertical):
    border = Swing.EmptyBorder(10, 10, 10, 10) // Margine interno (padding)
    contents += welcomeMessage
    contents += chooseActionsLabel
    contents += createGameButton
    contents += askToServerGameButton
    contents += joinAGameWithLinkButton

    listenTo(createGameButton, askToServerGameButton, joinAGameWithLinkButton)

    reactions += {
      case ButtonClicked(b) =>
        if b == createGameButton then
          println("Crea nuova partita")
          new CreateGameFrame(thisFrameRef, viewListener).open()
          close()
        else if b == askToServerGameButton then
          println("Unisciti ad una partita")
        else if b == joinAGameWithLinkButton then
          println("Unisciti mediante link")
    }


class CreateGameFrame(previousFrame: Frame, viewListener: IViewListener) extends MainFrame
  with GoToPreviousFrame(previousFrame):

  private val MIN_TIME_TURN_DURATION = 20
  private val MAX_TIME_TURN_DURATION = 120

  private val MAX_PLAYER_PER_GAME = 5
  private val DECK_SIZE = 52

  title = "Cabo Online - Crea nuova partita"
  preferredSize = new Dimension(400, 300)
  centerOnScreen()

  private val goToPreviousFrameButton = new Button("Indietro")

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

  private val createButton = new Button("Crea")

  contents = new BoxPanel(Orientation.Vertical):
    border = Swing.EmptyBorder(10, 10, 10, 10)
    contents += goToPreviousFrameButton

    contents += visibilityLabel
    contents += visibilityPublicRadioButton
    contents += visibilityPrivateRadioButton

    contents += maxPlayersLabel
    contents += numPlayersSelected

    contents += new FlowPanel(FlowPanel.Alignment.Left)() {
      contents += durationLabel
      contents += Swing.HStrut(10)
      contents += durationField
    }
    contents += Swing.VStrut(15)
    contents += createButton

  listenTo(goToPreviousFrameButton, durationField, createButton)

  reactions += {
    case ButtonClicked(`goToPreviousFrameButton`) =>
      println("Indietro")
      this.goToPreviousFrame()
      close()
    case EditDone(`durationField`) =>
      println(s"Text field value changed: ${durationField.text}")
      validateAndSetDuration()
    case ButtonClicked(`createButton`) =>
      println(s"Premuto bottone creazione partita")
      viewListener.createGame(
        visibilityPublicRadioButton.selected,
        currentValidTurnDuration,
        // TODO: remove this number
        DECK_SIZE - (numPlayersSelected.selection.item * 4),
        numPlayersSelected.selection.item
      )
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
        println(s"aaaaa")
        // Ripristina il testo del campo al valore valido precedente
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


@main
def testView: Unit = new View().main(Array.empty)