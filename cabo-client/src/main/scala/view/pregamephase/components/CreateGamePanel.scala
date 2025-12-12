package view.pregamephase.components

import view.pregamephase.ScreenNavigator

import java.awt.Font
import scala.swing.{Alignment, BoxPanel, Button, ButtonGroup, ComboBox, Dialog, Dimension, Label, MainFrame, Orientation, Panel, RadioButton, Swing, TextField}
import scala.swing.event.{ButtonClicked, EditDone}
import scala.util.Try

trait ICreateGameListener:
  def createGame(isPublic: Boolean, maxTimeRound: Int, maxNumRound: Int, maxPlayers: Int): Unit

class CreateGamePanel(navigator: ScreenNavigator, viewListener: ICreateGameListener) extends BoxPanel(Orientation.Vertical):
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
  private val numPlayersSelected = new ComboBox(numPlayerPossibilities){
    maximumSize = new Dimension(100, preferredSize.height)
  }
  private val maxPlayersLabel = new Label("Numero massimo di giocatori: " + numPlayersSelected.selection.item)

  private var currentValidTurnDuration: Int = 60
  private val durationLabel = new Label("Durata massima di un turno (secondi): ")
  private val durationField = new TextField(currentValidTurnDuration.toString) {
    columns = 5
    maximumSize = new Dimension(100, preferredSize.height)
    horizontalAlignment = Alignment.Center
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
        navigator.goToPreviousPanel() // Torna alla schermata precedente
    case EditDone(`durationField`) =>
      println(s"Text field value changed: ${durationField.text}")
      validateAndSetDuration()
  }

  private def validateAndSetDuration(): Unit = {
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
  }

  private def checkIfInt(s: String): Boolean = {
    Try(s.toInt) match {
      case scala.util.Success(_) => true
      case scala.util.Failure(_) =>
        Dialog.showMessage(
          parent = this,
          message = s"Errore: Inserire un numero intero valido. Hai inserito: '$s'",
          title = "Errore di Formato",
          messageType = Dialog.Message.Error
        )
        false
    }
  }

class WaitingCreationGameDialog extends Dialog {
  title = "Waiting creation"
  preferredSize = new Dimension(300, 150)
  // TODO: modal should be true, for now is false to test multiple test frame and focus on it  
  modal = false
  resizable = false
  peer.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE)
  val messageLabel = new Label("Creating the game...") {
    font = new Font("Arial", java.awt.Font.BOLD, 16)
    horizontalAlignment = Alignment.Center
  }

  contents = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(20, 20, 20, 20)
    contents += messageLabel
    contents += Swing.VGlue
  }
  centerOnScreen()
}