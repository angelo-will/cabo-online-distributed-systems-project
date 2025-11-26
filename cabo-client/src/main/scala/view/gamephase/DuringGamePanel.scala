package view.gamephase

import model.Game.GameInProgress as GProg
import model.TurnEvent.CaboCalled
import model.{Card, PlayerPlaying, Power, TurnLog}
import view.lobbyphase.ViewListener.IDuringGameViewListener

import scala.swing.*
import scala.swing.event.*
import scala.swing.GridBagPanel.Fill
import java.awt.{Color, GridBagLayout, Font as AwtFont}
import javax.swing.{BorderFactory, SwingUtilities, UIManager}

class DuringGamePanel(viewListener: IDuringGameViewListener, gameInProgress: GProg, userID: String) extends GridBagPanel with IDuringGameInterface {

  // POSITIONIG ELEMENTS - START
  private val GAME_INFORMATION_PANEL_ROW = 1
  private val GAME_INFORMATION_PANEL_COLUMN = 3
  private val GAME_INFORMATION_PANEL_COLUMNS_QUANTITY = 3

  private val EMPTY_ROWS_UNDER_GAME_INFORMATION_PANEL = GAME_INFORMATION_PANEL_ROW + 1
  private val EMPTY_ROWS_UNDER_GAME_INFORMATION_PANEL_ROWS_QUANTITY = 1

  private val EMPTY_COLUMNS_AFTER_GAME_INFORMATION =
    GAME_INFORMATION_PANEL_COLUMN + GAME_INFORMATION_PANEL_COLUMNS_QUANTITY

  private val ADVERSARIES_PANEL_COLUMN = 1
  private val ADVERSARIES_PANEL_START_ROW =
    EMPTY_ROWS_UNDER_GAME_INFORMATION_PANEL + EMPTY_ROWS_UNDER_GAME_INFORMATION_PANEL_ROWS_QUANTITY + 1

  private val ADVERSARIES_PANEL_ROWS_GAP = 1

  private val DECK_PANEL_ROW = ADVERSARIES_PANEL_START_ROW
  private val DECK_PANEL_COLUMN = GAME_INFORMATION_PANEL_COLUMN

  private val DISCARD_PANEL_ROW = DECK_PANEL_ROW
  private val DISCARD_PANEL_COLUMN =
    GAME_INFORMATION_PANEL_COLUMN + GAME_INFORMATION_PANEL_COLUMN - 1

  private val DRAWN_CARD_PANEL_ROW = DECK_PANEL_ROW + 2
  private val DRAWN_CARD_PANEL_COLUMN = DECK_PANEL_COLUMN + 1

  private val EMPTY_ROW_UNDER_DRAWN_PANEL = DRAWN_CARD_PANEL_ROW + 1
  private val EMPTY_ROW_UNDER_DRAWN_PANEL_ROWS_QUANTITY = 1

  private val WHO_CALLED_CABO_ROW = EMPTY_ROW_UNDER_DRAWN_PANEL + EMPTY_ROW_UNDER_DRAWN_PANEL_ROWS_QUANTITY + 1

  private val MY_TURN_LOG_ROW = WHO_CALLED_CABO_ROW + 1
  private val MY_TURN_LOG_COLUMN = GAME_INFORMATION_PANEL_COLUMN
  private val MY_TURN_LOG_ROWS_QUANTITY = 2
  private val MY_TURN_LOG_COLUMNS_QUANTITY = GAME_INFORMATION_PANEL_COLUMNS_QUANTITY

  private val PLAYER_PANEL_ROW = MY_TURN_LOG_ROW + MY_TURN_LOG_ROWS_QUANTITY + 1
  private val PLAYER_PANEL_COLUMN = GAME_INFORMATION_PANEL_COLUMN
  private val PLAYER_PANEL_ROWS_QUANTITY = 1
  private val PLAYER_PANEL_COLUMNS_QUANTITY = GAME_INFORMATION_PANEL_COLUMNS_QUANTITY

  private val TIMER_PANEL_ROW = GAME_INFORMATION_PANEL_ROW
  private val TIMER_PANEL_COLUMN = EMPTY_COLUMNS_AFTER_GAME_INFORMATION + 1

  private val LOG_PANEL_ROW = ADVERSARIES_PANEL_START_ROW
  private val LOG_PANEL_COLUMN = EMPTY_COLUMNS_AFTER_GAME_INFORMATION + 1
  private val LOG_PANEL_ROWS_QUANTITY = 3

  private val DISCARD_DRAWN_CARD_BUTTON_ROW = LOG_PANEL_ROW + LOG_PANEL_ROWS_QUANTITY
  private val DISCARD_DRAWN_CARD_BUTTON_COLUMN = LOG_PANEL_COLUMN
  private val DISCARD_DRAWN_CARD_BUTTON_FILL = Fill.Horizontal

  private val END_TURN_BUTTON_ROW = DISCARD_DRAWN_CARD_BUTTON_ROW + 1
  private val END_TURN_BUTTON_COLUMN = LOG_PANEL_COLUMN
  private val END_TURN_BUTTON_FILL = Fill.Horizontal

  private val CALL_CABO_BUTTON_ROW = END_TURN_BUTTON_ROW + 1
  private val CALL_CABO_BUTTON_COLUMN = LOG_PANEL_COLUMN
  private val CALL_CABO_BUTTON_FILL = Fill.Horizontal

  private val EXIT_BUTTON_ROW = CALL_CABO_BUTTON_ROW + 1
  private val EXIT_BUTTON_COLUMN = LOG_PANEL_COLUMN
  private val EXIT_BUTTON_FILL = Fill.Horizontal
  // POSITIONIG ELEMENTS - END

  peer.setBorder(BorderFactory.createLineBorder(Color.CYAN, 3))

  private var caboHasCalled = false

  val c = new Constraints

  val adversariesPanelMap: Map[String, PlayerPanel] = gameInProgress.players
    .filter(p => p.userID != userID)
    .map(p => p.userID -> new PlayerPanel(p.name, index => viewListener.adversaryCardSelected(p.userID, index))).toMap

  val playerPanel = new PlayerPanel("YOU", n => viewListener.ownCardSelected(n))
  playerPanel.enableCardsButton(true)

  private def spacePanel = new Panel {
    preferredSize = new Dimension(this.preferredSize.width, 1)
    peer.setBorder(BorderFactory.createLineBorder(Color.GREEN, 3))
  }

  c.gridx = 0
  c.gridy = 0
  c.gridwidth = 9
  c.weightx = 1.0
  c.fill = Fill.Horizontal
  layout(spacePanel) = c
  resetConstraintsValues()

  // ADVERSARIES PANELS - START
  private var rowAdversaryIndex = ADVERSARIES_PANEL_START_ROW

  adversariesPanelMap.foreach { (id, panel) =>
    c.gridy = rowAdversaryIndex
    c.gridx = ADVERSARIES_PANEL_COLUMN
    layout(panel) = c
    val emptyRow = rowAdversaryIndex + ADVERSARIES_PANEL_ROWS_GAP
    addEmptyRow(emptyRow, 50)
    rowAdversaryIndex = emptyRow + 1
  }

  // ADVERSARIES PANELS - END

  // COLONNA VUOTA DI RIEMPIMENTO
  addEmptyColumn(2, 50)
  ////////////////////

  // CREAZIONE SCHERMATA DATI PARTITA E TURNO - INIZIO
  private val gameInfoPanel = new GameInfoPanel(gameInProgress)

  c.gridx = GAME_INFORMATION_PANEL_COLUMN
  c.gridy = GAME_INFORMATION_PANEL_ROW
  c.gridwidth = GAME_INFORMATION_PANEL_COLUMNS_QUANTITY
  c.fill = Fill.Both
  layout(gameInfoPanel) = c
  resetConstraintsValues()
  // CREAZIONE SCHERMATA DATI PARTITA E TURNO - FINE

  // CREAZIONE MAZZO PRINCIPALE - INIZIO
  val deckPanel = new DeckPanel("Deck", "Deck", () => {
    viewListener.drawFromDeck()
  })
  c.gridx = DECK_PANEL_COLUMN
  c.gridy = DECK_PANEL_ROW
  layout(deckPanel) = c
  resetConstraintsValues()
  // CREAZIONE MAZZO PRINCIPALE - FINE

  // CREAZIONE MAZZO SCARTI - INIZIO
  val discardPanel = new DeckPanel(
    "Discard",
    gameInProgress.discardDeckStack.cards.head.toString,
    viewListener.drawFromDiscard
  )
  c.gridx = DISCARD_PANEL_COLUMN
  c.gridy = DISCARD_PANEL_ROW
  layout(discardPanel) = c
  resetConstraintsValues()
  // CREAZIONE MAZZO SCARTI - FINE

  // CREAZIONE CARTA PESCATA - INIZIO
  private val drawnCardButton = new Button("Nascosta") {
    font = new AwtFont("Arial", AwtFont.PLAIN, 24)
    border = Swing.EmptyBorder(5, 5, 5, 5)
  }

  private val drawnCardPanel = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(10, 10, 10, 10)
    private val drawnCardLabel = new Label("Drawn Card") {
      font = new AwtFont("Arial", AwtFont.BOLD, 14)
      horizontalAlignment = Alignment.Center
    }
    contents += drawnCardLabel
    contents += Swing.VStrut(5)
    contents += drawnCardButton
  }
  c.gridx = DRAWN_CARD_PANEL_COLUMN
  c.gridy = DRAWN_CARD_PANEL_ROW
  layout(drawnCardPanel) = c
  resetConstraintsValues()
  // CREAZIONE CARTA PESCATA - FINE

  // WHO CALLED CABO PANEL - START
  private val whoCalledCaboLabel = new Label("Nobody has called Cabo.") {
    font = new AwtFont("Arial", AwtFont.BOLD, 12)
    horizontalAlignment = Alignment.Center
  }
  c.gridy = WHO_CALLED_CABO_ROW
  c.gridx = GAME_INFORMATION_PANEL_COLUMN
  c.gridwidth = GAME_INFORMATION_PANEL_COLUMNS_QUANTITY
  c.fill = Fill.Horizontal
  layout(whoCalledCaboLabel) = c
  resetConstraintsValues()
  // WHO CALLED CABO PANEL - END


  // PLAYER ACTIONS LOG PANEL - START
  private val myTurnActionsLog = new TextArea {
    editable = false
    lineWrap = true
    wordWrap = true
    font = new AwtFont("Arial", AwtFont.PLAIN, 12)
    text = "AZIONI COMPIUTE NEL TURNO:"
  }
  private val myTurnScrollPane = new ScrollPane(myTurnActionsLog) {
    preferredSize = new Dimension(this.preferredSize.width, 100)
    verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
    //    preferredSize = new Dimension(1, 100)
    peer.setBorder(BorderFactory.createLineBorder(Color.MAGENTA, 3))
  }

  c.gridx = MY_TURN_LOG_COLUMN
  c.gridy = MY_TURN_LOG_ROW
  c.gridwidth = MY_TURN_LOG_COLUMNS_QUANTITY
  c.gridheight = MY_TURN_LOG_ROWS_QUANTITY
  c.fill = Fill.Both
  layout(myTurnScrollPane) = c
  resetConstraintsValues()
  // PLAYER ACTIONS LOG PANEL - END

  // PLAYER PANEL - START
  c.gridx = PLAYER_PANEL_COLUMN
  c.gridy = PLAYER_PANEL_ROW
  c.gridwidth = PLAYER_PANEL_COLUMNS_QUANTITY
  c.fill = Fill.Both
  layout(playerPanel) = c
  resetConstraintsValues()
  // PLAYER PANEL - END

  // EMPTY COLUMN
  addEmptyColumn(6, 50)

  // TIMER PANEL - START
  val timerPanel = new TimerPanel(
    gameInProgress.gameParameters.maxTimeRound,
    () => {
      this.disableButExit()
      this.drawnCardButton.text = "Empty"
    })
  c.gridy = TIMER_PANEL_ROW
  c.gridx = TIMER_PANEL_COLUMN
  layout(timerPanel) = c
  // TIMER PANEL - END

  // TEXT AREA LOG - END
  private val logPanel = new LogPanel()

  c.gridy = LOG_PANEL_ROW
  c.gridx = LOG_PANEL_COLUMN
  c.gridheight = LOG_PANEL_ROWS_QUANTITY
  c.fill = Fill.Both
  layout(logPanel) = c

  resetConstraintsValues()
  // TEXT AREA LOG - END

  // END TURN BUTTON - START
  val endTurnButton: Button = new Button("End Turn") {
    font = new AwtFont("Arial", AwtFont.BOLD, 12)
    reactions += {
      case ButtonClicked(_) =>
        println("End Turn button clicked")
        viewListener.endTurn()
        timerPanel.stopTimer()
        timerPanel.resetTimer()
    }
  }

  c.gridy = END_TURN_BUTTON_ROW
  c.gridx = END_TURN_BUTTON_COLUMN
  c.fill = Fill.Horizontal
  layout(endTurnButton) = c
  resetConstraintsValues()
  // END TURN BUTTON - END

  // CALL CABO BUTTON - START
  val callCaboButton: Button = new Button("Call CABO") {
    font = new AwtFont("Arial", AwtFont.BOLD, 12)
    reactions += {
      case ButtonClicked(_) =>
        viewListener.callCabo()
        println("Call CABO button clicked")
        whoCalledCaboLabel.text = "You have called CABO!"
        timerPanel.stopTimer()
        timerPanel.resetTimer()
    }
  }
  c.gridy = CALL_CABO_BUTTON_ROW
  c.gridx = CALL_CABO_BUTTON_COLUMN
  layout(callCaboButton) = c
  // CALL CABO BUTTON - END

  //DISCARD CARD DRAWN BUTTON - START
  private val discardCardDrawnButton: Button = new Button("Discard Drawn Card") {
    font = new AwtFont("Arial", AwtFont.BOLD, 12)
    reactions += {
      case ButtonClicked(_) =>
        println("Discard Drawn Card button clicked")
        viewListener.discardCardDrawn()
    }
  }

  c.gridy = DISCARD_DRAWN_CARD_BUTTON_ROW
  c.gridx = DISCARD_DRAWN_CARD_BUTTON_COLUMN
  layout(discardCardDrawnButton) = c
  // DISCARD CARD DRAWN BUTTON - END

  // EXIT BUTTON - START
  val exitButton: Button = new Button("Exit Game") {
    font = new AwtFont("Arial", AwtFont.BOLD, 12)
    reactions += {
      case ButtonClicked(_) =>
        println("Exit Game button clicked")
        onExitDuringGamePolicy()

    }
  }
  c.gridy = EXIT_BUTTON_ROW
  c.gridx = EXIT_BUTTON_COLUMN
  layout(exitButton) = c
  // EXIT BUTTON - END

  // FUNZIONI DI SUPPORTO - INIZIO
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    peer.getLayout match {
      case gbl: GridBagLayout =>
        val widths = gbl.getLayoutDimensions()(0) // array delle larghezze delle colonne
        val heights = gbl.getLayoutDimensions()(1) // array delle altezze delle righe

        var x = 0
        g.setColor(Color.LIGHT_GRAY)
        for (w <- widths) {
          g.drawLine(x, 0, x, size.height)
          x += w
        }
        g.drawLine(x, 0, x, size.height) // bordo destro

        var y = 0
        for (h <- heights) {
          g.drawLine(0, y, size.width, y)
          y += h
        }
        g.drawLine(0, y, size.width, y) // bordo inferiore
    }
  }


  private def resetConstraintsValues(): Unit = {
    c.gridwidth = 1
    c.gridheight = 1
    c.fill = Fill.None
    c.weightx = 0.0
    c.weighty = 0.0
  }

  private def addEmptyColumn(colIndex: Int, width: Int): Unit = {
    val emptyColumn = new Panel {
      peer.setBorder(BorderFactory.createLineBorder(Color.BLACK, 3))
      preferredSize = new Dimension(width, 1)
    }
    c.gridy = 1
    c.gridx = colIndex
    layout(emptyColumn) = c
  }

  private def addEmptyRow(rowIndex: Int, height: Int): Unit = {
    val emptyRow = new Panel {
      peer.setBorder(BorderFactory.createLineBorder(Color.BLACK, 3))
      preferredSize = new Dimension(1, height)
    }
    c.gridy = rowIndex
    c.gridx = 1
    layout(emptyRow) = c
  }

  private def createCardToggleButton(cardText: String, isMainPlayer: Boolean): ToggleButton = new ToggleButton {
    this.text = cardText // Inizialmente potrebbe essere "Nascosta" o il dorso
    //    margin = new Insets(2, 2, 2, 2)
    font = new AwtFont("Arial", AwtFont.PLAIN, if (isMainPlayer) 12 else 10)
    // icon = new ImageIcon(getClass.getResource("/images/card_back.png")) // Esempio
    // selectedIcon = new ImageIcon(getClass.getResource("/images/card_front_selected.png")) // Esempio

    reactions += {
      case ButtonClicked(_) =>
        if (selected) {
          background = Color.CYAN // Evidenzia se selezionata
          println(s"Carta '${this.text}' selezionata: $selected")
        } else {
          background = UIManager.getColor("Button.background") // Ripristina colore default
          println(s"Carta '${this.text}' deselezionata: $selected")
        }
    }
    listenTo(this)
  }

  def notMyTurnPhase(): DuringGamePanel = {
    this.disableAll()
    this.exitButton.enabled = true
    this
  }

  private def disableAll(): Unit = {
    this.exitButton.enabled = false
    this.callCaboButton.enabled = false
    this.endTurnButton.enabled = false
    this.discardCardDrawnButton.enabled = false

    this.adversariesPanelMap.foreach((k, v) => v.enableCardsButton(false))

    this.playerPanel.enableCardsButton(false)
    this.playerPanel.enabled = false

    this.deckPanel.deckButton.enabled = false
    //    this.deckPanel.enabled = false
    this.discardPanel.deckButton.enabled = false
    //    this.discardPanel.enabled = false

    this.drawnCardButton.enabled = false
    //    this.drawnCardPanel.enabled = false
  }

  private def disableButExit(): Unit = {
    this.disableAll()
    this.exitButton.enabled = true
  }

  override def updateLastTurnLog(turnLog: TurnLog): Unit = {
    Swing.onEDT {
      println(s"DuringGamePanel - updateLastTurnLog: $turnLog")
      if turnLog.events.contains(CaboCalled()) && !this.caboHasCalled then
        whoCalledCaboLabel.text = s"${turnLog.playerName} has called CABO!"
        this.caboHasCalled = true
      logPanel.updateLastTurnLog(turnLog)
    }
  }

  override def updateGameInfo(gameInfo: GProg): Unit = {
    Swing.onEDT {
      println(s"DuringGamePanel - updateGameInfo: $gameInfo")
      this.gameInfoPanel.updateCurrentTurn(gameInfo)
    }
  }

  override def showCardDrawnFromDeck(cardDrawn: Card): Unit = {
    Swing.onEDT {
      println(s"DuringGamePanel - showCardDrawnFromDeck $cardDrawn")
      this.drawnCardButton.text = cardDrawn.toString
      this.myTurnActionsLog.text = textInfoCardDrawnFromDeck(cardDrawn)
    }
  }

  override def showCardDrawnFromDiscards(cardDrawn: Card): Unit = {
    Swing.onEDT {
      println(s"DuringGamePanel - showCardDrawnFromDiscards $cardDrawn")
      this.drawnCardButton.text = cardDrawn.toString
      this.myTurnActionsLog.text = textInfoCardDrawnFromDiscards(cardDrawn)
    }
  }


  override def emptyDiscardStack(): Unit = {
    Swing.onEDT {
      println(s"DuringGamePanel - emptyDiscardStack")
      this.discardPanel.deckButton.text = "Empty"
    }
  }

  override def emptyDrawnCardArea(): Unit = {
    Swing.onEDT {
      println("DuringGamePanel - emptyDrawnCardArea")
      this.drawnCardButton.text = "Empty"
    }
  }


  override def updateDiscardsTopCard(card: Card): Unit = {
    Swing.onEDT {
      println(s"DuringGamePanel - updateDiscardsTopCard $card")
      discardPanel.deckButton.text = card.toString
    }
  }

  override def showYourNthCard(card: Card): Unit =
    Swing.onEDT {
      println(s"DuringGamePanel - showYourNthCard: $card")
      this.myTurnActionsLog.text = s"YOUR CARD SELECTED HAS VALUE $card"
    }

  override def showAdversaryNthCard(adversaryName: String, n: Int, card: Card): Unit = {
    Swing.onEDT {
      println(s"DuringGamePanel - showAdversaryNthCard: $adversaryName, $n, $card")
      this.myTurnActionsLog.text = s"$adversaryName's CARD $n SELECTED HAS VALUE $card"
    }
  }

  override def updateRevealingLog(revealingLog: TurnLog): Unit = {
    Swing.onEDT {
      println(s"DuringGamePanel - updateRevealingLog: $revealingLog")
      logPanel.updateRevealingPhaseLog(revealingLog)
    }
  }

  override def usePowerToExchangeCardWithAdversary(): Unit = {
    Swing.onEDT {
      println(s"$userID - DuringGamePanel - usePowerToExchangeCardWithAdversary called")
      this.disableAll()
      this.exitButton.enabled = true
      this.activateAdversariesCards(true)
      this.activateOwnCards(true)
    }
  }

  override def activateAdversariesCards(areActivated: Boolean): Unit = {
    Swing.onEDT {
      this.adversariesPanelMap.foreach(_._2.enableCardsButton(areActivated))
    }
  }

  override def activateOwnCards(areActivated: Boolean): Unit = {
    Swing.onEDT {
      this.playerPanel.enableCardsButton(areActivated)
    }
  }

  override def notifyYourAdversaryCardSelection(adversaryID: String, index: Int): Unit = {
    Swing.onEDT {
      this.myTurnActionsLog.text += s"\nYou selected card $index of adversary with ID $adversaryID"
    }
  }

  override def notifyYourOwnCardSelection(index: Int): Unit = {
    Swing.onEDT {
      this.myTurnActionsLog.text += s"\nYou selected your card $index"
    }
  }

  override def changeCardWithAdversaryIsDone(): Unit = {
    Swing.onEDT {
      this.myTurnActionsLog.text = s"\nCard exchange with adversary completed!"
      this.disableAll()
      this.exitButton.enabled = true
      this.endTurnButton.enabled = true
      this.callCaboButton.enabled = true
    }
  }

  override def playerIsDisconnected(player: PlayerPlaying): Unit = ???

  override def lostYourConnection(): Unit = ???

  override def updatePlayerWhoIsPlaying(playerID: String): Unit =
    Swing.onEDT {
      adversariesPanelMap.foreach((id, panel) => {
        println(s"$userID - DuringGamePanel - i'm setting as playing ${id == playerID} of player $id")
        panel.setAsPlaying(id == playerID)
      })
    }

  override def startTurn(): Unit =
    Swing.onEDT {
      println("DuringGamePanel - startTurn")
      this.disableAll()
      this.exitButton.enabled = true
      this.deckPanel.deckButton.enabled = true
      this.discardPanel.deckButton.enabled = true
      this.timerPanel.resetTimer()
      this.timerPanel.startTimer()
    }

  override def afterDrawPhase(canDiscardDrawnCard: Boolean): Unit = {
    Swing.onEDT {
      this.disableAll()
      this.exitButton.enabled = true
      this.activateOwnCards(true)
      this.discardCardDrawnButton.enabled = canDiscardDrawnCard
      //      this.playerPanel.enableCardsButton()
    }
  }

  override def usePowerToSeeOwnCard(): Unit = {
    Swing.onEDT {
      println(s"$userID - DuringGamePanel - usePowerToSeeOwnCard called")
      this.disableAll()
      this.exitButton.enabled = true
      this.playerPanel.enabled = true
      this.playerPanel.enableCardsButton(true)
    }
  }

  override def usePowerToSeeAdversaryCard(): Unit = {
    Swing.onEDT {
      println(s"$userID - DuringGamePanel - usePowerToSeeAdversaryCard called")
      this.disableAll()
      this.exitButton.enabled = true
      this.adversariesPanelMap.foreach(_._2.enableCardsButton(true))
    }
  }

  override def enterWaitingPhase(): Unit = {
    Swing.onEDT {
      this.disableAll()
      this.exitButton.enabled = true
    }
  }

  override def enterRevealingInitialCardsPhase(): Unit = {
    Swing.onEDT {
      this.disableAll()
      this.exitButton.enabled = true
      this.playerPanel.enableCardsButton(true)
    }
  }

  override def afterDiscarded(): Unit = {
    Swing.onEDT {
      this.disableAll()
      this.exitButton.enabled = true
      this.endTurnButton.enabled = true
      if !this.caboHasCalled then this.callCaboButton.enabled = true
      this.emptyDrawnCardArea()
    }
  }

  override def gameEndedByCabo(game: GProg): Unit = this.gameEndedWithData(game)(ByCabo())

  override def gameEndedByTurns(game: GProg): Unit = this.gameEndedWithData(game)(ByTurns())

  override def gameEndedByEmptyDeck(game: GProg): Unit = this.gameEndedWithData(game)(ByEmptyDeck())

  private def gameEndedWithData(game: GProg)(ending: Ending): Unit = {
    Swing.onEDT {
      println(s"DuringGamePanel - gameEndedWithData: $game")
      val endingResultsDialog = new DisplayEndingResultsDialog(game)(ending)(onClose = this.viewListener.consultingResultsEnded)
      endingResultsDialog.open()
    }
  }

  def onExitDuringGamePolicy(): Unit = {
    val message = "Are you sure you want to exit the current game? Your progress might be lost."
    val title = "Confirm Exit"

    val options = List("Yes, Exit", "No, Stay")

    val result: Dialog.Result.Value = Dialog.showConfirmation(
      parent = this, // La finestra corrente è il genitore
      message = message,
      title = title,
      optionType = Dialog.Options.YesNo,
    )

    result match {
      case Dialog.Result.Yes =>
        println("User confirmed exit. Initiating exit procedure...")
        viewListener.exit()

      case Dialog.Result.No | Dialog.Result.Cancel | Dialog.Result.Closed =>
        println("User cancelled exit.")
    }
  }

  private def textInfoCardDrawnFromDeck(card: Card): String = {
    s"You have drawn the card: $card."
      + {
      card.power match
        case Power.SeeYourCard() => "\nThis card allow you to see one of your card."
        case Power.SeeYourOpponentCard() => "\nThis card allow you to see one card of one adversary."
        case Power.ChangeOneOfYourCardWithOpponent() => "\nThis card allow you to swap one of your card with one of one adversary."
        case Power.NoPower() => ""
    }
  }

  private def textInfoCardDrawnFromDiscards(card: Card): String = {
    s"You have drawn the card: $card from discard pile."
  }

}
