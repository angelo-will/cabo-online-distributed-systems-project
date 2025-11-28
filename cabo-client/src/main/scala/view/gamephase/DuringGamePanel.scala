package view.gamephase

import model.Game.GameInProgress as GProg
import model.TurnEvent.CaboCalled
import model.{Card, EndGameReason, PlayerPlaying, Power, TurnLog}
import view.gamephase.components.*
import view.gamephase.components.DisplayEndingResultsDialog.*
import view.gamephase.traits.IDuringGameInterface
import view.lobbyphase.ViewListener.IDuringGameViewListener

import java.awt.{Color, GridBagLayout, Font as AwtFont}
import javax.swing.BorderFactory
import scala.swing.GridBagPanel.Fill
import scala.swing.*
import scala.swing.event.ButtonClicked

class DuringGamePanel(viewListener: IDuringGameViewListener, gameInProgress: GProg, userID: String)
  extends GridBagPanel
    with IDuringGameInterface {

  // --- COSTANTI DI LAYOUT ---
  private object LayoutConstants {
    val GAME_INFO_ROW = 1
    val GAME_INFO_COL = 3
    val GAME_INFO_WIDTH = 3

    val ADVERSARIES_START_ROW = 4
    val ADVERSARIES_COL = 1
    val ADVERSARIES_GAP = 1

    val DECK_ROW = ADVERSARIES_START_ROW
    val DECK_COL = 3
    val DISCARD_COL = 5

    val DRAWN_CARD_ROW = DECK_ROW + 2
    val DRAWN_CARD_COL = 4

    val LOG_ROW = ADVERSARIES_START_ROW
    val LOG_COL = 7
    val LOG_HEIGHT = 3

    val MY_TURN_LOG_ROW = DRAWN_CARD_ROW + 2
    val MY_TURN_LOG_COL = 3
    val MY_TURN_LOG_WIDTH = 3
    val MY_TURN_LOG_HEIGHT = 2

    val PLAYER_PANEL_ROW = MY_TURN_LOG_ROW + MY_TURN_LOG_HEIGHT
    val PLAYER_PANEL_COL = 3
    val PLAYER_PANEL_WIDTH = 3

    val TIMER_ROW = 1
    val TIMER_COL = 7

    val BUTTONS_START_ROW = 7
    val BUTTONS_COL = 7
  }

  import LayoutConstants.*

  private var caboHasCalled = false
  private val c = new Constraints

  peer.setBorder(BorderFactory.createLineBorder(Color.CYAN, 3))


  // Adversaries and main player
  private val adversariesPanelMap: Map[String, PlayerPanel] = gameInProgress.players
    .filter(_.userID != userID)
    .map(p => p.userID -> new PlayerPanel(p.name, index => viewListener.adversaryCardSelected(p.userID, index)))
    .toMap

  private val playerPanel = new PlayerPanel("YOU", n => viewListener.ownCardSelected(n))
  playerPanel.enableCardsButton(true)

  // Game Info
  private val gameInfoPanel = new GameInfoPanel(gameInProgress)


  // Decks
  private val deckPanel = new DeckPanel("Deck", "Deck", () => viewListener.drawFromDeck())

  private val discardPanel = new DeckPanel(
    "Discard",
    gameInProgress.discardDeckStack.cards.headOption.map(_.toString).getOrElse("Empty"),
    viewListener.drawFromDiscard
  )

  private val drawnCardButton = new Button("Nascosta") {
    font = new AwtFont("Arial", AwtFont.PLAIN, 24)
    border = Swing.EmptyBorder(5, 5, 5, 5)
    enabled = false
  }

  private val drawnCardPanel = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(10, 10, 10, 10)
    contents += new Label("Drawn Card") {
      font = new AwtFont("Arial", AwtFont.BOLD, 14)
      horizontalAlignment = Alignment.Center
    }
    contents += Swing.VStrut(5)
    contents += drawnCardButton
  }

  // Logs e Timer
  private val logPanel = new LogPanel()

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
    peer.setBorder(BorderFactory.createLineBorder(Color.MAGENTA, 3))
  }

  private val timerPanel = new TimerPanel(
    gameInProgress.gameParameters.maxTimeRound,
    () => {
      disableButExit()
      drawnCardButton.text = "Empty"
    }
  )

  // Buttons
  private val endTurnButton = createActionButton("End Turn") { () =>
    viewListener.endTurn()
    timerPanel.stopTimer()
    timerPanel.resetTimer()
  }

  private val callCaboButton = createActionButton("Call CABO") { () =>
    viewListener.callCabo()
    //    whoCalledCaboLabel.text = "You have called CABO!"
    timerPanel.stopTimer()
    timerPanel.resetTimer()
  }

  private val discardCardDrawnButton = createActionButton("Discard Drawn Card") { () =>
    viewListener.discardCardDrawn()
  }

  private val exitButton = createActionButton("Exit Game") { () =>
    onExitDuringGamePolicy()
  }


  initializeLayout()


  // IGameInfoView Implementation
  override def updateGameInfo(gameInfo: GProg): Unit = Swing.onEDT {
    // println(s"DuringGamePanel - updateGameInfo: $gameInfo")
    this.gameInfoPanel.updateCurrentTurn(gameInfo)
  }

  override def updatePlayerWhoIsPlaying(playerID: String): Unit = Swing.onEDT {
    adversariesPanelMap.foreach { case (id, panel) =>
      panel.setAsPlaying(id == playerID)
    }
  }

  override def updateLastTurnLog(turnLog: TurnLog): Unit = Swing.onEDT {
    if (turnLog.events.contains(CaboCalled()) && !this.caboHasCalled) {
      //      whoCalledCaboLabel.text = s"${turnLog.playerName} has called CABO!"
      this.caboHasCalled = true
    }
    logPanel.updateLastTurnLog(turnLog)
  }

  override def updateRevealingLog(revealingLog: TurnLog): Unit = Swing.onEDT {
    logPanel.updateRevealingPhaseLog(revealingLog)
  }

  override def updateDiscardsTopCard(card: Card): Unit = Swing.onEDT {
    discardPanel.deckButton.text = card.toString
  }

  override def emptyDiscardStack(): Unit = Swing.onEDT {
    this.discardPanel.deckButton.text = "Empty"
  }

  // IGamePhaseStatesView Implementation
  override def enterRevealingInitialCardsPhase(): Unit = Swing.onEDT {
    disableAll()
    exitButton.enabled = true
    playerPanel.enableCardsButton(true)
  }

  override def enterWaitingPhase(): Unit = Swing.onEDT {
    disableButExit()
  }

  override def startTurn(): Unit = Swing.onEDT {
    disableAll()
    exitButton.enabled = true
    deckPanel.deckButton.enabled = true
    discardPanel.deckButton.enabled = true
    timerPanel.resetTimer()
    timerPanel.startTimer()
  }

  override def afterDrawPhase(canDiscardDrawnCard: Boolean): Unit = Swing.onEDT {
    disableAll()
    exitButton.enabled = true
    activateOwnCards(true)
    discardCardDrawnButton.enabled = canDiscardDrawnCard
  }

  override def afterDiscarded(): Unit = Swing.onEDT {
    disableAll()
    exitButton.enabled = true
    endTurnButton.enabled = true
    if (!this.caboHasCalled) callCaboButton.enabled = true
    emptyDrawnCardArea()
  }

  override def gameEndedWithData(game: GProg)(ending: EndGameReason): Unit = Swing.onEDT {
    val endingResultsDialog = DisplayEndingResultsDialog(game)(ending)(onClose = this.viewListener.consultingResultsEnded)
    endingResultsDialog.open()
  }

  // ICardActionView Implementation
  override def showCardDrawnFromDeck(cardDrawn: Card): Unit = Swing.onEDT {
    drawnCardButton.text = cardDrawn.toString
    myTurnActionsLog.text = textInfoCardDrawnFromDeck(cardDrawn)
  }

  override def showCardDrawnFromDiscards(cardDrawn: Card): Unit = Swing.onEDT {
    drawnCardButton.text = cardDrawn.toString
    myTurnActionsLog.text = textInfoCardDrawnFromDiscards(cardDrawn)
  }

  override def emptyDrawnCardArea(): Unit = Swing.onEDT {
    this.drawnCardButton.text = "Empty"
  }

  override def showYourNthCard(card: Card): Unit = Swing.onEDT {
    this.myTurnActionsLog.text = s"YOUR CARD SELECTED HAS VALUE $card"
  }

  override def showAdversaryNthCard(adversaryName: String, n: Int, card: Card): Unit = Swing.onEDT {
    this.myTurnActionsLog.text = s"$adversaryName's CARD $n SELECTED HAS VALUE $card"
  }

  // IPowerInteractionView Implementation
  override def usePowerToSeeOwnCard(): Unit = Swing.onEDT {
    disableAll()
    exitButton.enabled = true
    playerPanel.enabled = true
    playerPanel.enableCardsButton(true)
  }

  override def usePowerToSeeAdversaryCard(): Unit = Swing.onEDT {
    disableAll()
    exitButton.enabled = true
    adversariesPanelMap.values.foreach(_.enableCardsButton(true))
  }

  override def usePowerToExchangeCardWithAdversary(): Unit = Swing.onEDT {
    disableAll()
    exitButton.enabled = true
    activateAdversariesCards(true)
    activateOwnCards(true)
  }

  override def activateAdversariesCards(areActivated: Boolean): Unit = Swing.onEDT {
    adversariesPanelMap.values.foreach(_.enableCardsButton(areActivated))
  }

  override def activateOwnCards(areActivated: Boolean): Unit = Swing.onEDT {
    playerPanel.enableCardsButton(areActivated)
  }

  override def notifyYourAdversaryCardSelection(adversaryID: String, index: Int): Unit = Swing.onEDT {
    myTurnActionsLog.text += s"\nYou selected card $index of adversary with ID $adversaryID"
  }

  override def notifyYourOwnCardSelection(index: Int): Unit = Swing.onEDT {
    myTurnActionsLog.text += s"\nYou selected your card $index"
  }

  override def changeCardWithAdversaryIsDone(): Unit = Swing.onEDT {
    myTurnActionsLog.text = s"\nCard exchange with adversary completed!"
    disableAll()
    exitButton.enabled = true
    endTurnButton.enabled = true
    callCaboButton.enabled = true
  }

  // IConnectionsInfo Implementation
  override def playerIsDisconnected(player: PlayerPlaying): Unit = Swing.onEDT {
    // TODO: Implement UI feedback for disconnected player
    println(s"Player disconnected: ${player.name}")
  }

  override def lostYourConnection(): Unit = Swing.onEDT {
    // TODO: Implement UI feedback for lost connection
    println("Lost connection!")
  }

  private def initializeLayout(): Unit = {
    resetConstraintsValues()

    val spacePanel = new Panel {
      preferredSize = new Dimension(1, 1)
      // peer.setBorder(BorderFactory.createLineBorder(Color.GREEN, 3)) // Debug
    }
    c.gridx = 0
    c.gridy = 0
    c.gridwidth = 9
    c.weightx = 1.0
    c.fill = Fill.Horizontal
    layout(spacePanel) = c
    resetConstraintsValues()

    var currentRow = ADVERSARIES_START_ROW
    adversariesPanelMap.values.foreach { panel =>
      addToLayout(panel, ADVERSARIES_COL, currentRow)
      addEmptyRow(currentRow + 1, 50)
      currentRow += 2
    }

    addEmptyColumn(2, 50)

    // Info
    c.gridwidth = GAME_INFO_WIDTH
    c.fill = Fill.Both
    addToLayout(gameInfoPanel, GAME_INFO_COL, GAME_INFO_ROW)
    resetConstraintsValues()

    // Decks
    addToLayout(deckPanel, DECK_COL, DECK_ROW)
    addToLayout(discardPanel, DISCARD_COL, DECK_ROW)
    addToLayout(drawnCardPanel, DRAWN_CARD_COL, DRAWN_CARD_ROW)

    // My Turn Log
    c.gridwidth = MY_TURN_LOG_WIDTH
    c.gridheight = MY_TURN_LOG_HEIGHT
    
    c.fill = Fill.Both
    val myTurnLogRow = if currentRow <= MY_TURN_LOG_ROW then MY_TURN_LOG_ROW else currentRow
    println(s"currentRow: $currentRow, MY_TURN_LOG_ROW: $MY_TURN_LOG_ROW, myTurnLogRow: $myTurnLogRow")
    resetConstraintsValues()

    // Player Panel
    c.gridwidth = PLAYER_PANEL_WIDTH
    c.fill = Fill.Both
    addToLayout(playerPanel, PLAYER_PANEL_COL, myTurnLogRow + MY_TURN_LOG_HEIGHT)
    resetConstraintsValues()

    addEmptyColumn(6, 50)

    // Timer & Logs
    addToLayout(timerPanel, TIMER_COL, TIMER_ROW)

    c.gridheight = LOG_HEIGHT
    c.fill = Fill.Both
    addToLayout(logPanel, LOG_COL, LOG_ROW)
    resetConstraintsValues()

    // Buttons
    var btnRow = BUTTONS_START_ROW
    c.fill = Fill.Horizontal
    List(discardCardDrawnButton, endTurnButton, callCaboButton, exitButton).foreach { btn =>
      addToLayout(btn, BUTTONS_COL, btnRow)
      btnRow += 1
    }
    resetConstraintsValues()
  }

  // Helpers
  private def addToLayout(comp: Component, x: Int, y: Int): Unit = {
    c.gridx = x
    c.gridy = y
    layout(comp) = c
  }

  private def createActionButton(label: String)(f: () => Unit): Button = new Button(label) {
    font = new AwtFont("Arial", AwtFont.BOLD, 12)
    reactions += { case ButtonClicked(_) => f() }
  }

  private def addEmptyColumn(colIndex: Int, width: Int): Unit = {
    val emptyColumn = new Panel {
      preferredSize = new Dimension(width, 1)
    }
    c.gridy = 1
    c.gridx = colIndex
    layout(emptyColumn) = c
  }

  private def addEmptyRow(rowIndex: Int, height: Int): Unit = {
    val emptyRow = new Panel {
      preferredSize = new Dimension(1, height)
    }
    c.gridy = rowIndex
    c.gridx = 1
    layout(emptyRow) = c
  }

  private def resetConstraintsValues(): Unit = {
    c.gridwidth = 1
    c.gridheight = 1
    c.fill = Fill.None
    c.weightx = 0.0
    c.weighty = 0.0
  }

  private def disableAll(): Unit = {
    exitButton.enabled = false
    callCaboButton.enabled = false
    endTurnButton.enabled = false
    discardCardDrawnButton.enabled = false

    adversariesPanelMap.values.foreach(_.enableCardsButton(false))
    playerPanel.enableCardsButton(false)
    playerPanel.enabled = false

    deckPanel.deckButton.enabled = false
    discardPanel.deckButton.enabled = false
    drawnCardButton.enabled = false
  }

  private def disableButExit(): Unit = {
    disableAll()
    exitButton.enabled = true
  }

  def onExitDuringGamePolicy(): Unit = {
    val result = Dialog.showConfirmation(
      parent = this,
      message = "Are you sure you want to exit the current game? Your progress might be lost.",
      title = "Confirm Exit",
      optionType = Dialog.Options.YesNo
    )

    if (result == Dialog.Result.Yes) {
      viewListener.exit()
    }
  }

  private def textInfoCardDrawnFromDeck(card: Card): String = {
    val powerText = card.power match {
      case Power.SeeYourCard() => "\nThis card allows you to see one of your cards."
      case Power.SeeYourOpponentCard() => "\nThis card allows you to see one card of an adversary."
      case Power.ChangeOneOfYourCardWithOpponent() => "\nThis card allows you to swap one of your cards with an adversary's."
      case Power.NoPower() => ""
    }
    s"You have drawn the card: $card.$powerText"
  }

  private def textInfoCardDrawnFromDiscards(card: Card): String = {
    s"You have drawn the card: $card from discard pile."
  }

  // Paint grid
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    peer.getLayout match {
      case gbl: GridBagLayout =>
        val widths = gbl.getLayoutDimensions()(0)
        val heights = gbl.getLayoutDimensions()(1)
        var x = 0
        g.setColor(Color.LIGHT_GRAY)
        for (w <- widths) {
          g.drawLine(x, 0, x, size.height)
          x += w
        }
        var y = 0
        for (h <- heights) {
          g.drawLine(0, y, size.width, y)
          y += h
        }
    }
  }
}