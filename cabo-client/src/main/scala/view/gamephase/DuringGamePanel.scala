package view.gamephase

import model.Game.GameInProgress
import model.{Card, PlayerPlaying, TurnLog}
import model.TurnPhase.TurnPhase
import view.lobbyphase.ViewListener.IDuringGameViewListener

import scala.swing.*
import scala.swing.event.*
import scala.swing.GridBagPanel.Fill
import scala.swing.GridBagPanel.Anchor
import java.awt.{Color, GridBagConstraints, GridBagLayout, Insets, Font as AwtFont}
import javax.swing.{BorderFactory, ImageIcon, UIManager}

//class DuringGamePanelLogic(viewListener: IDuringGameViewListener, gameInProgress: GameInProgress, userID: String) extends GridBagPanel {
//  val duringGamePanel: DuringGamePanel = new DuringGamePanel(viewListener, gameInProgress, userID).revealingInitialCardsPhase()
//}

class DuringGamePanel(viewListener: IDuringGameViewListener, gameInProgress: GameInProgress, userID: String) extends GridBagPanel with IDuringGameInterface {

  private val PLAYER_CARDS = 4
  private val MIN_CENTER_FIELD_ROWS = 10
  private val NORTH_OFFSET_CENTER_FIELDS_ROWS = 2
  private var centerFieldRows = MIN_CENTER_FIELD_ROWS
  peer.setBorder(BorderFactory.createLineBorder(Color.CYAN, 3))

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

  // CREAZIONE GIOCATORI -- INIZIO
  private val columnAdversariesIndex = 1
  //  private var columnAdversariesIndex = columnAdversariesStartIndex
  private val rowAdversaryStartIndex = NORTH_OFFSET_CENTER_FIELDS_ROWS
  private var rowAdversaryIndex = rowAdversaryStartIndex

  //  private def nextAdversaryRow() =
  //    rowAdversaryIndex += 2
  //    rowAdversaryIndex

  adversariesPanelMap.foreach { (id, panel) =>
    c.gridy = rowAdversaryIndex
    c.gridx = columnAdversariesIndex
    layout(panel) = c
    val emptyRow = rowAdversaryIndex + 1
    addEmptyRow(emptyRow, 50)
    rowAdversaryIndex = emptyRow + 1
  }

  // For correct positioning the center field need 6 rows at least
  if (rowAdversaryIndex - rowAdversaryStartIndex) > centerFieldRows then
    centerFieldRows = rowAdversaryIndex - rowAdversaryStartIndex

  // BOTTONE USCITA DAL GIOCO - INIZIO
  val exitButton: Button = new Button("Exit Game") {
    font = new AwtFont("Arial", AwtFont.BOLD, 12)
    reactions += {
      case ButtonClicked(_) =>
        println("Exit Game button clicked")
    }
  }
  private val exitButtonRowIndex = NORTH_OFFSET_CENTER_FIELDS_ROWS + centerFieldRows + 1
  c.gridy = exitButtonRowIndex
  c.gridx = columnAdversariesIndex
  layout(exitButton) = c
  // BOTTONE USCITA DAL GIOCO - FINE

  // COLONNA VUOTA DI RIEMPIMENTO
  addEmptyColumn(2, 50)
  ////////////////////

  // CREAZIONE SCHERMATA DATI PARTITA E TURNO - INIZIO
  private val gameInfoPanel = new BoxPanel(Orientation.Vertical) {
    peer.setBorder(BorderFactory.createLineBorder(Color.RED, 3))
    //    border = Swing.EmptyBorder(10, 10, 10, 10)
    private val gameCodeLabel = new Label(s"Game Code: ${gameInProgress.code}") {
      font = new AwtFont("Arial", AwtFont.BOLD, 16)
      horizontalAlignment = Alignment.Center
    }

    private val numMaxTurnsLabel = new Label(s"Rounds ${gameInProgress.gameParameters.roundLimitation}") {
      font = new AwtFont("Arial", AwtFont.BOLD, 12)
      horizontalAlignment = Alignment.Center
    }

    private val currentTurnLabel = new Label(s"Round N: ${gameInProgress.currentRound}") {
      font = new AwtFont("Arial", AwtFont.BOLD, 12)
      horizontalAlignment = Alignment.Center
    }
    contents += gameCodeLabel
    contents += Swing.VStrut(5)
    contents += numMaxTurnsLabel
    contents += Swing.VStrut(5)
    contents += currentTurnLabel
  }

  private val rowInfoGameIndex = 1
  c.gridx = 3
  c.gridy = rowInfoGameIndex
  c.gridwidth = 3
  c.fill = Fill.Both
  //  c.weightx = 1.0
  layout(gameInfoPanel) = c
  resetConstraintsValues()
  // CREAZIONE SCHERMATA DATI PARTITA E TURNO - FINE

  // CREAZIONE MAZZO PRINCIPALE - INIZIO
  val deckPanel = new DeckPanel("Deck")
  private val deckPanelRowIndex = NORTH_OFFSET_CENTER_FIELDS_ROWS + 1
  private val deckPanelColumnIndex = 3
  private val deckHeight = 2
  c.gridx = deckPanelColumnIndex
  c.gridy = deckPanelRowIndex
  c.gridheight = deckHeight
  layout(deckPanel) = c
  resetConstraintsValues()
  // CREAZIONE MAZZO PRINCIPALE - FINE

  // CREAZIONE MAZZO SCARTI - INIZIO
  val discardPanel = new DeckPanel("Discards")
  private val discardPanelColumnIndex = deckPanelColumnIndex + 2
  c.gridx = discardPanelColumnIndex
  c.gridy = deckPanelRowIndex
  c.gridheight = deckHeight
  layout(discardPanel) = c
  resetConstraintsValues()
  // CREAZIONE MAZZO SCARTI - FINE

  // CREAZIONE CARTA PESCATA - INIZIO
  private val drawnCardPanel = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(10, 10, 10, 10)
    private val drawnCardLabel = new Label("Drawn Card") {
      font = new AwtFont("Arial", AwtFont.BOLD, 14)
      horizontalAlignment = Alignment.Center
    }

    private val drawnCardButton = new Button("Nascosta") {
      font = new AwtFont("Arial", AwtFont.PLAIN, 24)
      border = Swing.EmptyBorder(5, 5, 5, 5)
    }

    contents += drawnCardLabel
    contents += Swing.VStrut(5)
    contents += drawnCardButton
  }
  private val drawnCardPanelColumnIndex = deckPanelColumnIndex + 1
  private val drawnCardPanelRowIndex = deckPanelRowIndex + deckHeight + 1
  c.gridx = drawnCardPanelColumnIndex
  c.gridy = drawnCardPanelRowIndex
  c.gridheight = deckHeight
  layout(drawnCardPanel) = c
  resetConstraintsValues()
  // CREAZIONE CARTA PESCATA - FINE

  // CREAZIONE PANNELLO AZIONI COMPIUTE - INIZIO

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

  c.gridx = 3
  private val myTurnLogRowIndex = drawnCardPanelRowIndex + deckHeight + 1
  private val myTurnLogHeight = 2
  c.gridy = myTurnLogRowIndex
  //  c.gridy = exitButtonRowIndex
  c.gridwidth = 3
  c.gridheight = 2
  c.fill = Fill.Both
  //  c.weightx = 1.0
  layout(myTurnScrollPane) = c
  resetConstraintsValues()
  // CREAZIONE PANNELLO AZIONI COMPIUTE - FINE

  // CRAZIONE PANNELLO GIOCATORE SE STESSO - INIZIO
  c.gridx = 3
  //  c.gridy = exitButtonRowIndex
  c.gridy = myTurnLogRowIndex + myTurnLogHeight + 1
  c.gridwidth = 3
  c.fill = Fill.Both
  //  c.weightx = 1.0
  layout(playerPanel) = c
  resetConstraintsValues()

  // CRAZIONE PANNELLO GIOCATORE SE STESSO - FINE

  // COLONNA VUOTA DI RIEMPIMENTO
  addEmptyColumn(6, 50)
  ////////////////////

  // CREAZIONE TEXT AREA LOG - INIZIO
  private val logTextArea = new TextArea {
    editable = false
    lineWrap = true
    wordWrap = true
    font = new AwtFont("Arial", AwtFont.PLAIN, 12)
    text = "ULTIMO TURNO GIOCATO:"
  }
  private val logScrollPane = new ScrollPane(logTextArea) {
    verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
    //    preferredSize = new Dimension(1, 100)
    peer.setBorder(BorderFactory.createLineBorder(Color.MAGENTA, 3))
  }

  c.gridy = NORTH_OFFSET_CENTER_FIELDS_ROWS
  c.gridx = 7
  c.gridheight = 6
  //  c.gridwidth = 2
  c.fill = Fill.Vertical
  //  c.weighty =
  layout(logScrollPane) = c

  resetConstraintsValues()

  // CREAZIONE TEXT AREA LOG - FINE

  // CREAZIONE TASTI END TURN E CALL CABO - INIZIO

  val endTurnButton: Button = new Button("End Turn") {
    font = new AwtFont("Arial", AwtFont.BOLD, 12)
    reactions += {
      case ButtonClicked(_) =>
        println("End Turn button clicked")
      // viewListener.endTurn()
    }
  }

  c.gridy = exitButtonRowIndex
  c.gridx = 7
  layout(endTurnButton) = c

  val callCaboButton: Button = new Button("Call CABO") {
    font = new AwtFont("Arial", AwtFont.BOLD, 12)
    reactions += {
      case ButtonClicked(_) =>
        println("Call CABO button clicked")
      // viewListener.callCabo()
    }
  }
  c.gridy = exitButtonRowIndex + 1
  c.gridx = 7
  layout(callCaboButton) = c
  // CREAZIONE TASTI END TURN E CALL CABO - FINE


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

  // Defining the phases of the game panel - start
  def revealingInitialCardsPhase(): DuringGamePanel = {
    this.disableAll()
    this.exitButton.enabled = true
    this.playerPanel.enableCardsButton(true)
    this
  }

  def beforeDrawPhase(): DuringGamePanel = {
    this.disableAll()
    this.deckPanel.enabled = true
    this.discardPanel.enabled = true
    this.exitButton.enabled = true
    this
  }

  //  def beforeDrawPhase(): DuringGamePanel = {
  //    this.exitButton.enabled = true
  //    this.callCaboButton.enabled = false
  //    this.endTurnButton.enabled = false
  //    this.adversariesPanelMap.foreach((k, v) => v.disableCardsButton())
  //  }

  def notMyTurnPhase(): DuringGamePanel = {
    this.disableAll()
    this.exitButton.enabled = true
    this
  }

  private def disableAll(): Unit = {
    this.exitButton.enabled = false
    this.callCaboButton.enabled = false
    this.endTurnButton.enabled = false
    this.adversariesPanelMap.foreach((k, v) => v.enableCardsButton(false))
    this.playerPanel.enableCardsButton(false)
    this.deckPanel.deckButton.enabled = false
    this.discardPanel.deckButton.enabled = false
  }
  // Defining the phases of the game panel - end


  override def updateLastTurnLog(turnLog: TurnLog): Unit = ???

  override def updateGameInfo(gameInfo: GameInProgress): Unit = ???

  override def showCardDrawnFromDeck(cardDrawn: Card): Unit = ???

  override def showCardDrawnFromDiscards(cardDrawn: Card): Unit = ???

  override def newDiscardsTopCard(card: Card): Unit = ???

  override def showYourNthCard(card: Card): Unit =
    println(s"DuringGamePanel - showYourNthCard: $card")
    this.myTurnActionsLog.text = s"YOUR CARD SELECTED HAS VALUE $card"

  override def showAdversaryNthCard(adversaryName: String, n: Int, card: Card): Unit = ???

  override def changeCardWithAdversaryIsDone(): Unit = ???

  override def playerIsDisconnected(player: PlayerPlaying): Unit = ???

  override def lostYourConnection(): Unit = ???

  override def startTurn(): Unit = ???

  override def enterWaitingPhase(): Unit = 
    this.disableAll()
    this.exitButton.enabled = true
}

private class PlayerPanel(playerName: String, f: (index: Int) => Unit) extends BoxPanel(Orientation.Vertical):
  //  border = Swing.EmptyBorder(10, 10, 10, 10)
  peer.setBorder(BorderFactory.createLineBorder(Color.BLUE, 3))
  private val nameLabel = new Label(playerName) {
    font = new AwtFont("Arial", AwtFont.BOLD, 14)
    horizontalAlignment = Alignment.Center
  }

  private val seqButtonCards: IndexedSeq[Button] = for (i <- 1 to 4) yield {
    //      contents += new Button(s"$i") {
    new Button(s"$i") {
      font = new AwtFont("Arial", AwtFont.PLAIN, 24)
      border = Swing.EmptyBorder(0, 5, 0, 5)
      enabled = false
      reactions += {
        case ButtonClicked(_) =>
          println(s"Player '$playerName' card $i clicked")
          f(i)
        //          f(i - 1) // Call the function with the index (0-based)
      }
    }
  }

  private val cards: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    seqButtonCards.foreach(b => contents += b)
  }

  def enableCardsButton(enable: Boolean): Unit = {
    seqButtonCards.foreach(b => b.enabled = enable)
  }

  contents += nameLabel
  contents += Swing.VStrut(5)
  contents += cards

private class DeckPanel(name: String) extends BoxPanel(Orientation.Vertical) {
  border = Swing.EmptyBorder(10, 10, 10, 10)
  private val discardLabel = new Label(name) {
    font = new AwtFont("Arial", AwtFont.BOLD, 14)
    horizontalAlignment = Alignment.Center
  }

  val deckButton: Button = new Button(name) {
    font = new AwtFont("Arial", AwtFont.PLAIN, 24)
    border = Swing.EmptyBorder(5, 5, 5, 5)
  }

  contents += discardLabel
  contents += Swing.VStrut(5)
  contents += deckButton
} 


