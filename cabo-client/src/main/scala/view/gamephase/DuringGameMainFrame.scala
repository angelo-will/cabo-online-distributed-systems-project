package view.gamephase

import akka.actor.typed.ActorRef
import model.{CardStack, GameParameters, GameStatus, Hand, IGameParameters, PlayerPlaying}
import model.Game.GameInProgress
import utils.Message
import view.lobbyphase.ViewListener.IDuringGameViewListener

import java.awt.{Dimension, Toolkit}
import java.util.{Timer, TimerTask}
import scala.swing.BorderPanel.Position
import scala.swing.{BorderPanel, BoxPanel, Label, MainFrame, Orientation, Panel, Point, Swing}

class DuringGameMainFrame(val viewListener: IDuringGameViewListener) extends MainFrame:
  title = "Cabo - The Game"

  // Set frame dimension
  val screenSize: Dimension = Toolkit.getDefaultToolkit.getScreenSize
  val screenWidth: Int = screenSize.getWidth.toInt
  val screenHeight: Int = screenSize.getHeight.toInt
  val appWidth: Int = (screenWidth * 0.5).toInt
  val appHeight: Int = (screenHeight * 0.5).toInt
  val verticalPosition: Int = (screenHeight * 0.25).toInt
  val horizontalPosition: Int = (screenWidth * 0.25).toInt

  preferredSize = new Dimension(appWidth, appHeight)
  location = new Point(horizontalPosition, verticalPosition)

  // TODO: aggiungere dialog "Sei sicuro di uscire?"

  private val containerPanel = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(30, 30, 30, 30)
  }

  private var duringGamePanel: Option[DuringGamePanel] = None

  setPanel(new WaitingToStartGamePanel())
  contents = containerPanel
  visible = true

  private def setPanel(panel: Panel): Unit =
    containerPanel.contents.clear()
    containerPanel.contents += panel
    containerPanel.revalidate()
    containerPanel.repaint()

  def startGame(game: GameInProgress, userID: String): Unit = {
    println("Starting game...")
    duringGamePanel = Some(new DuringGamePanel(viewListener, game, userID))
    //    duringGamePanel.get.peer.putClientProperty("JComponent.outline", "true")
    setPanel(duringGamePanel.get)
  }

  def setAdversariesCardsButton(enabled: Boolean): Unit =
    if duringGamePanel.isDefined then
      duringGamePanel.get.adversariesPanelMap.foreach((k, v) => v.enableCardsButton(enabled))

  def setExitButton(enabled: Boolean): Unit =
    if duringGamePanel.isDefined then
      duringGamePanel.get.exitButton.enabled = enabled

  def setDeckButton(enabled: Boolean): Unit =
    if duringGamePanel.isDefined then
      duringGamePanel.get.deckPanel.deckButton.enabled = enabled

  def setDiscardDeck(enabled: Boolean): Unit =
    if duringGamePanel.isDefined then
      duringGamePanel.get.discardPanel.deckButton.enabled = enabled

  def setPlayerCardsButton(enabled: Boolean): Unit =
    if duringGamePanel.isDefined then
      duringGamePanel.get.playerPanel.enableCardsButton(enabled)

  def setEndTurnButton(enable: Boolean): Unit =
    if duringGamePanel.isDefined then
      duringGamePanel.get.endTurnButton.enabled = enable

  def setCallCaboButton(enable: Boolean): Unit =
    if duringGamePanel.isDefined then
      duringGamePanel.get.callCaboButton.enabled = enable


private class WaitingToStartGamePanel extends BorderPanel:
  private val waitingLabel = new Label("Waiting for the game to start...")
  layout(waitingLabel) = Position.Center

@main def runTestDuringGameFrame(): Unit = {
  // Assicura che la UI sia creata e manipolata sull'Event Dispatch Thread di Swing
  Swing.onEDT {
    val ui = new DuringGameMainFrame(new IDuringGameViewListener {
      override def showCardNth(cardIndex: Int): Unit = println(s"showCardNth($cardIndex)")

      override def drawFromDeck(): Unit = println("drawFromDeck()")

      override def drawFromDiscard(): Unit = println("drawFromDiscard()")

      override def discardCardNth(carIndex: Int): Unit = println(s"discardCardNth($carIndex)")

      override def discardCardDrawn(): Unit = println("discardCardDrawn()")

      override def showAdversaryNthCard(adversaryID: String, cardIndex: Int): Unit = println(s"showAdversaryNthCard($adversaryID, $cardIndex)")

      override def swapCardWithAdversaryNthCard(ownCardIndex: Int, adversaryID: String, adversaryCardIndex: Int): Unit = println(s"swapCardWithAdversaryNthCard($ownCardIndex, $adversaryID, $adversaryCardIndex)")

      override def endTurn(): Unit = println("endTurn()")

      override def callCabo(): Unit = println("callCabo()")
    })
    ui.visible = true

    val timer = new Timer()
    timer.schedule(new TimerTask {
      override def run(): Unit = {
        println("Timer finished!")
        val userID = "player1"
        ui.startGame(generateGameInProgress(userID), userID)
        timer.cancel() // Stops the timer after execution
      }
    }, 3000) // 3000 milliseconds = 3 seconds
  }

  def generateGameInProgress(userID: String): GameInProgress = {
    // 1. Creazione dei giocatori e delle loro mani
    val fullDeck = CardStack.buildShuffledFullDeck
    val (hand1Cards, deckAfterHand1) = fullDeck.drawNCards(4)
    val (hand2Cards, deckAfterHand2) = deckAfterHand1.drawNCards(4)
    //    val (hand3Cards, deckAfterHand3) = deckAfterHand2.drawNCards(4)
    //    val (hand4Cards, deckAfterHand4) = deckAfterHand3.drawNCards(4)

    val player1 = PlayerPlaying(userID, "Alice", 1, Hand(hand1Cards))
    val player2 = PlayerPlaying("user2", "Bob", 2, Hand(hand2Cards))
    //    val player3 = PlayerPlaying("user3", "Charlie", Hand(hand3Cards))
    //    val player4 = PlayerPlaying("user4", "Diana", Hand(hand4Cards))


    val playersList = List(
      player1,
      player2,
      //      player3,
      //      player4
    )

    // 2. Creazione del mazzo e del mazzo degli scarti
    val gameDeck = deckAfterHand2
    //    val gameDeck = deckAfterHand4
    val discardDeck = CardStack.buildEmptyDeck

    // 3. Creazione dei parametri del gioco e dello stato
    val gameParameters: IGameParameters = GameParameters()
    val gameStatus = GameStatus.InProgress()
    val currentRound = 1

    GameInProgress(
      code = "game-1234",
      gameParameters = gameParameters,
      gameStatus = gameStatus,
      players = playersList,
      deckStack = gameDeck,
      discardDeckStack = discardDeck,
      currentRound = currentRound
    )
  }
}


