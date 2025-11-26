package view.gamephase

import model.{CardStack, GameParameters, GameStatus, Hand, IGameParameters, PlayerPlaying}
import model.Game.GameInProgress
import view.gamephase.traits.IDuringGameInterface
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
  peer.setDefaultCloseOperation(
    javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
  )

  override def closeOperation(): Unit = {
    if duringGamePanel.isDefined then
      duringGamePanel.get.onExitDuringGamePolicy()
    else
      viewListener.exit()
      this.dispose()
  }
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

  def startGame(game: GameInProgress, userID: String): IDuringGameInterface = {
    println("Starting game...")
    duringGamePanel = Some(new DuringGamePanel(viewListener, game, userID))
    //    duringGamePanel.get.peer.putClientProperty("JComponent.outline", "true")
    setPanel(duringGamePanel.get)

    duringGamePanel.get
  }

private class WaitingToStartGamePanel extends BorderPanel:
  private val waitingLabel = new Label("Waiting for the game to start...")
  layout(waitingLabel) = Position.Center
