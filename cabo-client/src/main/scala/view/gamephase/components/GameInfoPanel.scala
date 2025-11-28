package view.gamephase.components

import model.Game.GameInProgress

import java.awt.{Color, Font as AwtFont}
import javax.swing.BorderFactory
import scala.swing.*

class GameInfoPanel(game: GameInProgress) extends BoxPanel(Orientation.Vertical) {

  val stringForCaboNotCalled = "Nobody has called Cabo."
  peer.setBorder(BorderFactory.createLineBorder(Color.RED, 3))
  //    border = Swing.EmptyBorder(10, 10, 10, 10)
  private val gameCodeLabel = new Label(s"Game Code: ${game.code}") {
    font = new AwtFont("Arial", AwtFont.BOLD, 16)
    horizontalAlignment = Alignment.Center
  }

  private val numMaxTurnsLabel = new Label(s"Rounds ${game.gameParameters.roundLimitation}") {
    font = new AwtFont("Arial", AwtFont.BOLD, 12)
    horizontalAlignment = Alignment.Center
  }

  private val currentTurnLabel = new Label("") {
    font = new AwtFont("Arial", AwtFont.BOLD, 12)
    horizontalAlignment = Alignment.Center
  }

  private val whoCalledCaboLabel = new Label(stringForCaboNotCalled) {
    font = new AwtFont("Arial", AwtFont.BOLD, 12)
    horizontalAlignment = Alignment.Center
  }

  this.updateCurrentTurn(game)

  def updateCurrentTurn(game: GameInProgress): Unit = {
    currentTurnLabel.text = s"Round N: ${game.currentRound}"
    if game.isCaboCalled then
      whoCalledCaboLabel.text = s"Player ${game.caboState.get.name} has called cabo"
  }

  contents += gameCodeLabel
  contents += Swing.VStrut(5)
  contents += numMaxTurnsLabel
  contents += Swing.VStrut(5)
  contents += currentTurnLabel
  contents += Swing.VStrut(5)
  contents += whoCalledCaboLabel
}
