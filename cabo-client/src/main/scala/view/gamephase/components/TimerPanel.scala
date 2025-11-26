package view.gamephase.components

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.Timer
import scala.swing.{Alignment, BoxPanel, Label, Orientation}

class TimerPanel(time: Int, endTimerBehavior: () => Unit) extends BoxPanel(Orientation.Vertical) {
  private val interval = 1000
  private val turnTime = time
  private var counter = turnTime
  private val task: ActionListener = (e: ActionEvent) =>
    if counter >= 0 then {
      timerValueLabel.text = s"Time left: $counter seconds"
      counter -= 1
    }
    else {
      timer.stop()
      timerValueLabel.text = "Time's up!"
      endTimerBehavior()
    }
  private val timer = new Timer(interval, task)
  timer.setInitialDelay(0)

  private val timerValueLabel = new Label("Time left:") {
    font = new java.awt.Font("Arial", java.awt.Font.BOLD, 24)
    horizontalAlignment = Alignment.Center
  }

  contents += timerValueLabel

  def startTimer(): Unit = {
    timer.start()
  }

  def stopTimer(): Unit = {
    timer.stop()
  }

  def resetTimer(): Unit = {
    counter = turnTime
    timerValueLabel.text = s"Time left: $counter seconds"
  }
}
