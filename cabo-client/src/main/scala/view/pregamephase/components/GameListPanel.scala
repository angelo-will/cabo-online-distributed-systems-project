package view.pregamephase.components

import model.Game.GameInConstruction
import view.pregamephase.ScreenNavigator

import java.awt.{Color, Font}
import javax.swing.SwingUtilities
import scala.swing.{Alignment, BorderPanel, BoxPanel, Button, Dialog, Label, Orientation, ScrollPane, Swing}
import scala.swing.event.ButtonClicked

trait IListGamesListener:
  def joinGame(game: GameInConstruction): Unit

  def updateGamesList(): Unit

  def returnToStart(): Unit

class GameListPanel(navigator: ScreenNavigator, listener: IListGamesListener) extends BoxPanel(Orientation.Vertical) {

  border = Swing.EmptyBorder(30, 30, 30, 30)

  private val titleLabel = new Label("Unisciti a una delle seguenti partite") {
    font = new Font("Arial", java.awt.Font.BOLD, 20)
    horizontalAlignment = Alignment.Center
  }

  private val gamesContainer = new GameListContainer()

  private val backButton = new Button("Indietro")
  private val refreshGamesButton = new Button("Aggiorna")

  contents += titleLabel
  contents += Swing.VStrut(20)
  contents += backButton
  contents += Swing.VStrut(20)
  contents += refreshGamesButton
  contents += Swing.VStrut(20)
  contents += gamesContainer


  def updateGameList(games: Seq[GameInConstruction]): Unit =
    gamesContainer.updateGameList(games)


  listenTo(backButton, refreshGamesButton)
  reactions += {
    case ButtonClicked(b) =>
      if b == backButton then
        println("GameListPanel: Cliccato 'Indietro'.")
        listener.returnToStart()
        navigator.goToPreviousPanel()
      else if b == refreshGamesButton then
        println("GameListPanel: Cliccato 'Aggiorna'.")
        listener.updateGamesList()
  }

  private class GameListContainer extends ScrollPane {
    private val listContainer = new BoxPanel(Orientation.Vertical) {
      border = Swing.EmptyBorder(10, 10, 10, 10)
      contents += new Label("Caricamento partite in corso...")
    }

    contents = listContainer

    def updateGameList(games: Seq[GameInConstruction]): Unit =
      SwingUtilities.invokeLater(() => {
        listContainer.contents.clear()

        if (games.isEmpty) {
          listContainer.contents += new Label("No games at the moment.") {
            horizontalAlignment = Alignment.Center
            font = new Font("SansSerif", java.awt.Font.ITALIC, 14)
          }
        } else {
          games.foreach { game =>
            listContainer.contents += new GameRowPanel(game)
            listContainer.contents += Swing.VStrut(5)
          }
        }
        listContainer.revalidate()
        listContainer.repaint()
      })
  }

  private class GameRowPanel(game: GameInConstruction) extends BorderPanel {
    border = Swing.LineBorder(Color.LIGHT_GRAY, 1)
    background = Color.WHITE


    private val playerCountLabel = new Label(s"${game.players.size}/${game.gameParameters.maxPlayers} Giocatori") {
      font = new Font("SansSerif", java.awt.Font.BOLD, 14)
    }

    private val gameNameLabel = new Label(game.code) {
      font = new Font("SansSerif", java.awt.Font.PLAIN, 12)
      foreground = Color.DARK_GRAY
    }

    val infoButton = new Button("Info")
    val joinButton = new Button("Entra")

    layout(new BoxPanel(Orientation.Vertical) {
      contents += gameNameLabel
      contents += Swing.VStrut(5)
      contents += playerCountLabel
    }) = BorderPanel.Position.Center

    layout(new BoxPanel(Orientation.Horizontal) {
      contents += infoButton
      contents += Swing.HStrut(10)
      contents += joinButton
    }) = BorderPanel.Position.East

    listenTo(infoButton, joinButton)

    reactions += {
      case ButtonClicked(b) =>
        if b == infoButton then
          showInfoDialog()
        else if b == joinButton then
          if showYesNoJoinDialog() == Dialog.Result.Yes then
            println("Yes pressed to enter in the game")
            listener.joinGame(game)
    }


    private def showInfoDialog(): Unit = {
      val details =
        // TODO: insert -AAA- insert max round
        s"""<html>
           |ID: ${game.code}
           |Max Time Per Turn: ${game.gameParameters.maxTimeRound} sec
           |Max Round:
           |Lobby state: ${game.players.size}/${game.gameParameters.maxPlayers}
           |Players: ${game.players.map(_.name).mkString(", ")}
         """.stripMargin

      Dialog.showMessage(this, details, "Dettagli Partita: " + game.code, Dialog.Message.Info)
    }

    private def showYesNoJoinDialog() =
      Dialog.showConfirmation(
        this,
        s"Vuoi davvero unirti alla partita '${game.code}'?",
        "Conferma Unione Partita",
        Dialog.Options.YesNo,
        Dialog.Message.Question
      )
  }
}



