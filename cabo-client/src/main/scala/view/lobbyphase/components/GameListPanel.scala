package view.lobbyphase.components

import model.Game.GameInConstruction
import view.lobbyphase.{IViewListener, InitialPhaseNamesEnum, ScreenNavigator}

import java.awt.{Color, Font}
import javax.swing.SwingUtilities
import scala.swing.{Alignment, BorderPanel, BoxPanel, Button, Dialog, Label, Orientation, ScrollPane, Swing}
import scala.swing.event.ButtonClicked
import scala.util.Random

class GameListPanel(navigator: ScreenNavigator, listener: IViewListener) extends BoxPanel(Orientation.Vertical):
  border = Swing.EmptyBorder(30, 30, 30, 30)

  private val titleLabel = new Label("Unisciti a una delle seguenti partite") {
    font = new Font("Arial", java.awt.Font.BOLD, 20)
    horizontalAlignment = Alignment.Center
  }

  private val gamesContainer = new GameListContainer(listener)
  private val backButton = new Button("Indietro")
  private val refreshGamesButton = new Button("Aggiorna")

  contents += titleLabel
  contents += Swing.VStrut(20)
  contents += backButton
  contents += Swing.VStrut(20)
  contents += refreshGamesButton
  contents += Swing.VStrut(20)
  contents += gamesContainer

  // TODO: delete remove this than -AAA- per test
  var games: Seq[GameInConstruction] = _

  def updateGameList(games: Seq[GameInConstruction]): Unit =
    this.games = games
    gamesContainer.updateGameList(games)


  listenTo(backButton, refreshGamesButton)
  reactions += {
    case ButtonClicked(b) =>
      if b == backButton then
        println("GameListPanel: Cliccato 'Indietro'.")
        navigator.showScreen(InitialPhaseNamesEnum.WelcomePanel)
      else if b == refreshGamesButton then
        println("GameListPanel: Cliccato 'Aggiorna'.")
        // TODO: delete remove this than -AAA- togliere quando si prenderanno i dati dal server
        updateGameList(Random.shuffle(games))
  }


private class GameListContainer(listener: IViewListener) extends ScrollPane:
  private val listContainer = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(10, 10, 10, 10)
    contents += new Label("Caricamento partite in corso...")
  }

  contents = listContainer

  def updateGameList(games: Seq[GameInConstruction]): Unit =
    SwingUtilities.invokeLater(() => {
      listContainer.contents.clear()

      if (games.isEmpty) {
        listContainer.contents += new Label("Nessuna partita disponibile al momento.") {
          horizontalAlignment = Alignment.Center
          font = new Font("SansSerif", java.awt.Font.ITALIC, 14)
        }
      } else {
        games.foreach { game =>
          listContainer.contents += new GameRowPanel(game, listener)
          listContainer.contents += Swing.VStrut(5)
        }
      }
      listContainer.revalidate()
      listContainer.repaint()
    })


private class GameRowPanel(game: GameInConstruction, listener: IViewListener) extends BorderPanel:
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
        showJoinConfirmationDialog()
    // TODO: inserire la chiamata al listener corretta per partecipare al game
    // listener.joinAGame("")
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

  private def showJoinConfirmationDialog(): Unit =
    val result = Dialog.showConfirmation(
      this,
      s"Vuoi davvero unirti alla partita '${game.code}'?",
      "Conferma Unione Partita",
      Dialog.Options.YesNo,
      Dialog.Message.Question
    )

    if result == Dialog.Result.Yes then
      println("Yes pressed to enter in the game")
      val waitingDialog = new Dialog() {
        title = "Attesa"
        modal = true
        contents = new BoxPanel(Orientation.Vertical) {
          border = Swing.EmptyBorder(20, 20, 20, 20)
          contents += new Label(s"Tentativo di unione alla partita '${game.code}'...") {
            font = new Font("SansSerif", java.awt.Font.BOLD, 14)
          }
          contents += Swing.VStrut(10)
          contents += new Label("Attendere prego...")
        }
        pack()
        centerOnScreen()
      }

      // TODO: delete remove this than -AAA- Replace to use server response
      scala.concurrent.ExecutionContext.global.execute(() => {
        Thread.sleep(3000)
        SwingUtilities.invokeLater(() => {
          waitingDialog.close()
          println(s"GameRowPanel: Host of game '${game.code}' has accepted your request.")
          listener.joinGame(game)
        })
      })
      waitingDialog.open()



