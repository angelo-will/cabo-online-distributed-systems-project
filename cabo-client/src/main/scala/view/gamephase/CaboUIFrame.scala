package view.gamephase

import scala.swing._
import scala.swing.event._
import java.awt.{Color, Dimension, Insets, GridBagConstraints, Font => AwtFont} // AwtFont per evitare ambiguità
import javax.swing.{ImageIcon, UIManager}

class CaboUIFrame extends MainFrame {
  title = "Cabo Online"
//  preferredSize = new Dimension(1000, 800)

  // --- Costanti ---
//  private val MainPlayerCardWidth = 80
//  private val MainPlayerCardHeight = 120
//  private val OpponentCardWidth = 60
//  private val OpponentCardHeight = 90
  private val PlayerCardCount = 4
  private val MarginSize = 10

  // --- Funzioni Helper (ora metodi privati della classe Frame) ---
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

//  private def createStyledButton(label: String): Button = new Button(label) {
////    margin = new Insets(MarginSize / 2, MarginSize, MarginSize / 2, MarginSize)
//    font = new AwtFont("Arial", AwtFont.BOLD, 12)
//  }

  // --- Layout Principale ---
  val mainPanel = new GridBagPanel {
    background = Color(50, 100, 50) // Verde scuro per il tavolo da gioco
    border = Swing.LineBorder(Color(50, 100, 50), 2) // Bordo nero per il tavolo da gioco

    val c = new Constraints // Oggetto GridBagConstraints

    // --- CARTE DEI GIOCATORI ---
    // Giocatore 0 (Sud - Principale)
    val player0Cards: Seq[ToggleButton] = (0 until PlayerCardCount).map(i =>
      createCardToggleButton(s"P0 C${i + 1}", true)
    ).toList

    player0Cards.zipWithIndex.foreach { case (card, idx) =>
      // Posizionamento delle carte del Giocatore 0 (Sud)
      c.gridx = 5 + (idx * 2) // Spaziatura di 2 colonne tra le carte
      c.gridy = 9
      layout(card) = c
    }

    // Giocatore 1 (Ovest - Avversario)
    val player1Cards: Seq[ToggleButton] = (0 until PlayerCardCount).map(i =>
      createCardToggleButton(s"P1 C${i + 1}", false)
    ).toList

    player1Cards.zipWithIndex.foreach { case (card, idx) =>
      c.gridx = 1
      c.gridy = 3 + idx
      layout(card) = c
    }

    // Giocatore 2 (Nord - Avversario)
    val player2Cards: Seq[ToggleButton] = (0 until PlayerCardCount).map(i =>
      createCardToggleButton(s"P2 C${i + 1}", false)
    ).toList

    player2Cards.zipWithIndex.foreach { case (card, idx) =>
      c.gridx = 5 + idx
      c.gridy = 1
      layout(card) = c
    }

    // Giocatore 3 (Est - Avversario)
    val player3Cards: Seq[ToggleButton] = (0 until PlayerCardCount).map(i =>
      createCardToggleButton(s"P3 C${i + 1}", false)
    ).toList

    player3Cards.zipWithIndex.foreach { case (card, idx) =>
      c.gridx = 13
      c.gridy = 3 + idx
      layout(card) = c
    }

    // --- PANNELLO CENTRALE ---
//    val gameFieldLabel = new Label("Campo di Gioco") {
//      font = new AwtFont("Arial", AwtFont.BOLD, 18)
//      foreground = Color.WHITE
//    }
//    c.gridx = 5
//    c.gridy = 6
//    layout(gameFieldLabel) = c

    val gameDeck = new Button ("Deck")
    c.gridx = 9
    c.gridy = 6
    layout(gameDeck) = c

    val discardPile = new Button ("Discards")
    c.gridx = 3
    c.gridy = 4
    layout(discardPile) = c

    // Pulsanti di azione
    val caboButton: Button = new Button("CABO!")
    c.gridx = 2
    c.gridy = 8
    layout(caboButton) = c

    val usePowerButton: Button = new Button("use pow")
    c.gridx = 10
    c.gridy= 3
    layout(usePowerButton) = c


    val endTurnButton: Button = new Button("end turn")
    c.gridx = 12
    c.gridy = 8
    layout(endTurnButton) = c

  }

  val outerPanel = new BorderPanel {
    layout(mainPanel) = BorderPanel.Position.Center
    border = Swing.EmptyBorder(MarginSize)
    background = new Color(30, 60, 30)
  }
  contents = outerPanel

  // --- Gestione Eventi ---
  // Riferimenti ai bottoni per la gestione eventi devono essere accessibili qui.
  // Poiché sono definiti dentro mainPanel, dobbiamo esporli o accedere tramite mainPanel.contents.
  // Per semplicità, li recupero da mainPanel.contents se necessario, o meglio, li definisco prima.
  // In questo caso, sono già val accessibili nello scope di CaboUIFrame grazie a come è strutturato mainPanel.

  // Per accedere ai bottoni definiti DENTRO mainPanel, e non come campi della classe CaboUIFrame:
  // Dovresti definire `caboButton`, etc., come campi di `CaboUIFrame` se vuoi accedervi
  // direttamente qui in modo pulito.
  // Per ora, assumiamo che la cattura degli eventi sui bottoni come `ToggleButton` (le carte) e
  // i bottoni di azione (che sono `Button`) possa essere gestita se sono nell'elenco `listenTo`.

  // Recupero i riferimenti ai bottoni definiti nel `mainPanel` per la gestione eventi.
  // Questo è un modo per farlo, ma sarebbe più pulito definire i bottoni come membri della classe `CaboUIFrame`.
  // Tuttavia, il tuo codice attuale funziona perché `caboButton`, etc. sono `val`
  // nello scope di `CaboUIFrame` poiché `mainPanel` è un `val` e i suoi contenuti sono definiti nel suo costruttore.
//  private val actionButtonsFromPanel = mainPanel.contents.collect {
//    case gp: GridPanel => gp.contents.collect { case b: Button => b }
//    case bp: FlowPanel => bp.contents.collect { case b: Button => b } // per gameDeck e discardPile
//    case b: Button => Seq(b) // per endTurnButton
//  }.flatten.distinct // distinct per evitare duplicati se la struttura cambia

//  private val gameDeckButton = mainPanel.contents.collectFirst { case fp: FlowPanel => fp.contents.collectFirst { case b: Button if b.text == "Mazzo" => b } }.flatten.get
//  private val discardPileButton = mainPanel.contents.collectFirst { case fp: FlowPanel => fp.contents.collectFirst { case b: Button if b.text == "Scarti" => b } }.flatten.get
//  private val caboButton = mainPanel.contents.collectFirst { case gp: GridPanel => gp.contents.collectFirst { case b: Button if b.text == "CABO!" => b } }.flatten.get
//  private val usePowerButton = mainPanel.contents.collectFirst { case gp: GridPanel => gp.contents.collectFirst { case b: Button if b.text == "USA POTERE" => b } }.flatten.get
//  private val endTurnButton = mainPanel.contents.collectFirst { case b: Button if b.text == "TERMINA TURNO" => b }.get


//  private val allCardButtons = player0Cards ++ player1Cards ++ player2Cards ++ player3Cards
//  private val allActionButtonsManual = Seq(caboButton, usePowerButton, endTurnButton, gameDeckButton, discardPileButton)
//
//
//  (allActionButtonsManual ++ allCardButtons).foreach(listenTo(_))
//
//  reactions += {
//    case ButtonClicked(source) =>
//      source match {
//        case b if b == caboButton => println("CABO!")
//        case b if b == usePowerButton => println("USA POTERE action")
//        case b if b == endTurnButton => println("END TURN action")
//        case b if b == gameDeckButton => println("Draw from Game Deck")
//        case b if b == discardPileButton => println("Interact with Discard Pile")
//        case card: ToggleButton if allCardButtons.contains(card) =>
//          // La logica di feedback visivo è già nel ToggleButton.
//          // Qui si può aggiungere la logica di gioco relativa al click sulla carta.
//          println(s"Evento click su carta: ${card.text}, Selezionata: ${card.selected}")
//        case _ => println(s"Clicked: ${source.getClass.getName}")// Non dovrebbe accadere se listenTo è completo
//      }
//  }
  pack()
  centerOnScreen()
  resizable = true
}

// Entry point dell'applicazione in stile Scala 3
@main def runCaboGame(): Unit = {
  // Assicura che la UI sia creata e manipolata sull'Event Dispatch Thread di Swing
  Swing.onEDT {
    val ui = new CaboUIFrame
    ui.visible = true
  }
}
