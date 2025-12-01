package view.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import model.Game.{GameInConstruction, GameInProgress}
import utils.Message
import utils.InitialViewMessages
import utils.DuringGameViewMessages
import view.lobbyphase.actors.InitialPhaseViewActor
import view.gamephase.actors.DuringGameViewActor

// Definiamo i messaggi specifici per il Coordinator
object ViewCoordinatorActor {

  // Comandi interni per gestire il cambio di view
  sealed trait Command extends Message

  // Ordina al coordinator di passare alla Initial View (Menu/Lobby)
  case class SwitchToInitialView() extends Command

  // Ordina al coordinator di passare alla Game View (Partita in corso)
  // Nota: richiede i dati necessari per inizializzare la Game View
  case class SwitchToGameView(
//                               game: GameInProgress,
//                               gameCoordinator: ActorRef[Message]
                             ) extends Command

  // Wrapper generico per inoltrare qualsiasi messaggio alla view attiva
  case class ForwardToView(msg: Message) extends Command

  def apply(userId: String, userName: String, clientRef: ActorRef[Message]): Behavior[Message] =
    Behaviors.setup { ctx =>
      ctx.log.info(s"ViewCoordinator started for user $userId")

      // All'avvio, partiamo subito con la Initial View
      val initialView = ctx.spawn(
        InitialPhaseViewActor(clientRef, userName),
        "InitialView"
      )

      // Inizializza la view (protocollo specifico di InitialPhaseViewActor)
      initialView ! InitialViewMessages.WhoToSendResponse(clientRef)

      new ViewCoordinatorActor(ctx, userId, userName, clientRef).active(initialView, isGamePhase = false)
    }
}

private class ViewCoordinatorActor(
                                    ctx: ActorContext[Message],
                                    userId: String,
                                    userName: String,
                                    clientRef: ActorRef[Message]
                                  ) {
  import ViewCoordinatorActor.*

  /**
   * Comportamento principale: mantiene il riferimento alla view corrente.
   * @param currentView L'attore della view attualmente visualizzata
   * @param isGamePhase Booleano per sapere in che fase siamo (utile per logica specifica)
   */
  def active(currentView: ActorRef[Message], isGamePhase: Boolean): Behavior[Message] =
    Behaviors.receiveMessage {

      // --- 1. RICHIESTE DI CAMBIO VIEW ---

      case SwitchToGameView() =>
        ctx.log.info("Switching to GAME View")

        // Arresta la view precedente
//        ctx.stop(currentView)

        // Crea la nuova view di gioco
        val gameView = ctx.spawn(
          DuringGameViewActor(userId, clientRef, null), // null per mainMenuRef se non usato, o passagli un riferimento valido
          s"InitialView-$userId-${System.currentTimeMillis()}"
        )

        // Inizializza la Game View (Simula quello che faceva il client)
//        gameView ! DuringGameViewMessages.StartGame(game, gameCoordinator)

        // Passa al nuovo stato con la nuova view
        active(gameView, isGamePhase = true)

      case SwitchToInitialView() =>
        ctx.log.info("Switching to INITIAL View")

        // Arresta la view precedente (gioco)
        ctx.stop(currentView)

        // Crea la nuova view iniziale
        val initialView = ctx.spawn(
          InitialPhaseViewActor(clientRef, userName),
          s"InitialView-$userId-${System.currentTimeMillis()}" // Nome univoco per evitare conflitti se ricreato velocemente
        )

        initialView ! InitialViewMessages.WhoToSendResponse(clientRef)

        active(initialView, isGamePhase = false)


      // --- 2. INOLTRO MESSAGGI (FORWARDING) ---

      // Messaggi specifici che sappiamo essere per la view
      case msg: InitialViewMessages.ViewCommand if !isGamePhase =>
        currentView ! msg
        Behaviors.same

      case msg: DuringGameViewMessages.DuringGameViewMessage if isGamePhase =>
        currentView ! msg
        Behaviors.same

      // Messaggio generico di forward (utile se il client non vuole preoccuparsi del tipo)
      case ForwardToView(msg) =>
        currentView ! msg
        Behaviors.same

      // Catch-all: se arriva un messaggio che non conosciamo, proviamo a inoltrarlo
      // (Attenzione: questo potrebbe essere rischioso, meglio essere espliciti coi tipi sopra)
      case other =>
        ctx.log.debug(s"Received unknown message to view: $other")
//        currentView ! other
        Behaviors.same
    }
}