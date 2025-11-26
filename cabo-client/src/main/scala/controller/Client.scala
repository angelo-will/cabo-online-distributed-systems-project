package controller

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import akka.cluster.ClusterEvent.MemberExited
import model.Game.{GameInConstruction, GameInProgress}
import model.{GameParameters, PlayerInLobby, PlayerPlaying, TurnLog}
import utils.ClientMessages.*
import utils.GameCoordinatorMessage.{GameCoordinatorMessage, NewTurn}
import utils.ServerMessages.{AbortGame, ServerKey}
import utils.{DuringGameViewMessages, GameCoordinatorMessage, InitialViewMessages, Message, ServerMessages}
import view.gamephase.DuringGameViewActor
import view.lobbyphase.actors.InitialPhaseViewActor

import java.util.UUID
import scala.concurrent.duration.DurationInt

object Client:

  trait ClientInternalCommand extends ClientCommand

  private case class ListingResponseListing(listing: Receptionist.Listing) extends ClientInternalCommand

  // Commands about the state before joining/creating a game
  case class IWantToPlay(newPlayer: PlayerInLobby, reply: ActorRef[Message]) extends ClientInternalCommand

  case class YouJoinedTheGame(game: GameInConstruction) extends ClientInternalCommand

  case class YouCanNotJoinTheGame(game: GameInConstruction) extends ClientInternalCommand

  case class FailedToContactHost() extends ClientInternalCommand

  case class UpdateAboutGame(game: GameInConstruction) extends ClientInternalCommand

  case class IWantToLeaveTheGame(player: PlayerInLobby) extends ClientInternalCommand

  case class GameCancelled() extends ClientInternalCommand

  case class GameHasStarted(hostRef: ActorRef[ClientInternalCommand], gameInProgress: GameInProgress) extends ClientInternalCommand

  case class PlayerUnreachable(playerInLobby: PlayerInLobby) extends ClientInternalCommand

  //Commands after the game has started

  case class SynchronizationAck(fromWho: String) extends ClientInternalCommand

  case class FailedToSynchronize() extends ClientInternalCommand

  case class GameInProgressUpdate(replyTo: ActorRef[ClientInternalCommand], game: GameInProgress, turnLog: TurnLog) extends ClientInternalCommand

  case class StartGameBehavior(thisBehavior: () => Behavior[Message], hostRef: ActorRef[ClientInternalCommand]) extends ClientInternalCommand

  case class RemoveCheckPlayerStatus() extends ClientInternalCommand

  //messages for new host election

  case class ElectionStarted(candidateRank: Int, replyTo: ActorRef[ClientInternalCommand]) extends ClientInternalCommand

  case class NoYouCanNot() extends ClientInternalCommand

  case class ElectionWon() extends ClientInternalCommand

  case class NewHostElected(replyTo: ActorRef[ClientInternalCommand]) extends ClientInternalCommand

  // todo: inserito da angelo per far passare la prima fase fino che non è definito come farla
  case class RevealingCardsPhaseForOtherClients(log: TurnLog) extends ClientInternalCommand

  case class PlayerStatus(playerID: String, address: ActorRef[ClientInternalCommand], rank: Int, isOnline: Boolean)

  private def viewDefaultBehavior: Behavior[Message] = Behaviors.setup { ctx =>
    Behaviors.receiveMessagePartial {
      case _ =>
        ctx.log.info("View actor received a message, but it is not implemented yet.")
        Behaviors.same
    }
  }

  def apply(userId: String = "Player", name: String = "defaultCoolName", optionalViewActor: ActorRef[Message] = null): Behavior[Message] = Behaviors.setup { ctx =>
    //    val clientID = userId+ctx.self.path.address.hashCode()
    val clientID = userId + UUID.randomUUID().hashCode()

    val connectionHandler = ctx.spawn(ConnectionHandler[MemberExited](ctx.self), "ConnectionHandler")

    new Client(clientID, name, optionalViewActor, connectionHandler).initialize(optionalViewActor)
  }

private case class Client(userId: String, var name: String, viewActorRef: ActorRef[Message], connectionHandler: ActorRef[ConnectionHandler.InternalCommand]):

  import controller.Client.*

  // todo: aggiunto da angelo ma si può rimuovere quando la revealing first phase sarà definita
  private var howManyHaveWatchedCards = 0

  private case class ListingResponse(listing: Receptionist.Listing) extends Message

  //todo - substitute every log with this
  private def logInfo(ctx: ActorContext[Message], msg: String): Unit = {
    ctx.log.info(s"Client[$userId]: $msg")
  }

  private def logError(ctx: ActorContext[Message], msg: String): Unit = {
    ctx.log.error(s"Client[$userId]: $msg")
  }

  private def contactInReceptionistAndAsk[T](key: ServiceKey[T])(whatToAskTo: ActorRef[T] => Unit)(ifFailure: () => Unit): Behavior[Message] = {
    Behaviors.setup { ctx =>
      val listingResponseAdapter = ctx.messageAdapter[Receptionist.Listing](ListingResponse.apply)

      ctx.system.receptionist ! Receptionist.find(key, listingResponseAdapter)

      Behaviors.receiveMessagePartial {
        case ListingResponse(key.Listing(listing)) =>
          if (listing.nonEmpty) {
            val contact = listing.head
            ctx.log.info(s"Found required contact: $contact")
            whatToAskTo(contact)
          } else {
            ctx.log.error(s"Contact with key ${key.id} not found")
            ifFailure()
          }
          Behaviors.stopped
      }
    }
  }

  private def awaitSynchronization(ctx: ActorContext[Message], usersToWait: List[String], nextBehavior: () => Behavior[Message], ifFailure: () => Behavior[Message]): Behavior[Message] = {
    //    val gameInProgress: GameInProgress = ???
    Behaviors.withStash(100) { buffer =>
      Behaviors.withTimers { timers =>
        var checkSync = Map.empty[String, Boolean] ++ usersToWait.map(id => id -> false)
        //todo - make the timeout configurable
        timers.startTimerAtFixedRate(FailedToSynchronize(), 5.seconds)
        Behaviors.receiveMessage {
          case FailedToSynchronize() =>
            ctx.log.error(s"Failed to synchronize all players in time: ${checkSync.filterNot(_._2).keys.mkString(", ")} did not respond")
            timers.cancelAll()
            buffer.unstashAll(ifFailure())
          case SynchronizationAck(fromWho) =>
            ctx.log.info(s"Synchronization ack received from: $fromWho")
            checkSync = checkSync.updatedWith(fromWho)({ case None => None case Some(v) => Some(true) })
            if checkSync.forall(_._2) then {
              ctx.log.info(s"All players synchronized")
              timers.cancelAll()
              buffer.unstashAll(nextBehavior())
            } else {
              ctx.log.info(s"Waiting for players to synchronize: ${checkSync.filterNot(_._2).keys.mkString(", ")}")
              Behaviors.same
            }
          case other =>
            buffer.stash(other)
            Behaviors.same
        }
      }
    }
  }

  private def sharedHandler: PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, GetPlayerInfo(replyTo)) =>
      ctx.log.info(s"Sending player info to $replyTo")
      replyTo ! PlayerInfo(userId, name)
      Behaviors.same
  }

  private def withShared(
                          specific: PartialFunction[(ActorContext[Message], Message), Behavior[Message]]
                        ): Behavior[Message] =
    Behaviors.receivePartial(sharedHandler.orElse(specific))

  private def initialize(initialViewRef: ActorRef[Message]): Behavior[Message] = {
    Behaviors.setup { ctx =>
      val viewActorRef = if initialViewRef != null then initialViewRef else ctx.spawn(InitialPhaseViewActor(ctx.self, name), "actor-initialphaseview")
      this.copy(viewActorRef = viewActorRef).start
    }
  }


  private def start: Behavior[Message] = Behaviors.setup { ctx =>

    viewActorRef ! InitialViewMessages.WhoToSendResponse(ctx.self)

    withShared({
      case (ctx, CreateNewGame(makePublic, maxTimeRound, maxNumRound, maxPlayers, gameCode)) =>

        val player: PlayerInLobby = PlayerInLobby(userId, name, ctx.self)

        val game: GameInConstruction = GameInConstruction(gameCode.getOrElse(userId + "game"), GameParameters(makePublic, maxTimeRound, maxNumRound, maxPlayers), List(player))

        if makePublic then {
          ctx.spawnAnonymous(contactInReceptionistAndAsk
            (ServerKey)
            (_ ! ServerMessages.RegisterGame(game, ctx.self))
            (() => viewActorRef ! InitialViewMessages.FailedToPublishToServer()))
        }

        ctx.system.receptionist ! Receptionist.register(akka.actor.typed.receptionist.ServiceKey[Message](game.code), ctx.self)

        connectionHandler ! ConnectionHandler.UpdateList(game.players)

        viewActorRef ! InitialViewMessages.GameCreated(game)

        hostBehavior(game)

      case (ctx, JoinAGame()) =>
        ctx.log.info("Preparing to join a game")
        ctx.spawnAnonymous(contactInReceptionistAndAsk
          (ServerKey)
          (_ ! ServerMessages.GetGames(ctx.self))
          (() => viewActorRef ! InitialViewMessages.FailedToPublishToServer()))
        joiningAGame

      case (ctx, ChangePlayerName(newName, replyTo)) =>
        ctx.log.info(s"Changing player name from $name to $newName")
        this.name = newName
        replyTo ! PlayerInfo(userId, name)
        Behaviors.same
    })
  }


  def createPlayersStatus(playersInLobby: List[PlayerInLobby], playersPlaying: List[PlayerPlaying]): List[PlayerStatus] = {
    val idAddress = playersInLobby.map(p => (p.userID, p.address))
    val idRank = playersPlaying.map(p => (p.userID, p.rank))

    val rankById = idRank.map(r => r._1 -> r).toMap

    idAddress.flatMap { a =>
      rankById.get(a._1).map { r =>
        PlayerStatus(a._1, a._2, r._2, true)
      }
    }
  }

  private def hostBehavior(game: GameInConstruction): Behavior[Message] = {

    def removePlayerFromGame(ctx: ActorContext[Message], playerInLobby: PlayerInLobby) = {
      val gameUpdated = game.copy(players = game.players.filterNot(_.userID == playerInLobby.userID))

      if gameUpdated.gameParameters.isPublic then
        ctx.spawnAnonymous(contactInReceptionistAndAsk
          (ServerKey)
          (_ ! ServerMessages.UpdateGame(game, ctx.self))
          (() => viewActorRef ! InitialViewMessages.FailedToPublishToServer()))

      gameUpdated.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! UpdateAboutGame(gameUpdated))

      connectionHandler ! ConnectionHandler.UpdateList(gameUpdated.players)

      viewActorRef ! InitialViewMessages.GameInfoUpdate(gameUpdated)

      ctx.log.info(s"Player: ${playerInLobby.userID} removed the game, now the players are: ${gameUpdated.players.map(_.userID).mkString(", ")}")

      hostBehavior(gameUpdated)
    }

    withShared({

      case (ctx, ServerMessages.GameRegistered(game, server)) =>
        //The server has registered the game
        ctx.log.info(s"Game update: ${game.code} by server: $server")
        Behaviors.same

      case (ctx, ServerMessages.FailedToRegisterGame(game, server)) =>
        //The server has failed to register the game
        ctx.log.error(s"Failed to register game: ${game.code}")
        viewActorRef ! InitialViewMessages.FailedToPublishToServer()
        //Go into lobby
        Behaviors.same

      case (ctx, IWantToPlay(newPlayer: PlayerInLobby, replyTo: ActorRef[Message])) =>
        //The player wants to play
        ctx.log.info(s"Player: ${newPlayer.userID} wants to play")
        if (game.players.size < game.gameParameters.maxPlayers) {
          //The player can join the game
          ctx.log.info(s"Player: ${newPlayer.userID} can join the game: ${game.code}")
          // todo - move this after updating the other players so no need to send update to himself
          val gameUpdated = game.copy(players = game.players :+ newPlayer)

          if gameUpdated.gameParameters.isPublic then
            //Update the game on the server
            ctx.spawnAnonymous(contactInReceptionistAndAsk
              (ServerKey)
              (_ ! ServerMessages.UpdateGame(game, ctx.self))
              (() => viewActorRef ! InitialViewMessages.FailedToPublishToServer()))

          replyTo ! YouJoinedTheGame(gameUpdated)

          connectionHandler ! ConnectionHandler.UpdateList(gameUpdated.players)

          gameUpdated.players.filter(p => !p.address.equals(ctx.self) & !p.address.equals(newPlayer.address)).foreach(_.address ! UpdateAboutGame(gameUpdated))

          viewActorRef ! InitialViewMessages.GameInfoUpdate(gameUpdated)

          hostBehavior(gameUpdated)
        } else {
          //The player cannot join the game
          ctx.log.info(s"Player: $newPlayer cannot join the game: $game")
          replyTo ! YouCanNotJoinTheGame(game)
          Behaviors.same
        }

      case (ctx, IWantToLeaveTheGame(player)) =>
        //A player wants to leave the game
        ctx.log.info(s"Player: ${player.userID} wants to leave the game: ${game.code}")
        removePlayerFromGame(ctx, player)

      case (ctx, PlayerUnreachable(playerInLobby)) =>
        //A player is unreachable
        ctx.log.info(s"Player: ${playerInLobby.userID} is unreachable")
        //todo - decide if we want to wait some time before removing the player
        removePlayerFromGame(ctx, playerInLobby)

      case (ctx, LeaveTheGame()) =>
        //The user wants to leave the game
        ctx.log.info(s"Leaving ${game.code}, aborting game")
        if game.gameParameters.isPublic then
          ctx.spawnAnonymous(contactInReceptionistAndAsk
            (ServerKey)
            (_ ! ServerMessages.AbortGame(game, ctx.self))
            (() => viewActorRef ! InitialViewMessages.FailedToPublishToServer()))
        game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameCancelled())
        ctx.system.receptionist ! Receptionist.deregister(akka.actor.typed.receptionist.ServiceKey[Message](game.code), ctx.self)
        viewActorRef ! InitialViewMessages.GameAborted()
        //todo - if we use the variable argument this has to be changed
        connectionHandler ! ConnectionHandler.UpdateList(List())
        start

      case (ctx, StartTheGame()) =>
        //The game has started
        //        ctx.log.info(s"Starting game: ${game.code}")
        logInfo(ctx, s"Starting game: ${game.code}")

        if game.gameParameters.isPublic then
          ctx.spawnAnonymous(contactInReceptionistAndAsk
            (ServerKey)
            (_ ! ServerMessages.StartGame(game, ctx.self))
            (() => viewActorRef ! InitialViewMessages.FailedToPublishToServer()))

        //        //todo - check if we need to keep it for re-entering the game
        //        ctx.system.receptionist ! Receptionist.deregister(akka.actor.typed.receptionist.ServiceKey[Message](game.code), ctx.self)

        ctx.stop(viewActorRef)
        val duringGameViewActor = ctx.spawn(DuringGameViewActor(userId, ctx.self, null), s"duringGameView-$userId")
        //todo - fix this, you can't call the method directly
        ctx.self ! StartGameBehavior(() => GameCoordinatorActor(ctx.self, duringGameViewActor, userId, game), ctx.self)

        this.copy(viewActorRef = duringGameViewActor).hostBehavior(game)

      case (ctx, StartGameBehavior(thisBehavior, hostRef)) =>

        logInfo(ctx, s"Starting game behavior for game: ${game.code}")

        //todo - check if we need to keep it for re-entering the game
        ctx.system.receptionist ! Receptionist.deregister(akka.actor.typed.receptionist.ServiceKey[Message](game.code), ctx.self)

        val gameCoordinator = ctx.spawn(thisBehavior(), "GameCoordinatorActor")

        hostWaitGameFromCoordinator(hostRef, game, gameCoordinator)
    })
  }

  private def hostWaitGameFromCoordinator(hostRef: ActorRef[ClientInternalCommand], game: GameInConstruction, gameCoordinator: ActorRef[Message]): Behavior[Message] = {
    Behaviors.receivePartial {
      case (ctx, TakeGetInProgressGame(gameInProgress)) =>
        //            ctx.log.info(s"Game in progress received: ${gameInProgress.code}")
        logInfo(ctx, s"Game in progress received: ${gameInProgress.code}")
        game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameHasStarted(hostRef, gameInProgress))
        awaitSynchronization(ctx, game.players.filter(!_.address.equals(ctx.self)).map(_.userID), () => {
          //              ctx.log.info(s"All players synchronized, starting the game: ${gameInProgress.code}")
          logInfo(ctx, s"All players synchronized, starting the game: ${gameInProgress.code}")
          viewActorRef ! DuringGameViewMessages.StartGame(gameInProgress, gameCoordinator)
          gameCoordinator ! GameCoordinatorMessage.StartGame()
          //          viewActorRef ! InitialViewMessages.ReadyToPlay(gameCoordinator)
          inGameBehavior(gameCoordinator, createPlayersStatus(game.players, gameInProgress.players), hostRef)
        }, () => {
          //If failed to synchronize
          //Brutal policy, we abort the game
          //              ctx.log.error(s"Failed to synchronize all players, aborting the game: ${gameInProgress.code}")
          logError(ctx, s"Failed to synchronize all players, aborting the game: ${gameInProgress.code}")
          //todo - check if this is ok
          ctx.stop(gameCoordinator)
          game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameCancelled())
          viewActorRef ! InitialViewMessages.GameAborted()
          //todo - if we use the variable argument this has to be changed
          connectionHandler ! ConnectionHandler.UpdateList(List())
          start
        })
    }
  }

  private def joiningAGame: Behavior[Message] = {

    def responseForJoining(): Behavior[Message] = {
      Behaviors.withTimers { timers =>
        timers.startTimerAtFixedRate(FailedToContactHost(), 60.seconds)
        withShared({
          case (ctx, YouJoinedTheGame(game)) =>
            ctx.log.info(s"Joined game: $game")
            connectionHandler ! ConnectionHandler.UpdateList(List(game.players.head))
            viewActorRef ! InitialViewMessages.GameJoined(game)
            //Joined a game
            gameJoined(game)

          case (ctx, YouCanNotJoinTheGame(game)) =>
            ctx.log.warn("Could not join game")
            viewActorRef ! InitialViewMessages.GameJoinedFailed(game)
            //Failed to join, waiting for other commands from the user
            joiningAGame

          case (ctx, FailedToContactHost()) =>
            ctx.log.info("Failed to contact host")
            timers.cancelAll()
            //todo - ask the view if wants to retry
            joiningAGame
        })
      }
    }

    withShared({

      case (ctx, ServerMessages.GamesList(games)) =>
        if games.nonEmpty then {
          ctx.log.info(s"Games found: $games")
          viewActorRef ! InitialViewMessages.GameList(games.toList)
        } else {
          ctx.log.warn("No games found")
          viewActorRef ! InitialViewMessages.GameList(List())
        }
        Behaviors.same

      case (ctx, JoinAddress(address)) =>

        ctx.spawnAnonymous(contactInReceptionistAndAsk
          (akka.actor.typed.receptionist.ServiceKey[Message](address))
          (_ ! IWantToPlay(PlayerInLobby(userId, name, ctx.self), ctx.self))
          //todo - add a specific message to viewActorRef
            (() => viewActorRef ! InitialViewMessages.FailedToPublishToServer()))

        responseForJoining()

      case (ctx, JoinGame(game)) =>
        ctx.log.info(s"Trying to join game: ${game.code}")
        game.players.head.address ! IWantToPlay(PlayerInLobby(userId, name, ctx.self), ctx.self)
        responseForJoining()
    })
  }

  private def gameJoined(game: GameInConstruction): Behavior[Message] = {

    withShared({

      case (ctx, UpdateAboutGame(game)) =>
        ctx.log.info(s"Game info update: ${game.code}")
        viewActorRef ! InitialViewMessages.GameInfoUpdate(game)
        gameJoined(game)

      case (ctx, LeaveTheGame()) =>
        ctx.log.info(s"Leaving game: ${game.code}")
        game.players.head.address ! IWantToLeaveTheGame(PlayerInLobby(userId, name, ctx.self))
        // todo - decide if waiting for a response or not
        connectionHandler ! ConnectionHandler.UpdateList(List())
        start

      case (ctx, GameCancelled()) =>
        ctx.log.info(s"Game: ${game.code} has been aborted")
        viewActorRef ! InitialViewMessages.GameAborted()
        connectionHandler ! ConnectionHandler.UpdateList(List())
        start

      case (ctx, PlayerUnreachable(playerInLobby)) =>
        //todo - for now the same as above, but we could wait some time before assume the game is aborted
        ctx.log.info(s"Game: ${game.code} has been aborted")
        viewActorRef ! InitialViewMessages.GameAborted()
        connectionHandler ! ConnectionHandler.UpdateList(List())
        start
      case (ctx, GameHasStarted(hostRef, gameInProgress)) =>
        ctx.log.info(s"Game has started: ${game.code}")
        //          val index = gameInProgress.players.indexWhere(p => p.userID == userId && p.name == name)
        hostRef ! SynchronizationAck(userId)
        val duringGameViewActor = ctx.spawn(DuringGameViewActor(userId, ctx.self, null), s"duringGameView-$userId")
        //        viewActorRef ! InitialViewMessages.ReadyToPlay(gameCoordinator)
        val gameCoordinator = ctx.spawn(GameCoordinatorActor(ctx.self, duringGameViewActor, userId, gameInProgress), "GameCoordinatorActor")
        gameCoordinator ! GameCoordinatorMessage.StartGame()
        duringGameViewActor ! DuringGameViewMessages.StartGame(gameInProgress, gameCoordinator)
        //todo - a joiner initially check connection only with the host, in the game he should check also with other players?

        connectionHandler ! ConnectionHandler.UpdateList(game.players)
        viewActorRef ! InitialViewMessages.GameStarted()
        this.copy(viewActorRef = duringGameViewActor).inGameBehavior(gameCoordinator, createPlayersStatus(game.players, gameInProgress.players), hostRef)
      //          ctx.self ! StartGameBehavior(GameCoordinatorActor(ctx.self, viewActorRef, userId, gameInProgress), hostRef)
      //          Behaviors.same

      // may be useful to have a different starter as for the host?
      //        case (ctx, StartGameBehavior(behavior, hostRef)) =>
      //          val gameCoordinator = ctx.spawn(behavior, "GameCoordinatorActor")
      //          viewActorRef ! InitialViewMessages.ReadyToPlay(gameCoordinator)
      //          hostRef ! SynchronizationAck(userId)
      //          //todo - a joiner initially check connection only with the host, in the game he should check also with other players?
      //          connectionHandler ! ConnectionHandler.UpdateList(game.players)
      //          inGameBehavior(gameCoordinator, game.players.map(p => p -> true).toMap, hostRef)
    })
  }

  //todo - retrieve who am i, so the rank, by id from the game players?
  //todo - change the hostRef with a boolean if not needed
  private def inGameBehavior(gameCoordinator: ActorRef[GameCoordinatorMessage], playersStatus: List[PlayerStatus], hostRef: ActorRef[ClientInternalCommand]): Behavior[Message] = {

    //    lazy val otherPlayers = playersStatus.filterNot(_.playerID.equals(this.userId))
    val otherPlayers = playersStatus.filterNot(_.playerID.equals(this.userId))

    //    case class ElectionStarted(candidateRank: Int, replyTo: ActorRef[ClientInternalCommand]) extends ClientInternalCommand
    //    case class NoYouCanNot() extends ClientInternalCommand
    //    case class ElectionWon() extends ClientInternalCommand
    //    case class NewHostElected(replyTo: ActorRef[ClientInternalCommand]) extends ClientInternalCommand

    def checkNextTurn(gameCoordinator: ActorRef[GameCoordinatorMessage], gameInProgress: GameInProgress, ctx: ActorContext[Message]): Unit = {

      ctx.log.info(s"Checking who has turn after ${gameInProgress.currentRound} in game: ${gameInProgress.code}")

      val playerNumber = playersStatus.length

      playersStatus.find(_.rank == gameInProgress.currentRound + 1 % playerNumber) match {
        case Some(value) =>
          if (!value.isOnline) {
            //todo - ask gameCoordinator to skip the turn
            //            ctx.log.info(s"Next turn is for offline player: ${value.playerID}, skipping turn")
            logInfo(ctx, s"Next turn is for offline player: ${value.playerID}, skipping turn")
          } else {
            //            ctx.log.info(s"Next turn is for player: ${value.playerID} and it is online")
            logInfo(ctx, s"Next turn is for player: ${value.playerID} and it is online")
          }
        case None =>
          //should not happen
          ctx.log.error(s"Could not find next player for turn: ${gameInProgress.currentRound + 1}")
      }

    }

    def inElectionBehavior(myRank: Int): Behavior[Message] = {
      Behaviors.withStash(50) { buffer =>
        Behaviors.withTimers { timers =>
          timers.startSingleTimer(ElectionWon(), 5.seconds)
          withShared({
            case (ctx, NoYouCanNot()) =>
              logInfo(ctx, "Someone has a lower rank, stopping my election")
              timers.cancelAll()
              buffer.unstashAll(inGameBehavior(gameCoordinator, playersStatus, hostRef))

            case (ctx, ElectionStarted(candidateRank, replyTo)) =>
              //another player is starting an election
              ctx.log.info(s"Election started by another player: $replyTo")
              if myRank < candidateRank then {
                //i have lower rank, so i can not accept the election
                logInfo(ctx, s"My rank ($myRank) is lower than sender rank ($candidateRank), refusing his election")
                replyTo ! NoYouCanNot()
                // reset the election to give time to the others to align
                buffer.unstashAll(inElectionBehavior(myRank))
              } else {
                logInfo(ctx, s"My rank ($myRank) is higher than sender rank ($candidateRank), accepting his election")
                timers.cancelAll()
                buffer.unstashAll(inGameBehavior(gameCoordinator, playersStatus, hostRef))
              }

            case (ctx, ElectionWon()) =>
              logInfo(ctx, s"I won the election, becoming the new host")
              otherPlayers.filter(p => p.isOnline).foreach(_.address ! NewHostElected(ctx.self))
              buffer.unstashAll(inGameBehavior(gameCoordinator, playersStatus, ctx.self))

            case (ctx, NewHostElected(replyTo)) =>
              //todo - should not happen, should start a new election?
              logInfo(ctx, s"New host elected while there was an election: $replyTo")
              timers.cancelAll()
              buffer.unstashAll(inGameBehavior(gameCoordinator, playersStatus, replyTo))

            case (ctx, other) =>
              logInfo(ctx, s"Stashing message during election: $other")
              buffer.stash(other)
              Behaviors.same
          })
        }
      }
    }


    //todo - receive GameCancelled if the host failed to synchronize with the other players
    //todo - initial phase when exchanging log about cards viewed
    withShared({
      // GAME LOGIC LEVEL MESSAGES - START

      // todo: aggiunti da Angelo fino che non è definito come far passare la fase di reveal delle carte - START
      case (ctx, RevealingCardsPhaseLog(log)) =>
        logInfo(ctx, s"Received ${RevealingCardsPhaseLog(log)}")
        howManyHaveWatchedCards += 1
        playersStatus.filter(l => !l.playerID.equals(this.userId) && l.isOnline).map(_.address).foreach(_ ! RevealingCardsPhaseForOtherClients(log))
        ctx.log.info(s"Number of players that have watched the cards: $howManyHaveWatchedCards")
        if howManyHaveWatchedCards == playersStatus.size then {
          ctx.log.info(s"All players have watched the cards, resetting counter and informing GameCoordinator to proceed")
          howManyHaveWatchedCards = 0
          gameCoordinator ! GameCoordinatorMessage.StartPlayCycle()
        }
        Behaviors.same

      case (ctx, RevealingCardsPhaseForOtherClients(log)) =>
        logInfo(ctx, s"Received ${RevealingCardsPhaseForOtherClients(log)}")
        viewActorRef ! DuringGameViewMessages.RevealingCardsPhaseAdversaryLog(log)
        //        if hostRef == ctx.self then
        howManyHaveWatchedCards += 1
        ctx.log.info(s"Number of players that have watched the cards: $howManyHaveWatchedCards")
        if howManyHaveWatchedCards == playersStatus.size then {
          ctx.log.info(s"All players have watched the cards, resetting counter and informing GameCoordinator to proceed")
          howManyHaveWatchedCards = 0
          gameCoordinator ! GameCoordinatorMessage.StartPlayCycle()
        }

        Behaviors.same
      // todo: aggiunti da Angelo fino che non è definito come far passare la fase di reveal delle carte - END


      case (ctx, TurnEnded(game, log)) =>
        ctx.log.info(s"My turn ended: ${this.userId}")
        playersStatus.filter(l => !l.playerID.equals(this.userId) && l.isOnline).map(_.address).foreach(_ ! GameInProgressUpdate(ctx.self, game, log))
        //todo - sync to all the players
        awaitSynchronization(ctx, playersStatus.filterNot(_.playerID.equals(this.userId)).map(_.playerID), () => {
          ctx.log.info(s"All players synchronized after my turn, ${this.userId}, waiting for my turn again: ${game.code}")
          if ctx.self equals hostRef then checkNextTurn(gameCoordinator, game, ctx)
          inGameBehavior(gameCoordinator, playersStatus, hostRef)
        }, () => {
          //todo - what to do if not all the players have synchronized?
          Behaviors.same
        })

      case (ctx, GameInProgressUpdate(replyTo, game, log)) =>
        //todo - update gameCoordinator
        //        ctx.log.info(s"Game info update: ${this.userId}")
        logInfo(ctx, s"Game info update")
        gameCoordinator ! NewTurn(game, log)
        //todo - sync to all the players, wait for gameCoordinator ack?
        withShared({
          case (ctx, TurnUpdated()) =>
            //            ctx.log.info(s"GameCoordinator updated the turn")
            logInfo(ctx, s"GameCoordinator updated the turn")
            replyTo ! SynchronizationAck(userId)
            inGameBehavior(gameCoordinator, playersStatus, hostRef)
        })

      case (ctx, LeaveTheGame()) =>
        //todo
        //        ctx.log.info(s"Leaving game, informing other players like I am unreachable")
        logInfo(ctx, s"Leaving game, informing other players like I am unreachable")
        otherPlayers.filter(_.isOnline).foreach(_.address ! PlayerUnreachable(PlayerInLobby(userId, name, ctx.self)))
        ctx.stop(gameCoordinator)
        connectionHandler ! ConnectionHandler.UpdateList(List())
        initialize(null)

      case (ctx, GameEnded()) =>
        ctx.log.info(s"Game has ended, returning to initial phase")
        connectionHandler ! ConnectionHandler.UpdateList(List())
        ctx.stop(gameCoordinator)
        initialize(null)

      // GAME LOGIC LEVEL MESSAGES - END

      case (ctx, PlayerUnreachable(playerInLobby)) =>
        //todo - host management

        playersStatus.find(_.playerID == playerInLobby.userID) match {
          case None =>
            //            ctx.log.warn(s"Received unreachable for unknown player: ${playerInLobby.userID}")
            logError(ctx, s"Received unreachable for unknown player: ${playerInLobby.userID}")
            inGameBehavior(gameCoordinator, playersStatus, hostRef)
          case Some(p) =>
            if !p.isOnline then
              //              ctx.log.info(s"Received unreachable for already offline player: ${p.playerID}")
              logInfo(ctx, s"Received unreachable for already offline player: ${p.playerID}")
              inGameBehavior(gameCoordinator, playersStatus, hostRef)
            else {
              ctx.log.info(s"Player: ${playerInLobby.userID} is unreachable")

              //todo - inform view

              //              otherPlayers.foreach(_.address ! PlayerUnreachable(playerInLobby))

              val onlineUpdate = playersStatus.map { ps =>
                if ps.playerID == playerInLobby.userID then
                  ps.copy(isOnline = false)
                else
                  ps
              }

              if p.address equals hostRef then {
                //                ctx.log.info(s"Player: ${playerInLobby.userID} was the host, starting election")
                logInfo(ctx, s"Player: ${playerInLobby.userID} was the host, starting election")
                //start election
                val myRank = playersStatus.find(_.playerID == userId).map(_.rank).getOrElse(-1)
                //                otherPlayers.filter(p => p.isOnline && p.rank < myRank).foreach(_.address ! ElectionStarted(myRank, ctx.self))
                onlineUpdate.filter(p => !p.playerID.equals(this.userId) && p.isOnline && p.rank < myRank).foreach(_.address ! ElectionStarted(myRank, ctx.self))
                Behaviors.withTimers(timer => {
                  timer.startSingleTimer(ElectionWon(), 5.seconds)
                  inElectionBehavior(myRank)
                  //                  Behaviors.receiveMessagePartial {
                  //                    case NoYouCanNot() =>
                  //                      logInfo(ctx, "Someone has a lower rank, stopping my election")
                  //                      timer.cancelAll()
                  //                      inGameBehavior(gameCoordinator, onlineUpdate, hostRef)
                  //                  }
                })
                //                  inGameBehavior(gameCoordinator, onlineUpdate, hostRef)
              } else
                inGameBehavior(gameCoordinator, onlineUpdate, hostRef)
            }

        }

      // ELECTION HOST LOGIC MESSAGES - START
      case (ctx, ElectionStarted(candidateRank, replyTo)) =>
        //another player is starting an election
        //        ctx.log.info(s"Election started by another player: ${replyTo}")
        logInfo(ctx, s"Election started by another player: ${replyTo}")
        val myRank = playersStatus.find(_.playerID == userId).map(_.rank).getOrElse(-1)
        if myRank < candidateRank then {
          //i have lower rank, so i can not accept the election
          //          ctx.log.info(s"My rank ($myRank) is lower than sender rank ($candidateRank), refusing election")
          logInfo(ctx, s"My rank ($myRank) is lower than sender rank ($candidateRank), refusing election")
          replyTo ! NoYouCanNot()
          // i start my own election
          otherPlayers.filter(p => p.isOnline && p.rank < myRank).foreach(_.address ! ElectionStarted(myRank, ctx.self))
          inElectionBehavior(myRank)
          //          Behaviors.withTimers(timer => {
          //            timer.startSingleTimer(ElectionWon(), 5.seconds)
          //            inGameBehavior(gameCoordinator, playersStatus, hostRef)
          //          })
        } else {
          inGameBehavior(gameCoordinator, playersStatus, hostRef)
        }

      //      case (ctx, ElectionWon()) =>
      //        //i won the election
      ////        ctx.log.info(s"I won the election, becoming the new host")
      //        logInfo(ctx, s"I won the election, becoming the new host")
      //        otherPlayers.filter(p => p.isOnline).foreach(_.address ! NewHostElected(ctx.self))
      //        inGameBehavior(gameCoordinator, playersStatus, ctx.self)
      //
      //      case (ctx, NewHostElected(replyTo)) =>
      ////        ctx.log.info(s"New host elected: $replyTo")
      //        logInfo(ctx, s"New host elected: $replyTo")
      //        inGameBehavior(gameCoordinator, playersStatus, replyTo)



      case (ctx, RemoveCheckPlayerStatus()) =>
        logInfo(ctx, s"Removing player status checking, clearing player list")
        connectionHandler ! ConnectionHandler.UpdateList(List())
        Behaviors.same
    })
  }