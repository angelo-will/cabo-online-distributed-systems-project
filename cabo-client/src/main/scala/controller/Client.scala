package controller
import akka.actor.typed.scaladsl.TimerScheduler
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.AskPattern.*
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, Scheduler}
import akka.cluster.ClusterEvent.MemberRemoved
import akka.util.Timeout
import controller.ViewsProxyActor.SwitchToInitialView
import messages.*
import messages.ClientMessages.*
import messages.GameCoordinatorMessage.WhoIsPlayingRequest
import messages.GameViewMessages.WaitAfterPreCycleSection
import messages.ServerMessages.{AbortGame, ServerKey}
import model.Game.{GameInConstruction, GameInProgress}
import model.{GameParameters, PlayerInLobby, PlayerPlaying, TurnLog}

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.Success

object Client:

  trait ClientInternalCommand extends ClientCommand

  // trait useful to check whether a message is for the correct game in the in-game phase or not
  trait GameScopedMessage {
    def gameCode: String
    def replyTo: ActorRef[ClientInternalCommand]
  }

  private case class ListingResponseListing(listing: Receptionist.Listing) extends ClientInternalCommand

  // Commands about the state before joining/creating a game
  case class IWantToPlay(newPlayer: PlayerInLobby, reply: ActorRef[Message]) extends ClientInternalCommand

  case class YouJoinedTheGame(game: GameInConstruction) extends ClientInternalCommand

  case class YouCanNotJoinTheGame(game: GameInConstruction) extends ClientInternalCommand

  case class FailedToContactHost() extends ClientInternalCommand

  case class UpdateAboutGame(game: GameInConstruction) extends ClientInternalCommand

  case class IWantToLeaveTheGame(gameCode: String, replyTo: ActorRef[Message], player: PlayerInLobby) extends ClientInternalCommand with GameScopedMessage

  case class GameCancelled(gameCode: String, replyTo: ActorRef[ClientInternalCommand]) extends ClientInternalCommand with GameScopedMessage

  case class GameHasStarted(hostRef: ActorRef[ClientCommand], gameInProgress: GameInProgress) extends ClientInternalCommand

  case class PlayerUnreachable(playerInLobby: PlayerInLobby) extends ClientInternalCommand

  //Commands after the game has started

  case class SynchronizationAck(fromWho: String) extends ClientInternalCommand

  case class FailedToSynchronize() extends ClientInternalCommand

  case class GameInProgressUpdate(replyTo: ActorRef[Message], game: GameInProgress, turnLog: TurnLog) extends ClientInternalCommand with GameScopedMessage {
    override def gameCode: String = game.code
  }

  // messages added for test purpose

  case class StartGameBehavior(thisBehavior: () => Behavior[IGameCoordinatorMessage], hostRef: ActorRef[ClientCommand]) extends ClientInternalCommand

  case class RemoveCheckPlayerStatus() extends ClientInternalCommand

  //messages for new host election

  case class ElectionStarted(gameCode: String, candidateRank: Int, replyTo: ActorRef[Message]) extends ClientInternalCommand with GameScopedMessage

  case class NoYouCanNot(gameCode: String, replyTo: ActorRef[Message]) extends ClientInternalCommand with GameScopedMessage

  case class ElectionWon() extends ClientInternalCommand

  case class NewHostElected(gameCode: String, replyTo: ActorRef[Message]) extends ClientInternalCommand with GameScopedMessage

  // messages for the pre-play cycle phase

  case class AdversaryLogInfo(log: TurnLog) extends ClientInternalCommand

  case class AllTheLogs(logs: List[TurnLog]) extends ClientInternalCommand

  //  case class PlayerStatus(playerID: String, address: ActorRef[ClientInternalCommand], rank: Int, isOnline: Boolean)
  case class PlayerStatus(playerInfo: PlayerInLobby, rank: Int, isOnline: Boolean, hasLeft: Boolean)

  def apply(userId: String = "Player", name: String = "defaultCoolName", optionalViewActor: ActorRef[IViewMessage] = null): Behavior[Message] = Behaviors.setup { ctx =>
    //    val clientID = userId+ctx.self.path.address.hashCode()
    val clientID = userId + UUID.randomUUID().hashCode()
    val viewActorRef = optionalViewActor match {
      case null => ctx.spawn(ViewsProxyActor(clientID, name, ctx.self), "views-manager")
      case ref => ref
    }
    val connectionHandler = ctx.spawn(ConnectionHandler[MemberRemoved](ctx.self), "ConnectionHandler")

    new Client(clientID, name, viewActorRef, connectionHandler).start
  }

private case class Client(userId: String, var name: String, viewActorRef: ActorRef[IViewMessage], connectionHandler: ActorRef[ConnectionHandler.InternalCommand]):

  import controller.Client.*

  private val syncMaxTime = 5
  private val electionTime = 5

  private case class ListingResponse(listing: Receptionist.Listing) extends Message

  private def logInfo(ctx: ActorContext[Message], msg: String): Unit = {
    ctx.log.info(s"Client[$userId]: $msg")
  }

  private def logError(ctx: ActorContext[Message], msg: String): Unit = {
    ctx.log.error(s"Client[$userId]: $msg")
  }

  private def logWarn(ctx: ActorContext[Message], msg: String): Unit = {
    ctx.log.warn(s"Client[$userId]: $msg")
  }

  private def contactInReceptionistAndAsk[T](key: ServiceKey[T])(whatToAskTo: ActorRef[T] => Unit)(ifFailure: () => Unit): Behavior[Message] = {
    Behaviors.setup { ctx =>
      val listingResponseAdapter = ctx.messageAdapter[Receptionist.Listing](ListingResponse.apply)

      ctx.system.receptionist ! Receptionist.find(key, listingResponseAdapter)

      Behaviors.receiveMessagePartial {
        case ListingResponse(key.Listing(listing)) =>
          if (listing.nonEmpty) {
            val contact = listing.head
            logInfo(ctx, s"Found required contact: $contact")
            whatToAskTo(contact)
          } else {
            logError(ctx, s"Contact with key ${key.id} not found")
            ifFailure()
          }
          Behaviors.stopped
      }
    }
  }

  private def askServerForGamesList(ctx: ActorContext[Message]) = {
    ctx.spawnAnonymous(contactInReceptionistAndAsk
      (ServerKey)
      (_ ! ServerMessages.GetGames(ctx.self))
      (() => viewActorRef ! PreGameViewMessages.FailedToPublishToServer()))
  }

  private def createPlayersStatus(playersInLobby: List[PlayerInLobby], playersPlaying: List[PlayerPlaying]): List[PlayerStatus] = {
    val idRank = playersPlaying.map(p => (p.userID, p.rank))

    val rankById = idRank.map(r => r._1 -> r).toMap

    playersInLobby.flatMap { a =>
      rankById.get(a.userID).map { r =>
        PlayerStatus(a, r._2, true, false)
      }
    }
  }

  private def awaitSynchronization(ctx: ActorContext[Message], usersToWait: List[String], nextBehavior: () => Behavior[Message], ifFailure: (failures: List[String]) => Behavior[Message]): Behavior[Message] = {
    logInfo(ctx, s"Awaiting synchronization from players: ${usersToWait.mkString(", ")}")
    Behaviors.withStash(100) { buffer =>
      Behaviors.withTimers { timers =>
        var checkSync = Map.empty[String, Boolean] ++ usersToWait.map(id => id -> false)
        timers.startSingleTimer(FailedToSynchronize(), syncMaxTime.seconds)
        Behaviors.receiveMessage {
          case FailedToSynchronize() =>
            logError(ctx, s"Failed to synchronize all players in time: ${checkSync.filterNot(_._2).keys.mkString(", ")} did not respond")
            timers.cancelAll()
            // call the failure behavior with the list of players who did not respond
            buffer.unstashAll(ifFailure(checkSync.filterNot(_._2).keys.toList))
          case SynchronizationAck(fromWho) =>
            logInfo(ctx, s"Synchronization ack received from: $fromWho")
            checkSync = checkSync.updatedWith(fromWho)({ case None => None case Some(v) => Some(true) })
            if checkSync.forall(_._2) then {
              logInfo(ctx, s"All players synchronized")
              timers.cancelAll()
              buffer.unstashAll(nextBehavior())
            } else {
              logInfo(ctx, s"Waiting for players to synchronize: ${checkSync.filterNot(_._2).keys.mkString(", ")}")
              // restart the timer so that the timeout is counted from the last received ack
              timers.startSingleTimer(FailedToSynchronize(), 5.seconds)
              Behaviors.same
            }
          case PlayerUnreachable(playerInLobby) =>
            // if a player is unreachable, we remove it from the list of players to wait for and then re-queue the message to be handled when the
            // synchronization is over by the nextBehavior
            logInfo(ctx, s"Player: ${playerInLobby.userID} is unreachable during synchronization")
            checkSync = checkSync.updatedWith(playerInLobby.userID)({ case None => None case Some(v) => Some(true) })
            ctx.self ! PlayerUnreachable(playerInLobby)
            Behaviors.same
          case other =>
            logInfo(ctx, s"Stashing message while waiting for synchronization: $other")
            buffer.stash(other)
            Behaviors.same
        }
      }
    }
  }

  private def sharedHandler: PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, GetPlayerInfo(replyTo)) =>
      logInfo(ctx, s"Sending player info to $replyTo")
      replyTo ! PlayerInfo(userId, name)
      Behaviors.same
  }

  private def withShared(
                          specific: PartialFunction[(ActorContext[Message], Message), Behavior[Message]],
                          behaviorName: String = ""
                        ): Behavior[Message] = {
    Behaviors.receivePartial(sharedHandler
      .orElse(specific)
      .orElse({
        case (ctx, msg) =>
          logWarn(ctx, s"In $behaviorName: Unhandled message: $msg")
          Behaviors.same
      })
    )
  }

  private def start: Behavior[Message] = Behaviors.setup { ctx =>

    withShared({
      case (ctx, CreateNewGame(makePublic, maxTimeRound, maxNumRound, maxPlayers, gameCode)) =>

        val player: PlayerInLobby = PlayerInLobby(userId, name, ctx.self)

        val game: GameInConstruction = GameInConstruction(gameCode.getOrElse(userId + "game"), GameParameters(makePublic, maxTimeRound, maxNumRound, maxPlayers), List(player))

        if makePublic then {
          ctx.spawnAnonymous(contactInReceptionistAndAsk
            (ServerKey)
            (_ ! ServerMessages.RegisterGame(game, ctx.self))
            (() => viewActorRef ! PreGameViewMessages.FailedToPublishToServer()))
        }

        ctx.system.receptionist ! Receptionist.register(akka.actor.typed.receptionist.ServiceKey[Message](game.code), ctx.self)

        connectionHandler ! ConnectionHandler.UpdateList(game.players)

        viewActorRef ! PreGameViewMessages.GameCreated(game)

        hostBehavior(game)

      case (ctx, JoinAGame()) =>
        logInfo(ctx, "Preparing to join a game")
        askServerForGamesList(ctx)
        joiningAGame

      case (ctx, ChangePlayerName(newName, replyTo)) =>
        logInfo(ctx, s"Changing player name from $name to $newName")
        this.name = newName
        replyTo ! PlayerInfo(userId, name)
        Behaviors.same
    })
  }

  private def hostBehavior(game: GameInConstruction): Behavior[Message] = {

    def updateServer(ctx: ActorContext[Message], gameUpdated: GameInConstruction) = {
      if gameUpdated.gameParameters.isPublic then
        ctx.spawnAnonymous(contactInReceptionistAndAsk
          (ServerKey)
          (_ ! ServerMessages.UpdateGame(gameUpdated, ctx.self))
          (() => viewActorRef ! PreGameViewMessages.FailedToPublishToServer()))
    }

    def removePlayerFromGame(ctx: ActorContext[Message], playerInLobby: PlayerInLobby) = {
      val gameUpdated = game.copy(players = game.players.filterNot(_.userID == playerInLobby.userID))
      updateServer(ctx, gameUpdated)
      gameUpdated.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! UpdateAboutGame(gameUpdated))
      connectionHandler ! ConnectionHandler.UpdateList(gameUpdated.players)
      viewActorRef ! PreGameViewMessages.GameInfoUpdate(gameUpdated)
      logInfo(ctx, s"Player: ${playerInLobby.userID} removed the game, now the players are: ${gameUpdated.players.map(_.userID).mkString(", ")}")
      hostBehavior(gameUpdated)
    }

    def returnToStart(ctx: ActorContext[Message]) = {
      game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameCancelled(game.code, ctx.self))
      viewActorRef ! PreGameViewMessages.GameAborted()
      connectionHandler ! ConnectionHandler.UpdateList(List())
      start
    }

    withShared({

      case (ctx, ServerMessages.GameRegistered(game, server)) =>
        //The server has registered the game
        logInfo(ctx, s"Game update: ${game.code} by server: $server")
        Behaviors.same

      case (ctx, ServerMessages.FailedToRegisterGame(game, server)) =>
        //The server has failed to register the game
        logError(ctx, s"Failed to register game: ${game.code}")
        viewActorRef ! PreGameViewMessages.FailedToPublishToServer()
        //Go into lobby
        Behaviors.same

      case (ctx, IWantToPlay(newPlayer: PlayerInLobby, replyTo: ActorRef[Message])) =>
        //The player wants to play
        logInfo(ctx, s"Player: ${newPlayer.userID} wants to play")
        if (game.players.size < game.gameParameters.maxPlayers) {
          //The player can join the game
          logInfo(ctx, s"Player: ${newPlayer.userID} can join the game: ${game.code}")
          val gameUpdated = game.copy(players = game.players :+ newPlayer)
          updateServer(ctx, gameUpdated)
          replyTo ! YouJoinedTheGame(gameUpdated)
          connectionHandler ! ConnectionHandler.UpdateList(gameUpdated.players)
          gameUpdated.players.filter(p => !p.address.equals(ctx.self) & !p.address.equals(newPlayer.address)).foreach(_.address ! UpdateAboutGame(gameUpdated))
          viewActorRef ! PreGameViewMessages.GameInfoUpdate(gameUpdated)
          hostBehavior(gameUpdated)
        } else {
          //The player cannot join the game
          logInfo(ctx, s"Player: $newPlayer cannot join the game: $game")
          replyTo ! YouCanNotJoinTheGame(game)
          Behaviors.same
        }

      case (ctx, IWantToLeaveTheGame(_, _, player)) =>
        //A player wants to leave the game
        logInfo(ctx, s"Player: ${player.userID} wants to leave the game: ${game.code}")
        removePlayerFromGame(ctx, player)

      case (ctx, PlayerUnreachable(playerInLobby)) =>
        //A player is unreachable
        logInfo(ctx, s"Player: ${playerInLobby.userID} is unreachable")
        removePlayerFromGame(ctx, playerInLobby)

      case (ctx, LeaveTheGame()) =>
        //The user wants to leave the game
        logInfo(ctx, s"Leaving ${game.code}, aborting game")
        if game.gameParameters.isPublic then
          ctx.spawnAnonymous(contactInReceptionistAndAsk
            (ServerKey)
            (_ ! ServerMessages.AbortGame(game, ctx.self))
            (() => viewActorRef ! PreGameViewMessages.FailedToPublishToServer()))
        ctx.system.receptionist ! Receptionist.deregister(akka.actor.typed.receptionist.ServiceKey[Message](game.code), ctx.self)
//        game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameCancelled(game.code, ctx.self))
//        viewActorRef ! PreGameViewMessages.GameAborted()
//        connectionHandler ! ConnectionHandler.UpdateList(List())
//        start
        returnToStart(ctx)

      case (ctx, StartTheGame()) =>
        //The game has started
        logInfo(ctx, s"Starting game: ${game.code}")

        if game.gameParameters.isPublic then
          ctx.spawnAnonymous(contactInReceptionistAndAsk
            (ServerKey)
            (_ ! ServerMessages.StartGame(game, ctx.self))
            (() => viewActorRef ! PreGameViewMessages.FailedToPublishToServer()))

        viewActorRef ! ViewsProxyActor.SwitchToGameView()
        ctx.self ! StartGameBehavior(() => GameCoordinatorActor(ctx.self, viewActorRef, userId, game), ctx.self)

        hostBehavior(game)

      case (ctx, StartGameBehavior(thisBehavior, hostRef)) =>

        logInfo(ctx, s"Starting game behavior for game: ${game.code}")

        ctx.system.receptionist ! Receptionist.deregister(akka.actor.typed.receptionist.ServiceKey[Message](game.code), ctx.self)

        val gameCoordinator = ctx.spawn(thisBehavior(), "GameCoordinatorActor")

//        hostWaitGameFromCoordinator(hostRef, game, gameCoordinator)

        Behaviors.withStash(10) { buffer =>
          Behaviors.receiveMessage {
            case TakeGetInProgressGame(gameInProgress) =>
              logInfo(ctx, s"Game in progress received: ${gameInProgress.code}")
              game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameHasStarted(hostRef, gameInProgress))
              buffer.unstashAll(awaitSynchronization(ctx, game.players.filter(!_.address.equals(ctx.self)).map(_.userID), () => {
                logInfo(ctx, s"All players synchronized, starting the game: ${gameInProgress.code}")
                gameCoordinator ! GameCoordinatorMessage.StartPrePlayCycleSection()
                prePlayCyclePhase(gameInProgress.code, gameCoordinator, createPlayersStatus(game.players, gameInProgress.players), hostRef)
              }, _ => {
                //If failed to synchronize
                //Brutal policy, we abort the game
                logError(ctx, s"Failed to synchronize all players, aborting the game: ${gameInProgress.code}")
                ctx.stop(gameCoordinator)
//                game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameCancelled(gameInProgress.code, ctx.self))
//                viewActorRef ! PreGameViewMessages.GameAborted()
//                connectionHandler ! ConnectionHandler.UpdateList(List())
                returnToStart(ctx)
              }))
            case other =>
              logInfo(ctx, s"Stashing message while waiting for game in progress: $other")
              buffer.stash(other)
              Behaviors.same
          }
        }
    })
  }

//  private def hostWaitGameFromCoordinator(hostRef: ActorRef[ClientCommand], game: GameInConstruction, gameCoordinator: ActorRef[IGameCoordinatorMessage]): Behavior[Message] = {
//    Behaviors.receivePartial {
//      case (ctx, TakeGetInProgressGame(gameInProgress)) =>
//        logInfo(ctx, s"Game in progress received: ${gameInProgress.code}")
//        game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameHasStarted(hostRef, gameInProgress))
//        awaitSynchronization(ctx, game.players.filter(!_.address.equals(ctx.self)).map(_.userID), () => {
//          logInfo(ctx, s"All players synchronized, starting the game: ${gameInProgress.code}")
//          gameCoordinator ! GameCoordinatorMessage.StartPrePlayCycleSection()
//          prePlayCyclePhase(gameInProgress.code, gameCoordinator, createPlayersStatus(game.players, gameInProgress.players), hostRef)
//        }, _ => {
//          //If failed to synchronize
//          //Brutal policy, we abort the game
//          logError(ctx, s"Failed to synchronize all players, aborting the game: ${gameInProgress.code}")
//          ctx.stop(gameCoordinator)
//          game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameCancelled(gameInProgress.code, ctx.self))
//          viewActorRef ! PreGameViewMessages.GameAborted()
//          connectionHandler ! ConnectionHandler.UpdateList(List())
//          start
//        })
//    }
//  }

  private def joiningAGame: Behavior[Message] = {

    def responseForJoining(gameCode: String): Behavior[Message] = {
      Behaviors.withTimers { timers =>
        timers.startSingleTimer(FailedToContactHost(), 60.seconds)
        withShared({
          case (ctx, YouJoinedTheGame(game)) =>
            logInfo(ctx, s"Joined game: $game")
            connectionHandler ! ConnectionHandler.UpdateList(List(game.players.head))
            viewActorRef ! PreGameViewMessages.GameJoined(game)
            //Joined a game
            timers.cancelAll()
            gameJoined(game)

          case (ctx, YouCanNotJoinTheGame(game)) =>
            logInfo(ctx, "Could not join game")
            viewActorRef ! PreGameViewMessages.GameJoinedFailed(gameCode)
            //Failed to join, waiting for other commands from the user
            timers.cancelAll()
            joiningAGame

          case (ctx, FailedToContactHost()) =>
            logInfo(ctx, "Failed to contact host")
            viewActorRef ! PreGameViewMessages.GameJoinedFailed(gameCode)
            timers.cancelAll()
            joiningAGame
        })
      }
    }

    withShared({

      case (ctx, ServerMessages.GamesList(games)) =>
        if games.nonEmpty then {
          logInfo(ctx, s"Games found: $games")
          viewActorRef ! PreGameViewMessages.GameList(games.toList)
        } else {
          logInfo(ctx, "No games found")
          viewActorRef ! PreGameViewMessages.GameList(List())
        }
        Behaviors.same

      case (ctx, JoinWithGameCode(gameCode)) =>

        ctx.spawnAnonymous(contactInReceptionistAndAsk
          (akka.actor.typed.receptionist.ServiceKey[Message](gameCode))
          (_ ! IWantToPlay(PlayerInLobby(userId, name, ctx.self), ctx.self))
          (() => ctx.self ! FailedToContactHost())
        )

        responseForJoining(gameCode)

      case (ctx, JoinGame(game)) =>
        logInfo(ctx, s"Trying to join game: ${game.code}")
        game.players.head.address ! IWantToPlay(PlayerInLobby(userId, name, ctx.self), ctx.self)
        responseForJoining(game.code)

      case (ctx, JoinAGame()) =>
        askServerForGamesList(ctx)
        Behaviors.same

      case (ctx, ReturnToStart()) =>
        logInfo(ctx, "[joiningAGame] - Returning to start")
        start
    })
  }

  private def gameJoined(game: GameInConstruction): Behavior[Message] = {

    def returnToStart(ctx: ActorContext[Message]) = {
      logInfo(ctx, s"Game: ${game.code} has been aborted")
      viewActorRef ! PreGameViewMessages.GameAborted()
      connectionHandler ! ConnectionHandler.UpdateList(List())
      start
    }

    withShared({

      case (ctx, UpdateAboutGame(game)) =>
        logInfo(ctx, s"Game info update: ${game.code}")
        viewActorRef ! PreGameViewMessages.GameInfoUpdate(game)
        gameJoined(game)

      case (ctx, LeaveTheGame()) =>
        logInfo(ctx, s"Leaving game: ${game.code}")
        game.players.head.address ! IWantToLeaveTheGame(game.code, ctx.self, PlayerInLobby(userId, name, ctx.self))
        connectionHandler ! ConnectionHandler.UpdateList(List())
        start

      case (ctx, GameCancelled(_, _)) =>
        returnToStart(ctx)

      case (ctx, PlayerUnreachable(playerInLobby)) =>
        returnToStart(ctx)

      case (ctx, GameHasStarted(hostRef, gameInProgress)) =>
        logInfo(ctx, s"Game has started: ${game.code}")
        hostRef ! SynchronizationAck(userId)

        viewActorRef ! PreGameViewMessages.GameStarted()
        viewActorRef ! ViewsProxyActor.SwitchToGameView()
        val gameCoordinator = ctx.spawn(GameCoordinatorActor(ctx.self, viewActorRef, userId, gameInProgress), "GameCoordinatorActor")
        gameCoordinator ! GameCoordinatorMessage.StartPrePlayCycleSection()
        connectionHandler ! ConnectionHandler.UpdateList(game.players)
        prePlayCyclePhase(gameInProgress.code, gameCoordinator, createPlayersStatus(game.players, gameInProgress.players), hostRef)
    })
  }

  private def prePlayCyclePhase(gameCode: String, gameCoordinator: ActorRef[IGameCoordinatorMessage], playersStatus: List[PlayerStatus], hostRef: ActorRef[ClientCommand]): Behavior[Message] = {

    def otherPlayersOnline = playersStatus.filterNot(p => !p.isOnline || p.playerInfo.userID.equals(this.userId))

    val MaxTimeoutPrePhase = 30.seconds

    var phaseLogs: List[TurnLog] = List()

    def gameFailurePolicy(ctx: ActorContext[Message]): Behavior[Message] = {
      otherPlayersOnline.map(_.playerInfo.address).foreach(_ ! GameCancelled(gameCode, ctx.self))
      viewActorRef ! GameViewMessages.GameDeleted()
      // waiting for view to process the deletion
      Behaviors.receivePartial {
        case (ctx, LeaveTheGame()) =>
          returnToStart(ctx, gameCoordinator)
      }
    }

    def hostCheckIfReady(ctx: ActorContext[Message], log: TurnLog, timers: TimerScheduler[Message]): Behavior[Message] = {
      phaseLogs = phaseLogs :+ log

      if phaseLogs.size == playersStatus.size then {
        timers.cancelAll()
        logInfo(ctx, s"All players have sent their logs, informing other players")
        // informing view of the logs
        phaseLogs.foreach(viewActorRef ! GameViewMessages.PreCyclePhaseAdversaryLog(_))
        // sending all the logs to other players
        otherPlayersOnline.map(_.playerInfo.address).foreach(_ ! AllTheLogs(phaseLogs))
        // synchronizing all players
        awaitSynchronization(ctx, otherPlayersOnline.map(_.playerInfo.userID), () => {
          logInfo(ctx, s"All players synchronized after revealing cards phase, proceeding to game start")
          gameCoordinator ! GameCoordinatorMessage.StartPlayCycle()
          inGameBehavior(gameCode, gameCoordinator, playersStatus, hostRef)
        }, _ => {
          //If failed to synchronize
          //Brutal policy, we abort the game
          logInfo(ctx, s"Failed to synchronize all players after revealing cards phase, aborting the game")
          gameFailurePolicy(ctx)
        })
      }
      else {
        Behaviors.same
      }
    }

    Behaviors.withTimers { timers =>
      timers.startSingleTimer(GameCancelled(gameCode, hostRef), MaxTimeoutPrePhase)
      withShared({
        case (ctx, InitialPhaseCompleted(log)) =>
          logInfo(ctx, s"Received ${InitialPhaseCompleted(log)}")
          viewActorRef ! WaitAfterPreCycleSection()
          if ctx.self equals hostRef then
            hostCheckIfReady(ctx, log, timers)
          else {
            hostRef ! AdversaryLogInfo(log)
            Behaviors.same
          }

        case (ctx, AllTheLogs(logs)) =>
          logInfo(ctx, s"Received all the logs from host")
          timers.cancelAll()
          logs.foreach(viewActorRef ! GameViewMessages.PreCyclePhaseAdversaryLog(_))
          hostRef ! SynchronizationAck(userId)
          gameCoordinator ! GameCoordinatorMessage.StartPlayCycle()
          inGameBehavior(gameCode, gameCoordinator, playersStatus, hostRef)

        case (ctx, AdversaryLogInfo(log)) =>
          logInfo(ctx, s"Received ${AdversaryLogInfo(log)}")
          hostCheckIfReady (ctx, log, timers)

        case (ctx, LeaveTheGame()) =>
          logInfo(ctx, s"Leaving game: $gameCode during pre-play cycle phase")
          timers.cancelAll()
          returnToStart(ctx,  gameCoordinator)

        case (ctx, PlayerUnreachable(_)) =>
          logInfo(ctx, s"A player is unreachable, aborting the game")
          timers.cancelAll()
          gameFailurePolicy(ctx)

        case (ctx, GameCancelled(_, _)) =>
          logInfo(ctx, s"Game has been cancelled, returning to initial phase")
          timers.cancelAll()
          gameFailurePolicy(ctx)
      }, "preGamePhase")
    }
  }

//  private def prePlayCyclePhaseHost(gameCode: String, gameCoordinator: ActorRef[IGameCoordinatorMessage], playersStatus: List[PlayerStatus], hostRef: ActorRef[ClientInternalCommand]): Behavior[Message] = {
//
//    def otherPlayersOnline = playersStatus.filterNot(p => !p.isOnline || p.playerInfo.userID.equals(this.userId))
//
//    def gameFailurePolicy(ctx: ActorContext[Message]): Behavior[Message] = {
//      otherPlayersOnline.map(_.playerInfo.address).foreach(_ ! GameCancelled(gameCode, ctx.self))
//      viewActorRef ! GameViewMessages.GameDeleted()
//      // waiting for view to process the deletion
//      Behaviors.receivePartial {
//        case (ctx, LeaveTheGame()) =>
//          returnToStart(ctx, gameCoordinator)
//      }
//    }
//
//    var phaseLogs: List[TurnLog] = List()
//
//    def hostCheckIfReady(ctx: ActorContext[Message], log: TurnLog): Behavior[Message] = {
//      phaseLogs = phaseLogs :+ log
//
//      if phaseLogs.size == playersStatus.size then {
//        logInfo(ctx, s"All players have sent their logs, informing other players")
//        // informing view of the logs
//        phaseLogs.foreach(viewActorRef ! GameViewMessages.PreCyclePhaseAdversaryLog(_))
//        // sending all the logs to other players
//        otherPlayersOnline.map(_.playerInfo.address).foreach(_ ! AllTheLogs(phaseLogs))
//        // synchronizing all players
//        awaitSynchronization(ctx, otherPlayersOnline.map(_.playerInfo.userID), () => {
//          logInfo(ctx, s"All players synchronized after revealing cards phase, proceeding to game start")
//          gameCoordinator ! GameCoordinatorMessage.StartPlayCycle()
//          inGameBehavior(gameCode, gameCoordinator, playersStatus, hostRef)
//        }, _ => {
//          //If failed to synchronize
//          //Brutal policy, we abort the game
//          logInfo(ctx, s"Failed to synchronize all players after revealing cards phase, aborting the game")
//          gameFailurePolicy(ctx)
//        })
//      }
//      else {
//        Behaviors.same
//      }
//    }
//
//    Behaviors.withTimers { timers =>
//      timers.startSingleTimer(GameCancelled(gameCode, hostRef), 120.seconds)
//      withShared({
//        case (ctx, InitialPhaseCompleted(log)) =>
//          logInfo(ctx, s"Received ${InitialPhaseCompleted(log)}")
//          viewActorRef ! WaitAfterPreCycleSection()
//          hostCheckIfReady(ctx, log)
//
//        case (ctx, AdversaryLogInfo(log)) =>
//          logInfo(ctx, s"Received ${AdversaryLogInfo(log)}")
//          hostCheckIfReady (ctx, log)
//
//        case (ctx, PlayerUnreachable(_)) =>
//          logInfo(ctx, s"A player is unreachable, aborting the game")
//          gameFailurePolicy(ctx)
//
//        case (ctx, GameCancelled(_, _)) =>
//          logInfo(ctx, s"Game has been cancelled, returning to initial phase")
//          gameFailurePolicy(ctx)
//      })
//    }
//  }
//
//  private def prePlayCyclePhaseJoined(gameCode: String, gameCoordinator: ActorRef[IGameCoordinatorMessage], playersStatus: List[PlayerStatus], hostRef: ActorRef[ClientCommand]): Behavior[Message] = {
//
//    def otherPlayersOnline = playersStatus.filterNot(p => !p.isOnline || p.playerInfo.userID.equals(this.userId))
//
//    def waitToReturnToStart(ctx: ActorContext[Message]): Behavior[Message] = {
//      Behaviors.receivePartial {
//        case (ctx, LeaveTheGame()) =>
//          returnToStart(ctx, gameCoordinator)
//      }
//    }
//
//    withShared({
//      case (ctx, InitialPhaseCompleted(log)) =>
//        logInfo(ctx, s"Received ${InitialPhaseCompleted(log)}")
//        viewActorRef ! WaitAfterPreCycleSection()
//        hostRef ! AdversaryLogInfo(log)
//        Behaviors.same
//
//      case (ctx, AllTheLogs(logs)) =>
//        logInfo(ctx, s"Received all the logs from host")
//        logs.foreach(viewActorRef ! GameViewMessages.PreCyclePhaseAdversaryLog(_))
//        hostRef ! SynchronizationAck(userId)
//        gameCoordinator ! GameCoordinatorMessage.StartPlayCycle()
//        inGameBehavior(gameCode, gameCoordinator, playersStatus, hostRef)
//
//      case (ctx, PlayerUnreachable(_)) =>
//        logInfo(ctx, s"A player is unreachable, aborting the game")
//        // similar to host, but separated because it is a different behavior
//        otherPlayersOnline.map(_.playerInfo.address).foreach(_ ! GameCancelled(gameCode, ctx.self))
//        viewActorRef ! GameViewMessages.GameDeleted()
//        waitToReturnToStart(ctx)
//
//      case (ctx, GameCancelled(_, _)) =>
//        logInfo(ctx, s"Game has been cancelled, returning to initial phase")
//        viewActorRef ! GameViewMessages.GameDeleted()
//        waitToReturnToStart(ctx)
//
//    }, "preGamePhase")
//  }

  private def returnToStart[T](ctx: ActorContext[Message], toStop: ActorRef[T]) = {
    ctx.stop(toStop)
    connectionHandler ! ConnectionHandler.UpdateList(List())
    viewActorRef ! ViewsProxyActor.SwitchToInitialView(name)
    start
  }

  private def inGameBehavior(gameCode: String, gameCoordinator: ActorRef[IGameCoordinatorMessage], playersStatus: List[PlayerStatus], hostRef: ActorRef[ClientInternalCommand]): Behavior[Message] = {

    def otherPlayersInGame(players: List[PlayerStatus] = playersStatus) = players.filterNot(p => p.playerInfo.userID.equals(this.userId) || p.hasLeft)

    def getMyRank(players: List[PlayerStatus] = playersStatus) = players.find(_.playerInfo.userID == userId).map(_.rank)

    def AskCoordinatorNextPlayer(ctx: ActorContext[Message]): Unit = {
      given timeout: Timeout = 3.seconds
      given scheduler: Scheduler = ctx.system.scheduler
      given ec: ExecutionContext = ctx.executionContext

      // use pipeToSelf to be sure to manage the response after the awaitSynchronization messages
      ctx.pipeToSelf(
        gameCoordinator.ask[WhoIsPlaying](WhoIsPlayingRequest(_))
      ) {
        case Success(value) => value
        case _ => WhoIsPlaying("error")
      }
    }

    // used by the host to check if who has the next turn is online, if not, it will skip the turn
    def checkPlayerForTheTurn(id: String, ctx: ActorContext[Message]): Unit = {
      playersStatus.find(_.playerInfo.userID equals id) match {
        case Some(value) =>
          if (!value.isOnline || value.hasLeft) {
            logInfo(ctx, s"Turn for offline player: ${value.playerInfo.userID}, skipping turn")
            gameCoordinator ! GameCoordinatorMessage.GetEmptyTurn(value.playerInfo.userID)
          } else {
            logInfo(ctx, s"Next turn is for player: ${value.playerInfo.userID} and it is online")
          }
        case None =>
          //should not happen
          logError(ctx, s"Could not find player for $id")
      }
    }

    def verifyGameCode[M <: GameScopedMessage](ctx: ActorContext[Message], msg: M)(ifCorrect: => Behavior[Message]): Behavior[Message] = {
      if (msg.gameCode != gameCode) {
        logError(ctx, s"Received message for wrong game: ${msg.gameCode}")
        msg.replyTo ! IWantToLeaveTheGame(gameCode, ctx.self, PlayerInLobby(userId, name, ctx.self))
        Behaviors.same
      } else {
        ifCorrect
      }
    }


    def inElectionBehavior(myRank: Int, players: List[PlayerStatus]): Behavior[Message] = {
      Behaviors.withStash(50) { buffer =>
        Behaviors.withTimers { timers =>
          timers.startSingleTimer(ElectionWon(), electionTime.seconds)
          withShared({
            case (ctx, msg: NoYouCanNot) =>
              verifyGameCode(ctx, msg) {
                logInfo(ctx, "Someone has a lower rank, stopping my election")
                timers.cancelAll()
                buffer.unstashAll(inGameBehavior(gameCode, gameCoordinator, players, hostRef))
              }

            case (ctx, ElectionStarted(code, candidateRank, replyTo)) =>
              //another player is starting an election
              verifyGameCode(ctx, ElectionStarted(code, candidateRank, replyTo)) {
                logInfo(ctx, s"Election started by another player: $replyTo")
                if myRank < candidateRank then {
                  //i have lower rank, so i can not accept the election
                  logInfo(ctx, s"My rank ($myRank) is lower than sender rank ($candidateRank), refusing his election")
                  replyTo ! NoYouCanNot(gameCode, ctx.self)
                  // reset the election to give time to the others to align
                  buffer.unstashAll(inElectionBehavior(myRank, players))
                } else {
                  logInfo(ctx, s"My rank ($myRank) is higher than sender rank ($candidateRank), accepting his election")
                  timers.cancelAll()
                  buffer.unstashAll(inGameBehavior(gameCode, gameCoordinator, players, hostRef))
                }
              }

            case (ctx, ElectionWon()) =>
              logInfo(ctx, s"I won the election, becoming the new host")
              otherPlayersInGame(players).foreach(_.playerInfo.address ! NewHostElected(gameCode, ctx.self))
              AskCoordinatorNextPlayer(ctx)
              buffer.unstashAll(inGameBehavior(gameCode, gameCoordinator, players, ctx.self))

            case (ctx, NewHostElected(code, replyTo)) =>
              verifyGameCode(ctx, NewHostElected(code, replyTo)) {
                logInfo(ctx, s"New host elected while there was an election: $replyTo")
                timers.cancelAll()
                buffer.unstashAll(inGameBehavior(gameCode, gameCoordinator, players, replyTo))
              }

            case (ctx, other) =>
              logInfo(ctx, s"Stashing message during election: $other")
              buffer.stash(other)
              Behaviors.same
          })
        }
      }
    }

    def checkContinue(ctx: ActorContext[Message], playerInLobby: PlayerInLobby, onlineUpdate: List[PlayerStatus]) = {
      if onlineUpdate.count(p => p.isOnline && !p.hasLeft) < 2 then {
        logInfo(ctx, s"Less than 2 players online, aborting the game")
        viewActorRef ! GameViewMessages.AllOpponentsDisconnected()
        inGameBehavior(gameCode, gameCoordinator, onlineUpdate, hostRef)
      } else {
        viewActorRef ! GameViewMessages.OpponentDisconnected(playerInLobby)

        if playerInLobby.address equals hostRef then {
          logInfo(ctx, s"Player: ${playerInLobby.userID} was the host, starting election")
          //start election
          getMyRank() match {
            case None => // should not happen
              logError(ctx, s"Could not find my rank")
              inGameBehavior(gameCode, gameCoordinator, onlineUpdate, hostRef)
            case Some(myRank) =>
              onlineUpdate.filter(p => !p.playerInfo.userID.equals(this.userId) && p.isOnline && p.rank < myRank).foreach(_.playerInfo.address ! ElectionStarted(gameCode, myRank, ctx.self))
              inElectionBehavior(myRank, onlineUpdate)
          }
        } else {
          if ctx.self equals hostRef then AskCoordinatorNextPlayer(ctx)
          inGameBehavior(gameCode, gameCoordinator, onlineUpdate, hostRef)
        }
      }
    }

    withShared({
      // GAME LOGIC LEVEL MESSAGES - START

      // send by the host when it cannot synchronize all the players at the start of the game
      case (ctx, msg: GameCancelled) =>
        verifyGameCode(ctx, msg) {
          logInfo(ctx, s"Game has been cancelled, returning to initial phase")
          viewActorRef ! GameViewMessages.GameDeleted()
          returnToStart(ctx, gameCoordinator)
        }

      case (ctx, TurnEnded(game, log)) =>
        logInfo(ctx, s"My turn ended: ${this.userId}, round: ${game.currentRound}")
        otherPlayersInGame().map(_.playerInfo.address).foreach(_ ! GameInProgressUpdate(ctx.self, game, log))

        awaitSynchronization(ctx, otherPlayersInGame().map(_.playerInfo.userID),
          () => {
            logInfo(ctx, s"All players synchronized after my turn, ${this.userId}, waiting for my turn again: ${game.code}")
            AskCoordinatorNextPlayer(ctx)
            // if all players synchronized, set all to online, because now for me they are all reachable
            inGameBehavior(gameCode, gameCoordinator, playersStatus.map(_.copy(isOnline = true)), hostRef)
          },
          failures => {
            logError(ctx, s"Some Players failed to synchronise: ${failures.mkString(", ")}, retrying for them")
            val onlineUpdate = playersStatus.collect {
              case ps if failures.contains(ps.playerInfo.userID) =>
                ps.copy(isOnline = false)
              case ps => ps
            }
            AskCoordinatorNextPlayer(ctx)
            inGameBehavior(gameCode, gameCoordinator, onlineUpdate, hostRef)
        })

      case (ctx, GameInProgressUpdate(replyTo, game, log)) =>
        verifyGameCode(ctx, GameInProgressUpdate(replyTo, game, log)) {
          logInfo(ctx, s"Game info update, is turn: ${game.currentRound}")
          gameCoordinator ! GameCoordinatorMessage.LastTurnPlayed(game, log)
          Behaviors.withStash(50) { buffer =>
            withShared({
              case (ctx, TurnUpdated()) =>
                logInfo(ctx, s"GameCoordinator updated the turn")
                replyTo ! SynchronizationAck(userId)
                buffer.unstashAll(inGameBehavior(gameCode, gameCoordinator, playersStatus, hostRef))

              case (ctx, other) =>
                logInfo(ctx, s"Stashing message until turn is updated: $other")
                buffer.stash(other)
                Behaviors.same
            })
          }
        }

      case (ctx, LeaveTheGame()) =>
        logInfo(ctx, s"Leaving game normally")
        otherPlayersInGame().foreach(_.playerInfo.address ! IWantToLeaveTheGame(gameCode, ctx.self, PlayerInLobby(userId, name, ctx.self)))
        returnToStart(ctx, gameCoordinator)

      case (ctx, IWantToLeaveTheGame(code, replyTo, player)) =>
        otherPlayersInGame().find(_.playerInfo.userID == player.userID) match {
          case None =>
            logError(ctx, s"Received leave request for unknown player: ${player.userID}")
            inGameBehavior(code, gameCoordinator, playersStatus, hostRef)
          case Some(p) =>
            if p.hasLeft then {
              logInfo(ctx, s"Received leave request for already left player: ${p.playerInfo.userID}")
              inGameBehavior(code, gameCoordinator, playersStatus, hostRef)
            } else {
              logInfo(ctx, s"Player: ${player.userID} wants to leave the game")
              val updatedStatus = playersStatus.map { ps =>
                if ps.playerInfo.userID == player.userID then
                  ps.copy(hasLeft = true, isOnline = false)
                else
                  ps
              }
              // rebound the message to increase the consistency of the state among players
              updatedStatus.filterNot(p => p.playerInfo.userID.equals(userId) || p.hasLeft).foreach(_.playerInfo.address ! IWantToLeaveTheGame(gameCode, ctx.self, player))
              // I no longer need to check the status of a player who has left
              connectionHandler ! ConnectionHandler.UpdateList(updatedStatus.filterNot(_.hasLeft).map(_.playerInfo))
              checkContinue(ctx, player, updatedStatus)
            }
        }

      case (ctx, GameEnded()) =>
        logInfo(ctx, s"Game has ended, returning to initial phase")
        returnToStart(ctx, gameCoordinator)

      // GAME LOGIC LEVEL MESSAGES - END

      case (ctx, PlayerUnreachable(playerInLobby)) =>
        playersStatus.find(_.playerInfo.userID == playerInLobby.userID) match {
          case None =>
            logError(ctx, s"Received unreachable for unknown player: ${playerInLobby.userID}")
            inGameBehavior(gameCode, gameCoordinator, playersStatus, hostRef)
          case Some(p) =>
            if !p.isOnline then
              logInfo(ctx, s"Received unreachable for already offline player: ${p.playerInfo.userID}")
              inGameBehavior(gameCode, gameCoordinator, playersStatus, hostRef)
            else {
              logInfo(ctx, s"Player: ${playerInLobby.userID} is unreachable")

              val onlineUpdate = playersStatus.map { ps =>
                if ps.playerInfo.userID == playerInLobby.userID then
                  ps.copy(isOnline = false)
                else
                  ps
              }

              checkContinue(ctx, playerInLobby, onlineUpdate)
            }
        }

      // ELECTION HOST LOGIC MESSAGES - START
      case (ctx, ElectionStarted(code, candidateRank, replyTo)) =>
        verifyGameCode(ctx, ElectionStarted(code, candidateRank, replyTo)) {
          //another player is starting an election
          logInfo(ctx, s"Election started by another player: $replyTo")
          getMyRank() match {
            case None => // should not happen
              logError(ctx, s"Could not find my rank")
              inGameBehavior(gameCode, gameCoordinator, playersStatus, hostRef)
            case Some(myRank) =>
              if myRank < candidateRank then {
                //I have lower rank, so I cannot accept the election
                logInfo(ctx, s"My rank ($myRank) is lower than sender rank ($candidateRank), refusing election")
                replyTo ! NoYouCanNot(gameCode, ctx.self)
                // I start my own election
                otherPlayersInGame().filter(_.rank < myRank).foreach(_.playerInfo.address ! ElectionStarted(gameCode, myRank, ctx.self))
                val onlineUpdate = playersStatus.map { ps =>
                  if ps.playerInfo.address equals hostRef then
                    ps.copy(isOnline = false)
                  else
                    ps
                }
                inElectionBehavior(myRank, onlineUpdate)
              } else {
                inGameBehavior(gameCode, gameCoordinator, playersStatus, hostRef)
              }
          }
        }

      case (ctx, NewHostElected(gameCode, replyTo)) =>
        verifyGameCode(ctx, NewHostElected(gameCode, replyTo)) {
          logInfo(ctx, "New host elected: " + replyTo)
          inGameBehavior(gameCode, gameCoordinator, playersStatus, replyTo)
        }

      case (ctx, WhoIsPlaying(currentPlayerID)) =>
        logInfo(ctx, s"Who is playing request response, current player is: $currentPlayerID")
        checkPlayerForTheTurn(currentPlayerID, ctx)
        Behaviors.same

      // message for test purpose
      case (ctx, RemoveCheckPlayerStatus()) =>
        logInfo(ctx, s"Removing player status checking, clearing player list")
        connectionHandler ! ConnectionHandler.UpdateList(List())
        Behaviors.same
    })
  }