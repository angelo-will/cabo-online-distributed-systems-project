package controller

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.AskPattern.*
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, Scheduler}
import akka.cluster.ClusterEvent.MemberExited
import akka.util.Timeout
import messages.*
import messages.ClientMessages.*
import messages.GameCoordinatorMessage.WhoIsPlayingRequest
import messages.ServerMessages.{AbortGame, ServerKey}
import model.Game.{GameInConstruction, GameInProgress}
import model.{GameParameters, PlayerInLobby, PlayerPlaying, TurnLog}

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.Success

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

  case class GameHasStarted(hostRef: ActorRef[ClientCommand], gameInProgress: GameInProgress) extends ClientInternalCommand

  case class PlayerUnreachable(playerInLobby: PlayerInLobby) extends ClientInternalCommand

  //Commands after the game has started

  case class SynchronizationAck(fromWho: String) extends ClientInternalCommand

  case class FailedToSynchronize() extends ClientInternalCommand

  case class GameInProgressUpdate(replyTo: ActorRef[ClientInternalCommand], game: GameInProgress, turnLog: TurnLog) extends ClientInternalCommand

  // messages added for test purpose

  case class StartGameBehavior(thisBehavior: () => Behavior[IGameCoordinatorMessage], hostRef: ActorRef[ClientCommand]) extends ClientInternalCommand

  case class RemoveCheckPlayerStatus() extends ClientInternalCommand

  //messages for new host election

  case class ElectionStarted(candidateRank: Int, replyTo: ActorRef[ClientInternalCommand]) extends ClientInternalCommand

  case class NoYouCanNot() extends ClientInternalCommand

  case class ElectionWon() extends ClientInternalCommand

  case class NewHostElected(replyTo: ActorRef[ClientInternalCommand]) extends ClientInternalCommand

  case class AllTheLogs(logs: List[TurnLog]) extends ClientInternalCommand

  //  case class PlayerStatus(playerID: String, address: ActorRef[ClientInternalCommand], rank: Int, isOnline: Boolean)
  case class PlayerStatus(playerInfo: PlayerInLobby, rank: Int, isOnline: Boolean)

  def apply(userId: String = "Player", name: String = "defaultCoolName", optionalViewActor: ActorRef[IViewMessage] = null): Behavior[Message] = Behaviors.setup { ctx =>
    //    val clientID = userId+ctx.self.path.address.hashCode()
    val clientID = userId + UUID.randomUUID().hashCode()
    val viewActorRef = optionalViewActor match {
      case null => ctx.spawn(ViewsProxyActor(clientID, name, ctx.self), "views-manager")
      case ref => ref
    }
    val connectionHandler = ctx.spawn(ConnectionHandler[MemberExited](ctx.self), "ConnectionHandler")

    new Client(clientID, name, viewActorRef, connectionHandler).start
  }

private case class Client(userId: String, var name: String, viewActorRef: ActorRef[IViewMessage], connectionHandler: ActorRef[ConnectionHandler.InternalCommand]):

  import controller.Client.*

  private val syncMaxTime = 5

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
                          whereAmI: String = ""
                        ): Behavior[Message] = {
    Behaviors.receivePartial(sharedHandler
      .orElse(specific)
      .orElse({
        case (ctx, msg) =>
          logWarn(ctx, s"In $whereAmI Unhandled message in Client actor: $msg")
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
        ctx.spawnAnonymous(contactInReceptionistAndAsk
          (ServerKey)
          (_ ! ServerMessages.GetGames(ctx.self))
          (() => viewActorRef ! PreGameViewMessages.FailedToPublishToServer()))
        joiningAGame

      case (ctx, ChangePlayerName(newName, replyTo)) =>
        logInfo(ctx, s"Changing player name from $name to $newName")
        this.name = newName
        replyTo ! PlayerInfo(userId, name)
        Behaviors.same
    })
  }

  private def createPlayersStatus(playersInLobby: List[PlayerInLobby], playersPlaying: List[PlayerPlaying]): List[PlayerStatus] = {
    val idRank = playersPlaying.map(p => (p.userID, p.rank))

    val rankById = idRank.map(r => r._1 -> r).toMap

    playersInLobby.flatMap { a =>
      rankById.get(a.userID).map { r =>
        PlayerStatus(a, r._2, true)
      }
    }
  }

  private def hostBehavior(game: GameInConstruction): Behavior[Message] = {

    def updateServer(ctx: ActorContext[Message], gameUpdated: GameInConstruction) = {
      if gameUpdated.gameParameters.isPublic then
        ctx.spawnAnonymous(contactInReceptionistAndAsk
          (ServerKey)
          (_ ! ServerMessages.UpdateGame(game, ctx.self))
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

      case (ctx, IWantToLeaveTheGame(player)) =>
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
        game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameCancelled())
        ctx.system.receptionist ! Receptionist.deregister(akka.actor.typed.receptionist.ServiceKey[Message](game.code), ctx.self)
        viewActorRef ! PreGameViewMessages.GameAborted()
        connectionHandler ! ConnectionHandler.UpdateList(List())
        start

      case (ctx, StartTheGame()) =>
        //The game has started
        //        logInfo(ctx,s"Starting game: ${game.code}")
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

        hostWaitGameFromCoordinator(hostRef, game, gameCoordinator)
    })
  }

  private def hostWaitGameFromCoordinator(hostRef: ActorRef[ClientCommand], game: GameInConstruction, gameCoordinator: ActorRef[IGameCoordinatorMessage]): Behavior[Message] = {
    Behaviors.receivePartial {
      case (ctx, TakeGetInProgressGame(gameInProgress)) =>
        logInfo(ctx, s"Game in progress received: ${gameInProgress.code}")
        game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameHasStarted(hostRef, gameInProgress))
        awaitSynchronization(ctx, game.players.filter(!_.address.equals(ctx.self)).map(_.userID), () => {
          logInfo(ctx, s"All players synchronized, starting the game: ${gameInProgress.code}")
          gameCoordinator ! GameCoordinatorMessage.StartRevealingSection()
          prePlayCyclePhaseHost(gameCoordinator, createPlayersStatus(game.players, gameInProgress.players), hostRef)
        }, _ => {
          //If failed to synchronize
          //Brutal policy, we abort the game
          logError(ctx, s"Failed to synchronize all players, aborting the game: ${gameInProgress.code}")
          ctx.stop(gameCoordinator)
          game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameCancelled())
          viewActorRef ! PreGameViewMessages.GameAborted()
          connectionHandler ! ConnectionHandler.UpdateList(List())
          start
        })
    }
  }

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
            //Failed to join, waiting for other commands from the user
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
          (() => viewActorRef ! PreGameViewMessages.GameJoinedFailed(gameCode))
        )

        responseForJoining(gameCode)

      case (ctx, JoinGame(game)) =>
        logInfo(ctx, s"Trying to join game: ${game.code}")
        game.players.head.address ! IWantToPlay(PlayerInLobby(userId, name, ctx.self), ctx.self)
        responseForJoining(game.code)

      case (ctx, JoinAGame()) =>
        ctx.spawnAnonymous(contactInReceptionistAndAsk
          (ServerKey)
          (_ ! ServerMessages.GetGames(ctx.self))
          (() => viewActorRef ! PreGameViewMessages.FailedToPublishToServer()))
        joiningAGame

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
        game.players.head.address ! IWantToLeaveTheGame(PlayerInLobby(userId, name, ctx.self))
        connectionHandler ! ConnectionHandler.UpdateList(List())
        start

      case (ctx, GameCancelled()) =>
        returnToStart(ctx)

      case (ctx, PlayerUnreachable(playerInLobby)) =>
        returnToStart(ctx)

      case (ctx, GameHasStarted(hostRef, gameInProgress)) =>
        logInfo(ctx, s"Game has started: ${game.code}")
        hostRef ! SynchronizationAck(userId)

        viewActorRef ! PreGameViewMessages.GameStarted()
        viewActorRef ! ViewsProxyActor.SwitchToGameView()
        val gameCoordinator = ctx.spawn(GameCoordinatorActor(ctx.self, viewActorRef, userId, gameInProgress), "GameCoordinatorActor")
        gameCoordinator ! GameCoordinatorMessage.StartRevealingSection()
        connectionHandler ! ConnectionHandler.UpdateList(game.players)
        prePlayCyclePhaseJoined(gameCoordinator, createPlayersStatus(game.players, gameInProgress.players), hostRef)
    })
  }

  private def prePlayCyclePhaseHost(gameCoordinator: ActorRef[IGameCoordinatorMessage], playersStatus: List[PlayerStatus], hostRef: ActorRef[ClientInternalCommand]): Behavior[Message] = {

    def otherPlayersOnline = playersStatus.filterNot(p => !p.isOnline || p.playerInfo.userID.equals(this.userId))

    var phaseLogs: List[TurnLog] = List()

    def hostCheckIfReady(ctx: ActorContext[Message], log: TurnLog): Behavior[Message] = {
      phaseLogs = phaseLogs :+ log

      if phaseLogs.size == playersStatus.size then {
        logInfo(ctx, s"All players have sent their logs, informing other players")
        // informing view of the logs
        phaseLogs.foreach(viewActorRef ! GameViewMessages.PreCyclePhaseAdversaryLog(_))
        // sending all the logs to other players
        otherPlayersOnline.map(_.playerInfo.address).foreach(_ ! AllTheLogs(phaseLogs))
        // synchronizing all players
        awaitSynchronization(ctx, otherPlayersOnline.map(_.playerInfo.userID), () => {
          logInfo(ctx, s"All players synchronized after revealing cards phase, proceeding to game start")
          gameCoordinator ! GameCoordinatorMessage.StartPlayCycle()
          inGameBehavior(gameCoordinator, playersStatus, hostRef)
        }, _ => {
          //If failed to synchronize
          //Brutal policy, we abort the game
          logError(ctx, s"Failed to synchronize all players after revealing cards phase, aborting the game")
          ctx.stop(gameCoordinator)
          otherPlayersOnline.map(_.playerInfo.address).foreach(_ ! GameCancelled())
          viewActorRef ! GameViewMessages.GameDeleted()
          connectionHandler ! ConnectionHandler.UpdateList(List())
          start
        })
      }
      else {
        Behaviors.same
      }
    }

    withShared({
      case (ctx, InitialPhaseCompleted(log)) =>
        logInfo(ctx, s"Received ${InitialPhaseCompleted(log)}")
        hostCheckIfReady(ctx, log)
    })
  }

  private def prePlayCyclePhaseJoined(gameCoordinator: ActorRef[IGameCoordinatorMessage], playersStatus: List[PlayerStatus], hostRef: ActorRef[ClientCommand]): Behavior[Message] = {

    withShared({
      case (ctx, InitialPhaseCompleted(log)) =>
        logInfo(ctx, s"Received ${InitialPhaseCompleted(log)}")
        hostRef ! InitialPhaseCompleted(log)
        Behaviors.same

      case (ctx, AllTheLogs(logs)) =>
        logInfo(ctx, s"Received all the logs from host")
        logs.foreach(viewActorRef ! GameViewMessages.PreCyclePhaseAdversaryLog(_))
        hostRef ! SynchronizationAck(userId)
        gameCoordinator ! GameCoordinatorMessage.StartPlayCycle()
        inGameBehavior(gameCoordinator, playersStatus, hostRef)

      case (ctx, GameCancelled()) =>
        logInfo(ctx, s"Game has been cancelled, returning to initial phase")
        viewActorRef ! GameViewMessages.GameDeleted()
        returnToStart(ctx, gameCoordinator)

    }, "preGamePhase")
  }

  private def returnToStart[T](ctx: ActorContext[Message], toStop: ActorRef[T]) = {
    ctx.stop(toStop)
    connectionHandler ! ConnectionHandler.UpdateList(List())
    viewActorRef ! ViewsProxyActor.SwitchToInitialView()
    start
  }

  private def inGameBehavior(gameCoordinator: ActorRef[IGameCoordinatorMessage], playersStatus: List[PlayerStatus], hostRef: ActorRef[ClientInternalCommand]): Behavior[Message] = {

    implicit val timeout: Timeout = 5.seconds

    def otherPlayers = playersStatus.filterNot(p => p.playerInfo.userID.equals(this.userId))

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
          if (!value.isOnline) {
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

    def inElectionBehavior(myRank: Int, playersStatus: List[PlayerStatus]): Behavior[Message] = {
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
              logInfo(ctx, s"Election started by another player: $replyTo")
              if myRank < candidateRank then {
                //i have lower rank, so i can not accept the election
                logInfo(ctx, s"My rank ($myRank) is lower than sender rank ($candidateRank), refusing his election")
                replyTo ! NoYouCanNot()
                // reset the election to give time to the others to align
                buffer.unstashAll(inElectionBehavior(myRank, playersStatus))
              } else {
                logInfo(ctx, s"My rank ($myRank) is higher than sender rank ($candidateRank), accepting his election")
                timers.cancelAll()
                buffer.unstashAll(inGameBehavior(gameCoordinator, playersStatus, hostRef))
              }

            case (ctx, ElectionWon()) =>
              logInfo(ctx, s"I won the election, becoming the new host")
              otherPlayers.foreach(_.playerInfo.address ! NewHostElected(ctx.self))
//              gameCoordinator ! WhoIsPlayingRequest(ctx.self)
              AskCoordinatorNextPlayer(ctx)
              buffer.unstashAll(inGameBehavior(gameCoordinator, playersStatus, ctx.self))

            case (ctx, NewHostElected(replyTo)) =>
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

    withShared({
      // GAME LOGIC LEVEL MESSAGES - START

      // send by the host when it cannot synchronize all the players at the start of the game
      case (ctx, GameCancelled()) =>
        logInfo(ctx, s"Game has been cancelled, returning to initial phase")
        viewActorRef ! GameViewMessages.GameDeleted()
        returnToStart(ctx, gameCoordinator)

      case (ctx, TurnEnded(game, log)) =>
        logInfo(ctx, s"My turn ended: ${this.userId}, round: ${game.currentRound}")
        otherPlayers.map(_.playerInfo.address).foreach(_ ! GameInProgressUpdate(ctx.self, game, log))

        awaitSynchronization(ctx, otherPlayers.map(_.playerInfo.userID),
          () => {
            logInfo(ctx, s"All players synchronized after my turn, ${this.userId}, waiting for my turn again: ${game.code}")
            AskCoordinatorNextPlayer(ctx)
            val onlineUpdate = playersStatus.map(_.copy(isOnline = true))
            inGameBehavior(gameCoordinator, onlineUpdate, hostRef)
          },
          failures => {
            //todo - what to do if is the host?
            logError(ctx, s"Some Players failed to synchronise: ${failures.mkString(", ")}, retrying for them")
            val onlineUpdate = playersStatus.collect {
              case ps if failures.contains(ps.playerInfo.userID) =>
                ps.copy(isOnline = false)
              case ps => ps
            }
            AskCoordinatorNextPlayer(ctx)
            inGameBehavior(gameCoordinator, onlineUpdate, hostRef)
        })

      case (ctx, GameInProgressUpdate(replyTo, game, log)) =>
        logInfo(ctx, s"Game info update, is turn: ${game.currentRound}")
        gameCoordinator ! GameCoordinatorMessage.LastTurnPlayed(game, log)
        Behaviors.withStash(50) { buffer =>
          withShared({
            case (ctx, TurnUpdated()) =>
              logInfo(ctx, s"GameCoordinator updated the turn")
              replyTo ! SynchronizationAck(userId)
              buffer.unstashAll(inGameBehavior(gameCoordinator, playersStatus, hostRef))

            case (ctx, other) =>
              logInfo(ctx, s"Stashing message until turn is updated: $other")
              buffer.stash(other)
              Behaviors.same
          })
        }

      case (ctx, LeaveTheGame()) =>
        logInfo(ctx, s"Leaving game, informing other players like I am unreachable")
        otherPlayers.foreach(_.playerInfo.address ! PlayerUnreachable(PlayerInLobby(userId, name, ctx.self)))
        returnToStart(ctx, gameCoordinator)

      case (ctx, GameEnded()) =>
        logInfo(ctx, s"Game has ended, returning to initial phase")
        returnToStart(ctx, gameCoordinator)

      // GAME LOGIC LEVEL MESSAGES - END

      case (ctx, PlayerUnreachable(playerInLobby)) =>
        playersStatus.find(_.playerInfo.userID == playerInLobby.userID) match {
          case None =>
            logError(ctx, s"Received unreachable for unknown player: ${playerInLobby.userID}")
            inGameBehavior(gameCoordinator, playersStatus, hostRef)
          case Some(p) =>
            if !p.isOnline then
              logInfo(ctx, s"Received unreachable for already offline player: ${p.playerInfo.userID}")
              inGameBehavior(gameCoordinator, playersStatus, hostRef)
            else {
              logInfo(ctx, s"Player: ${playerInLobby.userID} is unreachable")

              val onlineUpdate = playersStatus.map { ps =>
                if ps.playerInfo.userID == playerInLobby.userID then
                  ps.copy(isOnline = false)
                else
                  ps
              }

              if onlineUpdate.count(_.isOnline) < 2 then {
                logInfo(ctx, s"Less than 2 players online, aborting the game")
                viewActorRef ! GameViewMessages.AllOpponentsDisconnected()
                Behaviors.same
              } else {
                viewActorRef ! GameViewMessages.OpponentDisconnected(playerInLobby)

                // inform connection handler to check the status for only those who are online
                connectionHandler ! ConnectionHandler.UpdateList(onlineUpdate.filter(_.isOnline).map(_.playerInfo))

                if p.playerInfo.address equals hostRef then {
                  logInfo(ctx, s"Player: ${playerInLobby.userID} was the host, starting election")
                  //start election
                  val myRank = playersStatus.find(_.playerInfo.userID == userId).map(_.rank).getOrElse(-1)
                  onlineUpdate.filter(p => !p.playerInfo.userID.equals(this.userId) && p.isOnline && p.rank < myRank).foreach(_.playerInfo.address ! ElectionStarted(myRank, ctx.self))
                  inElectionBehavior(myRank, onlineUpdate)
                } else {
                  if ctx.self equals hostRef then AskCoordinatorNextPlayer(ctx)
                  inGameBehavior(gameCoordinator, onlineUpdate, hostRef)
                }
              }
            }
        }

      // ELECTION HOST LOGIC MESSAGES - START
      case (ctx, ElectionStarted(candidateRank, replyTo)) =>
        //another player is starting an election
        logInfo(ctx, s"Election started by another player: $replyTo")
        //todo - correct the -1
        val myRank = playersStatus.find(_.playerInfo.userID == userId).map(_.rank).getOrElse(-1)
        if myRank < candidateRank then {
          //I have lower rank, so I cannot accept the election
          logInfo(ctx, s"My rank ($myRank) is lower than sender rank ($candidateRank), refusing election")
          replyTo ! NoYouCanNot()
          // I start my own election
          otherPlayers.filter(_.rank < myRank).foreach(_.playerInfo.address ! ElectionStarted(myRank, ctx.self))
          val onlineUpdate = playersStatus.map { ps =>
            if ps.playerInfo.address equals hostRef then
              ps.copy(isOnline = false)
            else
              ps
          }
          inElectionBehavior(myRank, onlineUpdate)
        } else {
          inGameBehavior(gameCoordinator, playersStatus, hostRef)
        }

      case (ctx, NewHostElected(replyTo)) =>
        logInfo(ctx, "New host elected: " + replyTo)
        inGameBehavior(gameCoordinator, playersStatus, replyTo)

      case (ctx, WhoIsPlaying(currentPlayerID)) =>
        logInfo(ctx, s"Who is playing request received, current player is: $currentPlayerID")
        checkPlayerForTheTurn(currentPlayerID, ctx)
        Behaviors.same

      // messages for test purpose
      case (ctx, RemoveCheckPlayerStatus()) =>
        logInfo(ctx, s"Removing player status checking, clearing player list")
        connectionHandler ! ConnectionHandler.UpdateList(List())
        Behaviors.same
    })
  }