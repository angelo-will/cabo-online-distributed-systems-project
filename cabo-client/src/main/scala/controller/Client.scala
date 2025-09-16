package controller

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import akka.cluster.ClusterEvent.MemberExited
import model.Game.{GameInConstruction, GameInProgress}
import model.{GameParameters, PlayerInLobby, TurnLog}
import utils.ClientMessages.*
import utils.GameCoordinatorMessage.{NewTurn, PlayerCommand}
import utils.ServerMessages.{AbortGame, ServerKey}
import utils.{Message, ServerMessages, InitialViewMessages}

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

  case class GameInProgressUpdate(replyTo: ActorRef[ClientInternalCommand],game: GameInProgress, turnLog: TurnLog) extends ClientInternalCommand
  
  private def viewDefaultBehavior: Behavior[Message] = Behaviors.setup { ctx =>
    Behaviors.receiveMessagePartial {
      case _ =>
        ctx.log.info("View actor received a message, but it is not implemented yet.")
        Behaviors.same
    }
  }
  
  def apply(userId: String = "Player", name: String = "defaultCoolName", optionalViewActor: ActorRef[Message] = null): Behavior[Message] = Behaviors.setup { ctx =>
    
    //todo - create a view actor
    val viewActorRef = optionalViewActor match {
      case null => ctx.spawnAnonymous(viewDefaultBehavior)
      case ref => ref
    }

    val connectionHandler = ctx.spawn(ConnectionHandler[MemberExited](ctx.self), "ConnectionHandler")
    
    new Client(userId+ctx.self.path.address.hashCode(), name, viewActorRef, connectionHandler).start
  }

private case class Client(userId: String, var name: String, viewActorRef: ActorRef[Message], connectionHandler: ActorRef[ConnectionHandler.InternalCommand]):

  import controller.Client.*

  private case class ListingResponse(listing: Receptionist.Listing) extends Message

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
            ctx.log.error(s"Failed to synchronize all players in time: ${checkSync.filterNot(_._2).keys.mkString(", ")}")
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

  private def start: Behavior[Message] = Behaviors.setup { ctx =>

    withShared( {
      case (ctx, CreateNewGame(makePublic, maxTimeRound, maxNumRound, maxPlayers)) =>

        val player: PlayerInLobby = PlayerInLobby(userId, name, ctx.self)

        val game: GameInConstruction = GameInConstruction(userId + "game", GameParameters(makePublic, maxTimeRound, maxNumRound, maxPlayers), List(player))

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
    
    withShared( {

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
        ctx.log.info(s"Starting game: ${game.code}")

        if game.gameParameters.isPublic then
          ctx.spawnAnonymous(contactInReceptionistAndAsk
            (ServerKey)
            (_ ! ServerMessages.StartGame(game, ctx.self))
            (() => viewActorRef ! InitialViewMessages.FailedToPublishToServer()))

        //todo - check if we need to keep it for re-entering the game
        ctx.system.receptionist ! Receptionist.deregister(akka.actor.typed.receptionist.ServiceKey[Message](game.code), ctx.self)

        val gameCoordinator = ctx.spawn(GameCoordinatorActor(ctx.self, viewActorRef, userId, game), "GameCoordinatorActor")

        Behaviors.receiveMessagePartial {
          case TakeGetInProgressGame(gameInProgress) =>
            ctx.log.info(s"Game in progress received: ${gameInProgress.code}")
            game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameHasStarted(ctx.self, gameInProgress))
            awaitSynchronization(ctx, game.players.filter(!_.address.equals(ctx.self)).map(_.userID), () => {
              ctx.log.info(s"All players synchronized, starting the game: ${gameInProgress.code}")
              viewActorRef ! InitialViewMessages.ReadyToPlay(gameCoordinator)
              inGameBehavior(gameCoordinator, game.players.map(p => p -> true).toMap, ctx.self)
            }, () => {
              //If failed to synchronize
              //Brutal policy, we abort the game
              ctx.log.error(s"Failed to synchronize all players, aborting the game: ${gameInProgress.code}")
              //todo - check if this is ok
              ctx.stop(gameCoordinator)
              game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameCancelled())
              viewActorRef ! InitialViewMessages.GameAborted()
              //todo - if we use the variable argument this has to be changed
              connectionHandler ! ConnectionHandler.UpdateList(List())
              start
            })
        }

//      case (ctx, TakeGetInProgressGame_try2(gameInProgress, coordinatorRef)) =>
//        ctx.log.info(s"Game in progress received: ${gameInProgress.code}")
//        game.players.foreach(_.address ! GameHasStarted(gameInProgress))
//        awaitSynchronization(ctx, game.players.map(_.userID), () => {
//          ctx.log.info(s"All players synchronized, starting the game: ${gameInProgress.code}")
//          viewActorRef ! ViewMessages.ReadyToPlay(coordinatorRef)
//          inGameBehavior(gameInProgress)
//        }, () => {
//          //If failed to synchronize
//          //Brutal policy, we abort the game
//          ctx.log.error(s"Failed to synchronize all players, aborting the game: ${gameInProgress.code}")
//          //todo - check if this is ok
//          ctx.stop(coordinatorRef)
//          ctx.self ! LeaveTheGame()
//          Behaviors.same
//        })
    })
  }

  private def joiningAGame: Behavior[Message] = {

    def responseForJoining(): Behavior[Message] = {
      Behaviors.withTimers { timers =>
        timers.startTimerAtFixedRate(FailedToContactHost(), 60.seconds)
        withShared( {
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
    
    withShared( {
  
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
    
    withShared( {

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
          val gameCoordinator = ctx.spawn(GameCoordinatorActor(ctx.self, viewActorRef, userId, gameInProgress), "GameCoordinatorActor")
          viewActorRef ! InitialViewMessages.ReadyToPlay(gameCoordinator)
          hostRef ! SynchronizationAck(userId)
          //todo - a joiner initially check connection only with the host, in the game he should check also with other players?
          connectionHandler ! ConnectionHandler.UpdateList(game.players)
          inGameBehavior(gameCoordinator, game.players.map(p => p -> true).toMap, hostRef)
    })
  }

  //todo - retrieve who am i, so the rank, by id from the game players?
  private def inGameBehavior(gameCoordinator: ActorRef[PlayerCommand], playerOnline: Map[PlayerInLobby, Boolean], hostRef: ActorRef[ClientInternalCommand]): Behavior[Message] = {
    withShared( {
      case (ctx, TurnEnded(game, log)) =>
        ctx.log.info(s"My turn ended: ${game.code}")
        playerOnline.toList.filter((p,o) => !p.address.equals(ctx.self) && o).map(_._1.address).foreach(_ ! GameInProgressUpdate(ctx.self, game, log))
        //todo - sync to all the players
        awaitSynchronization(ctx, playerOnline.filter(p => !p._1.address.equals(ctx.self) && p._2).keys.map(_.userID).toList, () => {
          ctx.log.info(s"All players synchronized after my turn, waiting for my turn again: ${game.code}")
          Behaviors.same
        }, () => {
          //todo - what to do if not all the players have synchronized?
          Behaviors.same
        })
        Behaviors.same

      case (ctx, LeaveTheGame()) =>
        //todo
        ctx.stop(gameCoordinator)
        connectionHandler ! ConnectionHandler.UpdateList(List())
        start

      case (ctx, PlayerUnreachable(playerInLobby)) =>
        //todo - host management
        val onlineUpdate = playerOnline.updatedWith(playerInLobby) {
          case None => None
          case Some(v) => Some(false)
        }
        //todo - add a specific message to the view
//        viewActorRef ! ViewMessages.GameAborted()
        inGameBehavior(gameCoordinator, onlineUpdate, hostRef)

      case (ctx, GameInProgressUpdate(replyTo, game, log)) =>
        //todo - update gameCoordinator
        ctx.log.info(s"Game info update: ${game.code}")
        gameCoordinator ! NewTurn(game)
        //todo - sync to all the players, wait for gameCoordinator ack?
        withShared( {
          case (ctx, TurnUpdated()) =>
            ctx.log.info(s"GameCoordinator updated the turn")
            replyTo ! SynchronizationAck(userId)
            inGameBehavior(gameCoordinator, playerOnline, hostRef)
        })
    })
  }