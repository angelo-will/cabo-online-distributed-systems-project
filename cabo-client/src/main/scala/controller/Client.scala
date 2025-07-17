package controller

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import akka.cluster.ClusterEvent.MemberExited
import model.Game.GameInConstruction
import model.{GameParameters, PlayerInLobby}
import utils.ClientMessages.*
import utils.ServerMessages.{AbortGame, ServerKey}
import utils.{Message, ServerMessages, ViewMessages}

import scala.concurrent.duration.DurationInt

object Client:

  case class IWantToPlay(newPlayer: PlayerInLobby, reply: ActorRef[Message]) extends Message

  case class YouJoinedTheGame(game: GameInConstruction) extends Message

  case class YouCanNotJoinTheGame(game: GameInConstruction) extends Message

  case class FailedToContactHost() extends Message

  case class UpdateAboutGame(game: GameInConstruction) extends Message

  case class IWantToLeaveTheGame(player: PlayerInLobby) extends Message

  case class GameCancelled() extends Message

  case class GameHasStarted() extends Message

  case class PlayerUnreachable(playerInLobby: PlayerInLobby) extends Message

  private case class ListingResponseListing(listing: Receptionist.Listing) extends Message
  
  private def viewDefaultBehavior: Behavior[Message] = Behaviors.setup { ctx =>
    Behaviors.receiveMessagePartial {
      case _ =>
        ctx.log.info("View actor received a message, but it is not implemented yet.")
        Behaviors.same
    }
  }
  
  def apply(userId: String = "Player", name: String = "defaultCoolName", viewBehavior: Behavior[Message] = viewDefaultBehavior): Behavior[Message] = Behaviors.setup { ctx =>
    
    //todo - create a view actor
    val viewActorRef = ctx.spawn(viewBehavior, "ViewActor")

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

  private def start: Behavior[Message] = Behaviors.setup { ctx =>

    Behaviors.receiveMessagePartial[Message] {
      
      case CreateNewGame(makePublic, maxTimeRound, maxNumRound, maxPlayers) =>

        val player: PlayerInLobby = PlayerInLobby(userId, name, ctx.self)

        val game: GameInConstruction = GameInConstruction(userId+"game", GameParameters(makePublic, maxTimeRound, maxNumRound, maxPlayers), List(player))

        if makePublic then {
          ctx.spawnAnonymous(contactInReceptionistAndAsk
            (ServerKey)
            (_ ! ServerMessages.RegisterGame(game, ctx.self))
            (() => viewActorRef ! ViewMessages.FailedToPublishToServer()))
        }

        ctx.system.receptionist ! Receptionist.register(akka.actor.typed.receptionist.ServiceKey[Message](game.code), ctx.self)

        connectionHandler ! ConnectionHandler.UpdateList(game.players)

        viewActorRef ! ViewMessages.GameCreated(game)

        hostBehavior(game)

      case JoinAGame() =>
        ctx.log.info("Preparing to join a game")
        ctx.spawnAnonymous(contactInReceptionistAndAsk
          (ServerKey)
          (_ ! ServerMessages.GetGames(ctx.self))
          (() => viewActorRef ! ViewMessages.FailedToPublishToServer()))
        joiningAGame

      case ChangePlayerName(newName, replyTo) =>
        ctx.log.info(s"Changing player name from $name to $newName")
        this.name = newName
        replyTo ! PlayerInfo(userId, name)
        Behaviors.same
        
        //todo - this should be shared between the states of the client
      case GetPlayerInfo(replyTo) =>
        ctx.log.info(s"Sending player info to $replyTo")
        replyTo ! PlayerInfo(userId, name)
        Behaviors.same
    }
  }

  private def hostBehavior(game: GameInConstruction): Behavior[Message] = {

    def removePlayerFromGame(ctx: ActorContext[Message], playerInLobby: PlayerInLobby) = {
      val gameUpdated = game.copy(players = game.players.filterNot(_.userID == playerInLobby.userID))

      if gameUpdated.gameParameters.isPublic then
        ctx.spawnAnonymous(contactInReceptionistAndAsk
          (ServerKey)
          (_ ! ServerMessages.UpdateGame(game, ctx.self))
          (() => viewActorRef ! ViewMessages.FailedToPublishToServer()))

      gameUpdated.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! UpdateAboutGame(gameUpdated))

      connectionHandler ! ConnectionHandler.UpdateList(gameUpdated.players)

      viewActorRef ! ViewMessages.GameInfoUpdate(gameUpdated)

      ctx.log.info(s"Player: ${playerInLobby.userID} removed the game, now the players are: ${gameUpdated.players.map(_.userID).mkString(", ")}")

      hostBehavior(gameUpdated)
    }

    Behaviors.receivePartial {

      case (ctx, GetPlayerInfo(replyTo)) =>
        ctx.log.info(s"Sending player info to $replyTo")
        replyTo ! PlayerInfo(userId, name)
        Behaviors.same
      
      case (ctx, ServerMessages.GameRegistered(game, server)) =>
        //The server has registered the game
        ctx.log.info(s"Game update: ${game.code} by server: $server")
        Behaviors.same

      case (ctx, ServerMessages.FailedToRegisterGame(game, server)) =>
        //The server has failed to register the game
        ctx.log.error(s"Failed to register game: ${game.code}")
        viewActorRef ! ViewMessages.FailedToPublishToServer()
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
              (() => viewActorRef ! ViewMessages.FailedToPublishToServer()))

          replyTo ! YouJoinedTheGame(gameUpdated)

          connectionHandler ! ConnectionHandler.UpdateList(gameUpdated.players)

          gameUpdated.players.filter(p => !p.address.equals(ctx.self) & !p.address.equals(newPlayer.address)).foreach(_.address ! UpdateAboutGame(gameUpdated))

          viewActorRef ! ViewMessages.GameInfoUpdate(gameUpdated)

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
            (() => viewActorRef ! ViewMessages.FailedToPublishToServer()))
        game.players.filter(!_.address.equals(ctx.self)).foreach(_.address ! GameCancelled())
        ctx.system.receptionist ! Receptionist.deregister(akka.actor.typed.receptionist.ServiceKey[Message](game.code), ctx.self)
        viewActorRef ! ViewMessages.GameAborted()
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
            (() => viewActorRef ! ViewMessages.FailedToPublishToServer()))

        game.players.foreach(_.address ! GameHasStarted())

        ctx.system.receptionist ! Receptionist.deregister(akka.actor.typed.receptionist.ServiceKey[Message](game.code), ctx.self)

        //Go into game
        Behaviors.same
    }
  }

  private def joiningAGame: Behavior[Message] = {

    def responseForJoining(): Behavior[Message] = {
      Behaviors.withTimers { timers =>
        timers.startTimerAtFixedRate(FailedToContactHost(), 60.seconds)
        Behaviors.receivePartial {
          case (ctx, YouJoinedTheGame(game)) =>
            ctx.log.info(s"Joined game: $game")
            connectionHandler ! ConnectionHandler.UpdateList(List(game.players.head))
            viewActorRef ! ViewMessages.GameJoined(game)
            //Joined a game
            gameJoined(game)

          case (ctx, YouCanNotJoinTheGame(game)) =>
            ctx.log.warn("Could not join game")
            viewActorRef ! ViewMessages.GameJoinedFailed(game)
            //Failed to join, waiting for other commands from the user
            joiningAGame

          case (ctx, FailedToContactHost()) =>
            ctx.log.info("Failed to contact host")
            timers.cancelAll()
            //todo - ask the view if wants to retry
            joiningAGame
        }
      }
    }

    Behaviors.receivePartial {

      case (ctx, GetPlayerInfo(replyTo)) =>
        ctx.log.info(s"Sending player info to $replyTo")
        replyTo ! PlayerInfo(userId, name)
        Behaviors.same
      
      case (ctx, ServerMessages.GamesList(games)) =>
        if games.nonEmpty then {
          ctx.log.info(s"Games found: $games")
          viewActorRef ! ViewMessages.GameList(games.toList)
        } else {
          ctx.log.warn("No games found")
          viewActorRef ! ViewMessages.GameList(List())
        }
        Behaviors.same

      case (ctx, JoinAddress(address)) =>

        ctx.spawnAnonymous(contactInReceptionistAndAsk
          (akka.actor.typed.receptionist.ServiceKey[Message](address))
          (_ ! IWantToPlay(PlayerInLobby(userId, name, ctx.self), ctx.self))
          //todo - add a specific message to viewActorRef
          (() => viewActorRef ! ViewMessages.FailedToPublishToServer()))

        responseForJoining()

      case (ctx, JoinGame(game)) =>
        ctx.log.info(s"Trying to join game: ${game.code}")
        game.players.head.address ! IWantToPlay(PlayerInLobby(userId, name, ctx.self), ctx.self)
        responseForJoining()
    }
  }

  private def gameJoined(game: GameInConstruction): Behavior[Message] = {
    Behaviors.receivePartial {

      case (ctx, GetPlayerInfo(replyTo)) =>
        ctx.log.info(s"Sending player info to $replyTo")
        replyTo ! PlayerInfo(userId, name)
        Behaviors.same
      
      case (ctx, UpdateAboutGame(game)) =>
        ctx.log.info(s"Game info update: ${game.code}")
        viewActorRef ! ViewMessages.GameInfoUpdate(game)
        gameJoined(game)

      case (ctx, LeaveTheGame()) =>
        ctx.log.info(s"Leaving game: ${game.code}")
        game.players.head.address ! IWantToLeaveTheGame(PlayerInLobby(userId, name, ctx.self))
        // todo - decide if waiting for a response or not
        connectionHandler ! ConnectionHandler.UpdateList(List())
        start

      case (ctx, GameCancelled()) =>
        ctx.log.info(s"Game: ${game.code} has been aborted")
        viewActorRef ! ViewMessages.GameAborted()
        connectionHandler ! ConnectionHandler.UpdateList(List())
        start

      case (ctx, PlayerUnreachable(playerInLobby)) =>
        //todo - for now the same as above, but we could wait some time before assume the game is aborted
        ctx.log.info(s"Game: ${game.code} has been aborted")
        viewActorRef ! ViewMessages.GameAborted()
        connectionHandler ! ConnectionHandler.UpdateList(List())
        start

      case (ctx, GameHasStarted()) =>
        ctx.log.info(s"Game has started: ${game.code}")
        //todo - Go into game
        Behaviors.empty
    }
  }