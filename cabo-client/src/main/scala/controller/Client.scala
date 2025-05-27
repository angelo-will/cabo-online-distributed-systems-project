package controller

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.Behaviors
import model.Game.GameInConstruction
import model.{GameParameters, PlayerInLobby}
import utils.ServerMessages.ServerCommand
import utils.ViewMessages.*
import utils.{Message, ServerMessages}

object Client:

  case class IWantToPlay(newPlayer: PlayerInLobby, reply: ActorRef[Message]) extends Message

  case class YouJoinedTheGame(game: GameInConstruction) extends Message

  case class YouCanNotJoinTheGame() extends Message

  case class UpdateAboutGame(game: GameInConstruction) extends Message
  
  case class GameHasStarted() extends Message

  case class ListingResponseListing(listing: Receptionist.Listing) extends Message

  def apply(userId: String = "Player", name: String = "defaultCoolName"): Behavior[Message] = Behaviors.setup { ctx =>
    
    //todo - create a view actor
    val viewActorRef = ctx.spawn(Behaviors.setup(ctx => Behaviors.receiveMessagePartial{
      case _ =>
        ctx.log.info("View actor received a message, but it is not implemented yet.")
        Behaviors.same
    }), "ViewActor")
    
    new Client(userId, name, viewActorRef).start
  }

private case class Client(userId: String, name: String, viewActorRef: ActorRef[Message]):

  import controller.Client.*

  private case class ListingResponse(listing: Receptionist.Listing) extends Message

  //TODO: decide if add a failed message to send to caller
  private def contactServerAndAsk(whatToSay: ActorRef[ServerCommand] => Unit): Behavior[Message] = {
    Behaviors.setup { ctx =>
      val listingResponseAdapter = ctx.messageAdapter[Receptionist.Listing](ListingResponse.apply)

      ctx.system.receptionist ! Receptionist.find(ServerMessages.ServerKey, listingResponseAdapter)

      Behaviors.receiveMessagePartial {
        case ListingResponse(ServerMessages.ServerKey.Listing(listing)) =>
          if (listing.nonEmpty) {
            val server = listing.head
            whatToSay(server)
          } else {
            ctx.log.error("Server not found")
            //Send an error message to user
            viewActorRef ! FailedToPublishToServer()
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
          ctx.spawnAnonymous(contactServerAndAsk(_ ! ServerMessages.RegisterGame(game, ctx.self)))
        }

        viewActorRef ! GameCreated(game)

        waitingStart(game)

      case JoinAGame() =>

        ctx.log.info("Preparing to join a game")

        ctx.spawnAnonymous(contactServerAndAsk(_ ! ServerMessages.GetGames(ctx.self)))

        Behaviors.receiveMessagePartial {
          case ServerMessages.GamesList(games) =>
            if games.nonEmpty then {
              ctx.log.info(s"Games found: $games")
              viewActorRef ! GameList(games.toList)
            } else {
              ctx.log.warn("No games found")
              viewActorRef ! GameList(List())
            }
            Behaviors.same

          case JoinGame(game) =>
            ctx.log.info(s"Trying to join game: ${game.code}")
            game.players.head.address ! IWantToPlay(PlayerInLobby(userId, name, ctx.self), ctx.self)
            Behaviors.receiveMessagePartial {
              case YouJoinedTheGame(game) =>
                ctx.log.info(s"Joined game: $game")
                viewActorRef ! GameJoined(game)
                //Joined a game
                Behaviors.receiveMessagePartial {
                  case UpdateAboutGame(game) =>
                    ctx.log.info(s"Game info update: ${game.code}")
                    viewActorRef ! UpdateAboutGame(game)
                    Behaviors.same

                  case GameHasStarted() =>
                    ctx.log.info(s"Game has started: ${game.code}")
                    //todo - Go into game
                    Behaviors.empty
                }

              case YouCanNotJoinTheGame() =>
                ctx.log.warn("Could not join game")
                viewActorRef ! GameJoinedFailed(game)
                //Failed to join, waiting for other commands from the user
                Behaviors.same
            }
        }
    }
  }

  //DECIDERE SE USARE PLAYER.ID INVECE DI PLAYER NELLA MAPPA
  private def waitingStart(game: GameInConstruction): Behavior[Message] = {
    Behaviors.receivePartial {
      case (ctx, ServerMessages.GameRegistered(game, server)) =>
        //The server has registered the game
        ctx.log.info(s"Game update: ${game.code} by server: $server")
        Behaviors.same

      case (ctx, ServerMessages.FailedToRegisterGame(game, server)) =>
        //The server has failed to register the game
        ctx.log.error(s"Failed to register game: ${game.code}")
        viewActorRef ! FailedToPublishToServer()
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
            ctx.spawnAnonymous(contactServerAndAsk(_ ! ServerMessages.UpdateGame(gameUpdated, ctx.self)))

          replyTo ! YouJoinedTheGame(gameUpdated)

          gameUpdated.players.filter(p => !p.address.equals(ctx.self) & !p.address.equals(newPlayer.address)).foreach(_.address ! UpdateAboutGame(gameUpdated))

//          gameUpdated.players.foreach(_.address ! GameInfoUpdate(gameUpdated))

          viewActorRef ! UpdateAboutGame(gameUpdated)

          waitingStart(gameUpdated)
        } else {
          //The player cannot join the game
          ctx.log.info(s"Player: $newPlayer cannot join the game: $game")
          replyTo ! YouCanNotJoinTheGame()
          Behaviors.same
        }

      case (ctx, StartTheGame()) =>
        //The game has started
        ctx.log.info(s"Starting game: ${game.code}")

        if game.gameParameters.isPublic then
          ctx.spawnAnonymous(contactServerAndAsk(_ ! ServerMessages.StartGame(game, ctx.self)))

        game.players.foreach(_.address ! GameHasStarted())

        //Go into game
        Behaviors.same
    }
  }
  
  private def joinGame(): Behavior[Message] = Behaviors.empty