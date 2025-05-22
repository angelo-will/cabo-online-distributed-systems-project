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

  private case class IWantToPlay(newPlayer: PlayerInLobby, reply: ActorRef[Message]) extends Message

  private case class YouJoinedTheGame(game: GameInConstruction) extends Message

  private case class YouCanNotJoinTheGame() extends Message

  private case class GameInfoUpdate(game: GameInConstruction) extends Message
  
  private case class GameHasStarted() extends Message

  private case class ListingResponse(listing: Receptionist.Listing) extends Message

  def apply(userId: String = "Player", name: String = "defaultCoolName"): Behavior[Message] = Behaviors.setup { ctx =>
    
    //todo - create a view actor
    val viewActorRef = ctx.spawn(Behaviors.empty, "ViewActor")
    
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
          Behaviors.empty
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

//          ctx.spawnAnonymous(Behaviors.setup { ctx =>
//
//            val listingResponseAdapter = ctx.messageAdapter[Receptionist.Listing](ListingResponse.apply)
//
//            ctx.system.receptionist ! Receptionist.find(ServerMessages.ServerKey, listingResponseAdapter)
//
//            Behaviors.receiveMessagePartial {
//              case ListingResponse(ServerMessages.ServerKey.Listing(listing)) =>
//                if listing.nonEmpty then
//                  val server = listing.head
//                  server ! ServerMessages.RegisterGame(game, parent)
//                else
//                  ctx.log.error("Server not found")
//                  //Send an error message to user
//                  viewActorRef ! FailedToPublishToServer()
//
//                Behaviors.empty
//            }
//
//          })
        }

        viewActorRef ! GameCreated(game)

        waitingStart(game)

      case JoinAGame() =>

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
            ctx.log.info(s"Trying to join game: $game")
            game.players.head.address ! IWantToPlay(PlayerInLobby(userId, name, ctx.self), ctx.self)
            Behaviors.receiveMessagePartial {
              case YouJoinedTheGame(game) =>
                ctx.log.info(s"Joined game: $game")
                viewActorRef ! GameJoined(game)
                //Joined a game
                Behaviors.receiveMessagePartial {
                  case GameInfoUpdate(game) =>
                    ctx.log.info(s"Game info update: $game")
                    viewActorRef ! GameInfoUpdate(game)
                    Behaviors.same

                  case GameHasStarted() =>
                    ctx.log.info(s"Game has started: $game")
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

        joinGame()
    }
  }
  
  //DECIDERE SE USARE PLAYER.ID INVECE DI PLAYER NELLA MAPPA
  private def waitingStart(game: GameInConstruction): Behavior[Message] = {
    Behaviors.receivePartial {
      case (ctx, ServerMessages.GameRegistered(game, server)) =>
        //The server has registered the game
        ctx.log.info(s"Game: $game has been updated by server: $server")
        Behaviors.same

      case (ctx, ServerMessages.FailedToRegisterGame(game, server)) =>
        //The server has failed to register the game
        ctx.log.error(s"Failed to register game: $game")
        viewActorRef ! FailedToPublishToServer()
        //Go into lobby
        Behaviors.same

//      case (ctx, ListingResponse(ServerMessages.ServerKey.Listing(listing))) =>
//        if (listing.nonEmpty) {
//          //The server has been found
//          ctx.log.info(s"Server found: $listing")
//          listing.head ! ServerMessages.UpdateGame(game, ctx.self)
//        } else {
//          //The server has not been found
//          ctx.log.warn("Server not found")
//          //Send an error message to user
//          viewActorRef ! FailedToPublishToServer()
//        }
//        Behaviors.same

      case (ctx, IWantToPlay(newPlayer: PlayerInLobby, replyTo: ActorRef[Message])) =>
        //The player wants to play
        ctx.log.info(s"Player: $newPlayer wants to play")
        if (game.players.size < game.gameParameters.maxPlayers) {
          //The player can join the game
          ctx.log.info(s"Player: $newPlayer can join the game: $game")
          val gameUpdate = game.copy(players = game.players :+ newPlayer)

          //todo - modify the gameParameters public as a Boolean
          if !gameUpdate.gameParameters.isPrivate then
            //Update the game on the server
            ctx.spawnAnonymous(contactServerAndAsk(_ ! ServerMessages.UpdateGame(gameUpdate, ctx.self)))

          replyTo ! YouJoinedTheGame(gameUpdate)

          gameUpdate.players.foreach(_.address ! GameInfoUpdate(gameUpdate))

          viewActorRef ! GameInfoUpdate(gameUpdate)

          waitingStart(gameUpdate)
        } else {
          //The player cannot join the game
          ctx.log.info(s"Player: $newPlayer cannot join the game: $game")
          replyTo ! YouCanNotJoinTheGame()
        }
        Behaviors.same

      case (ctx, StartTheGame()) =>
        //The game has started
        ctx.log.info(s"Game: $game has started")

        game.players.foreach(_.address ! GameHasStarted())

        //Go into game
        Behaviors.empty
    }
  }
  
  private def joinGame(): Behavior[Message] = Behaviors.empty