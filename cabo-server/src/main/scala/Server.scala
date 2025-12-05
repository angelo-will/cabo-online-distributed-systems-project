import akka.cluster.ddata.Replicator.*
import akka.cluster.ddata.{ORSet, ORSetKey, SelfUniqueAddress}
import akka.cluster.ddata.typed.scaladsl.{DistributedData, Replicator}
import akka.cluster.ddata.typed.scaladsl.Replicator.{Get, Update}
import akka.cluster.ddata.Replicator.Changed
import utils.ServerMessages


object Server:

  import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
  import akka.actor.typed.ActorRef
  import akka.actor.typed.Behavior
  import akka.actor.typed.scaladsl.Behaviors

  import model.*
  import model.Game.GameInConstruction
  import utils.ServerMessages.*
  import utils.Message

  private sealed trait InternalCommand extends Message

  private case class InternalUpdateResponse(rsp: UpdateResponse[ORSet[GameInConstruction]], game: GameInConstruction, replyTo: ActorRef[Message]) extends InternalCommand

  private case class InternalRemoveResponse(rsp: UpdateResponse[ORSet[GameInConstruction]]) extends InternalCommand

  private case class InternalGetResponse(rsp: GetResponse[ORSet[GameInConstruction]], replyTo: ActorRef[Message]) extends InternalCommand

  private case class InternalGetResponseForUpdate(rsp: GetResponse[ORSet[GameInConstruction]], game: GameInConstruction, replyTo: ActorRef[Message]) extends InternalCommand

  private case class InternalUpdateResponseForClear(rsp: UpdateResponse[ORSet[GameInConstruction]], replyTo: ActorRef[Message]) extends InternalCommand

  private case class InternalSubscribeResponse(rsp: SubscribeResponse[ORSet[GameInConstruction]]) extends InternalCommand

  def apply(): Behavior[Message] = Behaviors.setup { ctx =>
    ctx.log.info("Server started")
    ctx.system.receptionist ! Receptionist.register(ServerMessages.ServerKey, ctx.self)

    DistributedData.withReplicatorMessageAdapter[Message, ORSet[GameInConstruction]] { replicatorAdapter =>

      implicit val node: SelfUniqueAddress = DistributedData(ctx.system).selfUniqueAddress

      val listOfGames = ORSetKey[GameInConstruction]("listOfGames")
      replicatorAdapter.subscribe(listOfGames, InternalSubscribeResponse.apply)

      def removeGameFromList(game: GameInConstruction): Unit = {
        replicatorAdapter.askUpdate(
          askReplyTo => Update(listOfGames, ORSet.empty, writeLocal, askReplyTo) { currentSet =>
            val gameToRemove = currentSet.elements.find(_.code == game.code)

            gameToRemove match {
              case Some(realGame) => currentSet.remove(realGame)
              case None => currentSet
            }
          },
          InternalRemoveResponse.apply
//          rsp => InternalRemoveResponse(rsp, onComplete)
        )
      }

      def addGameInList(game: GameInConstruction, ref: ActorRef[Message]): Unit = {
        replicatorAdapter.askUpdate(
          askReplyTo => Update(listOfGames, ORSet.empty, writeLocal, askReplyTo)(_ :+ game),
          rsp => InternalUpdateResponse(rsp, game, ref))
      }

      def clearGamesList(ref: ActorRef[Message]): Unit = {
        replicatorAdapter.askUpdate(
          askReplyTo => Update(listOfGames, ORSet.empty, writeLocal, askReplyTo)(_.clear(node)),
          rsp => InternalUpdateResponseForClear(rsp, ref))
      }

      Behaviors.receiveMessagePartial[Message] {

        //Messages received from a player client

        case RegisterGame(game, ref) =>
          ctx.log.info(s"Registering game: $game")
          addGameInList(game, ref)
          Behaviors.same

        case StartGame(game, ref) =>
          ctx.log.info(s"Game started: $game, deleting from list")
          removeGameFromList(game)
          Behaviors.same

        case AbortGame(game, ref) =>
          ctx.log.info(s"Deleting game: $game")
          removeGameFromList(game)
          Behaviors.same

        case GetGames(ref) =>
          ctx.log.info(s"Getting games")
          replicatorAdapter.askGet(
            askReplyTo => Get(listOfGames, Replicator.ReadLocal, askReplyTo),
            rsp => InternalGetResponse(rsp, ref)
          )
          Behaviors.same

        case UpdateGame(game, ref) =>
          ctx.log.info(s"Updating game: $game")
          replicatorAdapter.askGet(
            askReplyTo => Get(listOfGames, Replicator.ReadLocal, askReplyTo),
            rsp => InternalGetResponseForUpdate(rsp, game, ref)
          )
          Behaviors.same

        case ClearGames(ref) =>
          ctx.log.info(s"Clearing games list")
          clearGamesList(ref)
          Behaviors.same

        // Message received from the adapter about the distributed data

        case InternalGetResponse(g@GetSuccess(key, _), ref) =>
          val data = g.get(key)
          ctx.log.info(s"Found the List Games:\n${data.elements}")
          ref ! GamesList(data.elements)
          Behaviors.same

        case InternalGetResponse(g@NotFound(key, _), ref) =>
          ctx.log.info(s"List of games data deleted")
          ref ! GamesList(Set())
          Behaviors.same

        //Not necessary, but written for match every possible case
        case InternalGetResponse(g@GetFailure(key, _), ref) =>
          ctx.log.info(s"Failed to found the list of Games")
          ref ! GamesList(Set())
          Behaviors.same

        // Check if the reply to the client can be done in another way, using the "request" parameter inside the Update message
        case InternalUpdateResponse(_: UpdateSuccess[_], game, ref) =>
          ctx.log.info(s"List of games updated")
          ctx.log.info(s"Actual game: $game")
          ref ! GameRegistered(game, ctx.self)
          Behaviors.same

        case InternalUpdateResponse(_: UpdateFailure[_], game, ref) =>
          ctx.log.info(s"Failed to update the list of games")
          ref ! FailedToRegisterGame(game, ctx.self)
          Behaviors.same

        case InternalUpdateResponseForClear(_: UpdateSuccess[_], ref) =>
          ctx.log.info(s"List of games cleared")
          ref ! GamesCleared(ctx.self)
          Behaviors.same

        case InternalRemoveResponse(_: UpdateSuccess[_]) =>
          ctx.log.info(s"Removed game from the list")
          Behaviors.same

        case InternalRemoveResponse(_: UpdateFailure[_]) =>
          ctx.log.info(s"Failed to removed game from the list")
          Behaviors.same

        case InternalGetResponseForUpdate(g@GetSuccess(key, _), gameUpdated, ref) =>
          ctx.log.info(s"Checking the list for the game to update")
          val data = g.get(listOfGames)
          val gameToRemove = data.elements.find(_.code == gameUpdated.code)
          gameToRemove match
            case Some(value) =>
              removeGameFromList(value)
              addGameInList(gameUpdated, ref)
            case None => ref ! FailedToUpdate(gameUpdated, ctx.self)
          Behaviors.same

        case InternalGetResponseForUpdate(NotFound(key, _), gameUpdate, ref) =>
          ctx.log.info(s"Updating game: Failed to get the list of Games")
          ref ! FailedToUpdate(gameUpdate, ctx.self)
          Behaviors.same

        case InternalGetResponseForUpdate(GetFailure(key, _), gameUpdate, ref) =>
          ctx.log.info(s"Updating game: Failed to get the list of Games")
          ref ! FailedToUpdate(gameUpdate, ctx.self)
          Behaviors.same

        case InternalSubscribeResponse(c@Changed(key)) =>
          val elements = c.get(key).elements
          ctx.log.info(s"--- GAMES UPDATE DETECTED ---")
          ctx.log.info(s"Current Games List (${elements.size}):")
          elements.foreach(g => ctx.log.info(s"   - ${g.code}"))
          Behaviors.same

        case InternalSubscribeResponse(d@Deleted(key)) =>
          ctx.log.warn(s"The key $key was deleted from DData")
          Behaviors.same

        case InternalSubscribeResponse(_) =>
          Behaviors.same

        case _ =>
          ctx.log.debug("Unknown message received")
          Behaviors.same
      }
    }

  }

//  private def idle(serverCode: String, game: Games): Behavior[Message] = Behaviors.receivePartial {
//    handleRegisterGame(game, idle(serverCode, _))
//      .orElse(handleGameStarted(game, idle(serverCode, _)))
//      .orElse(handleGameAborted(game, idle(serverCode, _)))
//      .orElse(handleGamesRequest(game, idle(serverCode, _)))
//  }
//
//  private def handleRegisterGame(games: Games, nextBehaviors: Games => Behavior[Message]): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
//    case (ctx, RegisterGame(game, ref)) =>
//      ctx.log.info(s"Registering game: $game")
//      val updatedGames = games :+ game
//      ref ! GameRegistered(game, ctx.self)
//      nextBehaviors(updatedGames)
//
//  private def handleGameStarted(games: Games, nextBehaviors: Games => Behavior[Message]): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
//    case (ctx, StartGame(game, ref)) =>
//      ctx.log.info(s"Game started: $game, deleting from list")
//      val updatedGames = games.filterNot(_.code == game.code)
//      nextBehaviors(updatedGames)
//
//  private def handleGameAborted(games: Games, nextBehaviors: Games => Behavior[Message]): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
//    case (ctx, AbortGame(game, ref)) =>
//      ctx.log.info(s"Deleting game: $game")
//      val updatedGames = games.filterNot(_.code == game.code)
//      nextBehaviors(updatedGames)
//
//  private def handleGamesRequest(games: Games, nextBehaviors: Games => Behavior[Message]): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
//    case (ctx, GetGames(ref)) =>
//      ctx.log.info(s"Getting games")
//      ref ! GamesList(games)
//      nextBehaviors(games)