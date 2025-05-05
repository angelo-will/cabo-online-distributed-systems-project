import akka.cluster.ddata.Replicator.*
import akka.cluster.ddata.{ORSet, ORSetKey, SelfUniqueAddress}
import akka.cluster.ddata.typed.scaladsl.{DistributedData, Replicator}
import akka.cluster.ddata.typed.scaladsl.Replicator.{Get, Update}


object Server:

  import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
  import akka.actor.typed.ActorRef
  import akka.actor.typed.Behavior
  import akka.actor.typed.scaladsl.Behaviors

  import model.*
  import utils.ServerMessages.*
  import utils.Message

  private sealed trait InternalCommand extends Message
  private case class InternalUpdateResponse(rsp: UpdateResponse[ORSet[GameInConstruction]], game: GameInConstruction, replyTo: ActorRef[Message])
    extends InternalCommand
  private case class InternalRemoveResponse(rsp: UpdateResponse[ORSet[GameInConstruction]]) extends InternalCommand
  private case class InternalGetResponse(rsp: GetResponse[ORSet[GameInConstruction]], replyTo: ActorRef[Message])
    extends InternalCommand

  def apply(serverCode: String): Behavior[Message] = Behaviors.setup { ctx =>
    ctx.log.info("Server started")
    ctx.system.receptionist ! Receptionist.register(ServiceKey[Message](serverCode), ctx.self)

    DistributedData.withReplicatorMessageAdapter[Message, ORSet[GameInConstruction]] { replicatorAdapter =>
      
      implicit val node: SelfUniqueAddress = DistributedData(ctx.system).selfUniqueAddress
      
      val listOfGames = ORSetKey[GameInConstruction]("listOfGames")

      def removeGameFromList(game: GameInConstruction): Unit = {
        replicatorAdapter.askUpdate(
          askReplyTo => Update(listOfGames, ORSet.empty, writeLocal, askReplyTo)(_ remove game),
          InternalRemoveResponse.apply)
      }

      Behaviors.receiveMessagePartial[Message] {

        //Messages received from a player client

        case RegisterGame(game, ref) =>
          ctx.log.info(s"Registering game: $game")
          replicatorAdapter.askUpdate(
            askReplyTo => Update(listOfGames, ORSet.empty, writeLocal, askReplyTo)(_ :+ game),
            rsp => InternalUpdateResponse(rsp, game, ref))
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

        // Message received from the adapter about the distributed data

        case InternalGetResponse(g @ GetSuccess(key, _), ref) =>
          ctx.log.info(s"Found the List Games")
          val data = g.get(listOfGames)
          ref ! GamesList(data.elements.toSeq)
          Behaviors.same

        //Not necessary, but written for match every possible case
        case InternalGetResponse(g @ GetFailure(key, _), ref) =>
          ctx.log.info(s"Failed to found the list of Games")
          ref ! GamesList(Seq())
          Behaviors.same

        case InternalGetResponse(g @ NotFound(key, _), ref) =>
          ctx.log.info(s"List of games data deleted")
          ref ! GamesList(Seq())
          Behaviors.same

        // Check if the reply to the client can be done in another way, using the "request" parameter inside the Update message
        case InternalUpdateResponse(_: UpdateSuccess[_], game, ref) =>
          ctx.log.info(s"List of games updated")
          ref ! GameRegistered(game, ctx.self)
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