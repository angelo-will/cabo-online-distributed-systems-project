import akka.actor.typed.scaladsl.ActorContext


object Server:

  import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
  import akka.actor.typed.ActorRef
  import akka.actor.typed.Behavior
  import akka.actor.typed.scaladsl.Behaviors

  import model.*
  import utils.*

  def apply(serverCode: String): Behavior[Message] = Behaviors.setup { ctx =>
    ctx.log.info("Server started")
    ctx.system.receptionist ! Receptionist.register(ServiceKey[Message](serverCode), ctx.self)
    idle(serverCode, Seq.empty)
  }

  private def idle(serverCode: String, game: Games): Behavior[Message] = Behaviors.receivePartial {
    handleRegisterGame(game, idle(serverCode, _))
      .orElse(handleGameStarted(game, idle(serverCode, _)))
      .orElse(handleGameAborted(game, idle(serverCode, _)))
      .orElse(handleGamesRequest(game, idle(serverCode, _)))
  }

  private def handleRegisterGame(games: Games, nextBehaviors: Games => Behavior[Message]): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, RegisterGame(game, ref)) =>
      ctx.log.info(s"Registering game: $game")
      val updatedGames = games :+ game
      ref ! GameRegistered(game, ctx.self)
      nextBehaviors(updatedGames)

  private def handleGameStarted(games: Games, nextBehaviors: Games => Behavior[Message]): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, StartGame(game, ref)) =>
      ctx.log.info(s"Game started: $game, deleting from list")
      val updatedGames = games.filterNot(_.code == game.code)
      nextBehaviors(updatedGames)

  private def handleGameAborted(games: Games, nextBehaviors: Games => Behavior[Message]): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, AbortGame(game, ref)) =>
      ctx.log.info(s"Deleting game: $game")
      val updatedGames = games.filterNot(_.code == game.code)
      nextBehaviors(updatedGames)

  private def handleGamesRequest(games: Games, nextBehaviors: Games => Behavior[Message]): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] =
    case (ctx, GetGames(ref)) =>
      ctx.log.info(s"Getting games")
      ref ! GamesList(games)
      nextBehaviors(games)