package controller

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.cluster.ClusterEvent.MemberEvent
import akka.cluster.typed.{Cluster, Subscribe}
import controller.Client.PlayerUnreachable
import messages.Message
import model.PlayerInLobby

import scala.reflect.ClassTag

object ConnectionHandler:
  sealed trait InternalCommand extends Message

  case class UpdateList(playerList: List[PlayerInLobby]) extends InternalCommand

  private case class MemberEventAdapter(event: MemberEvent) extends InternalCommand

  private def normalBehavior(parent: ActorRef[Message], playerReferences: List[PlayerInLobby]): Behavior[InternalCommand] = {
      Behaviors.receivePartial {
        case (ctx, MemberEventAdapter(event)) =>
          ctx.log.info(s"MemberEventAdapter received with event ${event.member.address}")
          ctx.log.info(playerReferences.map(_.address.path.address).mkString("Current players: ", ", ", ""))
          playerReferences.find(_.address.path.address == event.member.address) match {
            case Some(player) =>
              ctx.log.info(s"Player ${player.userID} is unreachable")
              parent ! PlayerUnreachable(player)
            case None =>
              ctx.log.info(s"Member event for an unknown player: ${event.member.address}")
          }
          Behaviors.same

        case (ctx, UpdateList(playerList)) =>
          ctx.log.info(s"Updating player list: ${playerList.map(_.userID).mkString(", ")}")
          // Here you can implement the logic to update the player list in the UI or notify the parent actor
          normalBehavior(parent, playerList)
      }
  }

  // Using Generic Type T we can handle different types of MemberEvents, but only one type at a time
  def apply[T <: MemberEvent](parent: ActorRef[Message])(using ct: ClassTag[T]): Behavior[InternalCommand] = Behaviors.setup { ctx =>
    ctx.log.info("Client connection handler started")
    // Here you can implement the logic to handle the connection, e.g., sending messages to the server
    Cluster(ctx.system).subscriptions ! Subscribe(
      // Instead of ClassOf[T] because generic
      ctx.messageAdapter[MemberEvent](MemberEventAdapter.apply),
      ct.runtimeClass.asInstanceOf[Class[T]]
    )
    normalBehavior(parent, List())
  }
