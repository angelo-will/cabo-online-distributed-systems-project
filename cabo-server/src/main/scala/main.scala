import akka.actor.typed.javadsl.Behaviors
import akka.CaboAkkaUtils._

object Main extends App:

  // Start also the seed nodes as access point for the cluster
  seeds.foreach(port => startup(port)(Behaviors.empty))

  startupWithRole(role="server", port = 2553)(deployActor(Server())("lobbyServer"))

