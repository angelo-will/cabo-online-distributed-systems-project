
import akka.actor.typed.javadsl.Behaviors
import com.typesafe.config.ConfigFactory
import utils.{deployActor, startup, startupWithRole}

object Main extends App:

  // Start also the seed nodes as access point for the cluster
  utils.seeds.foreach(port => startup(ConfigFactory.load("application.conf"), port)(Behaviors.empty))

  startupWithRole(role="server", port = 2553)(deployActor(Server("server"))("lobbyServer"))

