package akka

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import com.typesafe.config.{Config, ConfigFactory}
import messages.Message

object CaboAkkaUtils {

  val seeds: List[Int] = List(2551, 2552) // seed used in the configuration

  def startup[X](port: Int = 0, configuration: Config = ConfigFactory.load("application.conf"))(root: => Behavior[X]): ActorSystem[X] =
    // Override the configuration of the port
    // Siccome sono nella stessa macchina devo usare porte diverse per simulare nodi diversi
    val config = ConfigFactory
      .parseString(s"""akka.remote.artery.canonical.port=$port""")
      .withFallback(configuration)

    // Create an Akka system
    ActorSystem(root, "ClusterSystem", config)

  def startupWithRole[X](role: String, port: Int = 0, configuration: Config = ConfigFactory.load("application.conf"))(root: => Behavior[X]): ActorSystem[X] =
    val config = ConfigFactory
      .parseString(
        s"""
        akka.cluster.roles = [$role]
        """)
      .withFallback(configuration)

    // Create an Akka system
    startup(port, config)(root)

  def deployActor(behavior: Behavior[Message])(actorName: String): Behavior[Message] = Behaviors.setup { ctx =>
    ctx.spawn(behavior, actorName)
    Behaviors.empty
  }
}
