package akka

import akka.actor.typed.{ActorSystem, Behavior}
import com.typesafe.config.{Config, ConfigFactory}

object CaboAkkaUtils {

  val seeds: List[Int] = List(2551, 2552) // seed used in the configuration

  def startup[X](port: Int = 0, configuration: Config = ConfigFactory.load("application.conf"))(root: => Behavior[X]): ActorSystem[X] = {
    val config = ConfigFactory
      .parseString(s"""akka.remote.artery.canonical.port=$port""")
      .withFallback(configuration)

    ActorSystem(root, "ClusterSystem", config)
  }

  def startupWithRole[X](
                          role: String,
                          port: Int = 0,
                          configuration: Config = ConfigFactory.load("application.conf")
                        )(root: => Behavior[X]): ActorSystem[X] = {
    val config = ConfigFactory
      .parseString(
        s"""
        akka.cluster.roles = [$role]
        """)
      .withFallback(configuration)

    startup(port, config)(root)
  }
}
