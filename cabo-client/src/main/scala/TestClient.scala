import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.Behaviors
import akka.CaboAkkaUtils._
import com.typesafe.config.ConfigFactory
import controller.Client
import messages.{ClientMessages, Message}

import scala.io.StdIn.readLine

@main def deploySeeds(): Unit = seeds.foreach(port => startup(port = port)(Behaviors.empty))

@main def deployHost(): Unit = {
  //  val system = startup(2553)(deployActor(Client("Host", "CoolHost"))("host-client"))

  val system = ActorSystem(Client("Host", "CoolHost"), "ClusterSystem", ConfigFactory
    .parseString(s"""akka.remote.artery.canonical.port=2553""")
    .withFallback(ConfigFactory.load("application.conf")))

  println(s"Host client started with ID: Host and name: CoolHost")
  readLine("Press ENTER ro create the game\n")

  system ! ClientMessages.CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4)
}

@main def deployDieUser(): Unit = {
  val system = ActorSystem(Client("DieUser", "DieName"), "ClusterSystem", ConfigFactory
    .parseString(s"""akka.remote.artery.canonical.port=2554""")
    .withFallback(ConfigFactory.load("application.conf")))

  println(s"Die user started with ID: DieUser and name: DieName")
  readLine("Press ENTER to join the game...\n")

  system ! ClientMessages.JoinAGame()

  system ! ClientMessages.JoinWithGameCode("Hostgame")

  println("Die user should be in the game\n")

  readLine("Press ENTER to kill...\n")

  system.terminate()

  println("User should be dead, check log...\n")
}