import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.Behaviors
import com.typesafe.config.ConfigFactory
import controller.Client
import utils.{ClientMessages, Message, startup}

import scala.io.StdIn.readLine

@main def deploySeeds(): Unit = utils.seeds.foreach(port => startup(port = port)(Behaviors.empty))

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

  system ! ClientMessages.JoinAddress("Hostgame")

  println("Die user should be in the game\n")

  readLine("Press ENTER to kill...\n")

  system.terminate()

  println("User should be dead, check log...\n")
}

object TestWithRealClient extends App {

  private val hostUserID = "TestClientID"
  private val defaultName = "TestName"

//  val gameInConstruction = GameInConstruction(hostUserID + "game", GameParameters(false, 10, 5, 4), List(PlayerInLobby(hostUserID, defaultName, clientHost)))
  val hostUser = startup(port = 2579)(Client(hostUserID, defaultName))

  println(s"Host user started with ID: $hostUserID and name: $defaultName")

  hostUser ! ClientMessages.CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4)

  scala.io.StdIn.readLine("Press ENTER to join the game...\n")
  
  val userToDie = startup(port = 2580)(Client("DieUser", "DieName"))
  
  userToDie ! ClientMessages.JoinAGame()
  
  userToDie ! ClientMessages.JoinAddress(hostUserID+"game")

  scala.io.StdIn.readLine("Press ENTER to kill...\n")

  userToDie.terminate()

  scala.io.StdIn.readLine("User should be dead, check log...\n")
}
