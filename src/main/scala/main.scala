import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import com.typesafe.config.ConfigFactory
import controller.Client
import utils.{startup, startupWithRole, deployActor}

@main def main(): Unit =
  println("Hello world!")

@main def deployPlayer(port: Int, playerName: String): Unit = {
  //  val system = startup(2553)(deployActor(Client("Host", "CoolHost"))("host-client"))

  val system = ActorSystem(Client(playerName, playerName), "ClusterSystem", ConfigFactory
    .parseString(s"""akka.remote.artery.canonical.port=$port""")
    .withFallback(ConfigFactory.load("application.conf")))

  //  println(s"Host client started with ID: Host and name: CoolHost")
  //  readLine("Press ENTER ro create the game\n")
  //
  //  system ! ClientMessages.CreateNewGame(makePublic = false, maxTimeRound = 10, maxNumRound = 5, maxPlayers = 4)
}

@main def deploySeeds(): Unit = utils.seeds.foreach(port => startup(port = port)(Behaviors.empty))

@main def deployServer(): Unit = startupWithRole(role="server", port = 2560)(Server())

@main def deployP1():Unit = deployPlayer(2553,"player01")
@main def deployP2():Unit = deployPlayer(2554,"player02")
@main def deployP3():Unit = deployPlayer(2555,"player03")
@main def deployP4():Unit = deployPlayer(2556,"player04")

@main def multipleLaunch(): Unit =
  deployP1()
  deployP2()
  deployP3()
  deployP4()

