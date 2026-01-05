object SeedApp {
  def main(args: Array[String]): Unit = {
    import akka.actor.typed.javadsl.Behaviors
    import akka.CaboAkkaUtils._
    seeds.foreach(port => {
      startup(port)(Behaviors.empty)
      println("Started seed node on port " + port)
    })
    println("SEED APP started")
  }
}
