object ServerApp {
  def main(args: Array[String]): Unit = {
    import akka.CaboAkkaUtils
    CaboAkkaUtils.startupWithRole(role = "server")(Server())
    println("SERVER APP started")
  }
}
