object ClientApp {
  def main(args: Array[String]): Unit = {
    import akka.CaboAkkaUtils
    import controller.Client
    CaboAkkaUtils.startup()(Client())
    println("CLIENT APP started")
  }
}