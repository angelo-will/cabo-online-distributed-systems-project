object ClientApp {
  def main(args: Array[String]): Unit = {
    import akka.CaboAkkaUtils
    import controller.Client
    if args.length != 0 then
      CaboAkkaUtils.startup()(Client(args(0), args(0)))
    else
      CaboAkkaUtils.startup()(Client())
    println("CLIENT APP started")
  }
}