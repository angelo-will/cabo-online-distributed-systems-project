package utils

import akka.actor.typed.scaladsl.ActorContext

object AppLogger {

  import java.time.LocalTime
  import java.time.format.DateTimeFormatter

  case class Log(withTime: Boolean) {
    private var prefix = ""

    def startWith(str: String): Log = {
      prefix = str
      this
    }

    def log(msg: String): Unit = {
      println(s"${if (withTime) "[" + now() + "]" + " - " else ""}$prefix$msg")
    }

    private def now(): String = {
      val timeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")
      LocalTime.now().format(timeFormatter)
    }
  }
}
