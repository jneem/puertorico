package puertorico_cli

import akka.actor.ActorRef
import io.Source
import java.io.IOException

object StdinMonitor {
  case class Input(line: String)
  case object EOF

  def start(in: Source, listener: ActorRef): Thread = {
    val monitor = new StdinMonitor(in, listener)
    val thread = new Thread(monitor, "StdinMonitor")
    thread.setDaemon(true)
    thread.start()

    thread
  }
}

class StdinMonitor(in: Source, listener: ActorRef) extends Runnable {
  import StdinMonitor._

  def run() {
    try {
      in.getLines() foreach { line => listener ! Input(line) }
    } catch {
      case e: InterruptedException => println("interrupted")
      case e: IOException => println("I/O exception")
    } finally {
      listener ! EOF
    }
  }
}

