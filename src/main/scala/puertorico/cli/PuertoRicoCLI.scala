package org.puertorico.cli

import akka.actor.{ ActorSystem, Props }

object PuertoRicoCLI extends App {
  val system = ActorSystem("PuertoRicoCLISystem")
  val menuProps = Props(new MenuActor(io.Source.stdin, System.out, system))

  system.actorOf(menuProps, "cliMenu")
}
