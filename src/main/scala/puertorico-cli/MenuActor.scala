package puertorico_cli

import akka.actor.{Actor, ActorSystem}
import java.io.PrintStream
import puertorico._

class MenuActor(in: io.Source, out: PrintStream, system: ActorSystem) extends Actor {
  override def preStart() = {
    StdinMonitor.start(in, self)
    out.println("Welcome to the Puerto Rico CLI!")
    out.println("Player 1, enter your name.")
    showPrompt()
  }

  var p1Name: String = ""
  var p2Name: String = ""

  def showPrompt() = print("> ")

  def onEOF() {
      println(s"end of input, terminating...")
      system.shutdown()
  }

  def receive = p1EnterName
  
  def p1EnterName: Receive = {
    case StdinMonitor.Input(input) => {
      p1Name = input.trim
      out.println(s"Welcome, $p1Name!")
      out.println("Player 2, enter your name.")
      showPrompt()

      context.become(p2EnterName)
    }

    case StdinMonitor.EOF => onEOF()
  }

  def p2EnterName: Receive = {
    case StdinMonitor.Input(input) => {
      p2Name = input.trim
      out.println(s"Welcome, $p2Name!")
      out.println("Starting game...")
      out.println("")
      out.println(s"Your move, $p1Name.")
      out.println("For help, press ? at any time.")
      showPrompt()

      val gs = new GameState
      val menu = PuertoRicoCLIUtils.defaultMenu(gs, gs.playerOne, gs.playerTwo)
      context.become(runGame(menu))
    }

    case StdinMonitor.EOF => onEOF()
  }

  def runGame(state: MenuState): Receive = {
    case StdinMonitor.Input(input) => {
      if (state.hasAction(input)) {
        val result = state.takeAction(input)
        if (result.display != "") {
          out.println(result.display)
        }
        if (result.message != None) {
          out.println("sending message to game...")
          // TODO
        }
        if (result.newState != None) {
          context.become(runGame(result.newState.get))
        }
      } else {
        out.println("I didn't understand that...")
        out.println("For help, press ? at any time.")
      }

      showPrompt()
    }

    case StdinMonitor.EOF => onEOF()
  }
}

