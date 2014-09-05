package puertorico_cli

import akka.actor.{Actor, ActorSystem, Props, ActorRef}
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
  var p1Proxy: ActorRef = null
  var p2Proxy: ActorRef = null
  val gameState = new GameState

  private def nameAndState(player: ActorRef) = {
    if (player == p1Proxy)
      (p1Name, gameState.playerOneState)
    else
      (p2Name, gameState.playerTwoState)
  }

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

      // Start the game!
      val menu = PuertoRicoCLIUtils.defaultMenu(gameState, gameState.playerOneState, gameState.playerTwoState)
      p1Proxy = context.actorOf(Props[PassthroughActor])
      p2Proxy = context.actorOf(Props[PassthroughActor])
      val boss = context.actorOf(Props(new RoleBoss(p1Proxy, p2Proxy)))
      p1Proxy ! PassthroughActor.SetAlice(self)
      p2Proxy ! PassthroughActor.SetAlice(self)
      p1Proxy ! PassthroughActor.SetBob(boss)
      p2Proxy ! PassthroughActor.SetBob(boss)

      boss ! StartGame

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

    // TODO: instead of handling messages from the game
    // as the come, we need some mechanism for queueing them
    // up and dealing with them one at a time. For example,
    // the game will send two messages at the same time, asking
    // people to move their colonists. We need to deal with one first,
    // then the other one.
    case ChooseRole => {
      val (name, state) = nameAndState(sender)
      out.println(s"$name, please choose a role.")
      // TODO: finish this...
    }
  }
}

