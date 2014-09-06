package puertorico_cli

import akka.actor.{ Actor, ActorSystem, Props, ActorRef }
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

  // The game boss sends lots of messages that
  // require responses. Here, we queue up the ones
  // that we haven't responded to yet.
  var requestQueue = List[Any]()

  private def nameAndState(player: ActorRef) = {
    if (player == p1Proxy)
      (p1Name, gameState.playerOneState)
    else
      (p2Name, gameState.playerTwoState)
  }

  private def nameAndStates(player: ActorRef) = {
    if (player == p1Proxy)
      (p1Name, gameState.playerOneState, gameState.playerTwoState)
    else
      (p2Name, gameState.playerTwoState, gameState.playerOneState)
  }

  /**
   * If there is a request in our queue, pop it
   * and change state accordingly, asking the user for
   * the relevant information.
   */
  def popRequest() = {
    if (requestQueue.nonEmpty) {
      val req = requestQueue.head
      requestQueue = requestQueue.tail
      handleRequest(req)
    }
  }

  def showPrompt() = {
    print("> ")
  }

  /**
   * Every case in here should finish by calling
   * context.become on a non-idle state.
   */
  def handleRequest(req: Any) = req match {
    case ChooseRole => {
      val (name, myState, otherState) = nameAndStates(sender)
      val state = chooseRolesMenu(gameState, myState, otherState)

      out.println(s"$name, please choose a role.")
      context.become(runGame(state))
    }

    case _ => {
      out.println(s"Sorry, I don't know how to handle $req yet...")
      val (name, myState, otherState) = nameAndStates(sender)
      val state = defaultMenu(gameState, myState, otherState)
      context.become(runGame(state))
    }
  }

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
      val menu = defaultMenu(gameState, gameState.playerOneState, gameState.playerTwoState)
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
          out.println(s"sending message ${result.message.get} to game...")

          // TODO: should really send it through either p1Proxy or p2Proxy
          p1Proxy ! result.message.get
        }

        if (result.newState != None) {
          val newState = result.newState.get

          // If we're asked to switch to an idle state but there's
          // work pending, figure out the next state according
          // to the work to do instead.
          if (newState.idle && requestQueue.nonEmpty) {
            popRequest()
          } else {
            context.become(runGame(newState))
          }
        }
      } else {
        out.println("I didn't understand that...")
        out.println("For help, press ? at any time.")
      }

      showPrompt()
    }

    case StdinMonitor.EOF => onEOF()

    case x => {
      if (state.idle) {
        out.println("")
        handleRequest(x)
        showPrompt()
      } else {
        out.println("Got a message, but I'm busy...")
        requestQueue = requestQueue :+ x
      }
    }
  }
}

