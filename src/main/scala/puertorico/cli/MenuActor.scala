package org.puertorico.cli

import akka.actor.{ Actor, ActorSystem, Props, ActorRef }
import java.io.PrintStream
import org.slf4j.{ Logger, LoggerFactory }
import org.puertorico._

class MenuActor(in: io.Source, out: PrintStream, system: ActorSystem) extends Actor {
  lazy val logger = LoggerFactory.getLogger("MenuActor")

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

  // If we are currently gathering information for a player in order to
  // send a response back to the game manager, keep track of the player
  // on whose behalf we are gathering information.
  var activePlayer: ActorRef = null

  val gameState = new GameState

  // The game boss sends lots of messages that
  // require responses. Here, we queue up the ones
  // that we haven't responded to yet.
  var requestQueue = List[(Any, ActorRef)]()

  private def nameAndState(player: ActorRef) = {
    if (player == p1Proxy)
      (p1Name, gameState.playerOneState)
    else
      (p2Name, gameState.playerTwoState)
  }

  private def nameAndStates(player: ActorRef) = {
    if (player == p1Proxy)
      (p1Name, 0, gameState.playerOneState, gameState.playerTwoState)
    else
      (p2Name, 1, gameState.playerTwoState, gameState.playerOneState)
  }

  /**
   * While there are requests in our queue, keep acting on them
   * and removing them until we get to one that requires input
   * from the user.
   */
  def handleQueuedRequests() = {
    var done = false

    while (!done && requestQueue.nonEmpty) {
      val (req, sender) = requestQueue.head
      requestQueue = requestQueue.tail
      out.println(s"Acting on queued request ${req}")
      done = handleRequest(req, sender)
    }

    // If no request requires a reponse,
    // we just wait for more messages.
    if (!done)
      context.become(waitForMessage)
  }

  def showPrompt() = {
    print("> ")
  }

  // We sometimes queue up messages, so the sender of the message might
  // not agree with the sender field at the time we decided to process
  // the message. This variable will always store the sender of the
  // message being currently processed.
  var msgSource: ActorRef = null
  def senderId = if (msgSource == p1Proxy) 0 else 1
  def senderName = if (msgSource == p1Proxy) p1Name else p2Name
  def senderState =
    if (msgSource == p1Proxy) gameState.playerOneState
    else gameState.playerTwoState
  def nonSenderState =
    if (msgSource == p1Proxy) gameState.playerTwoState
    else gameState.playerOneState

  /**
   * Deals with a request from the game manager.
   *
   * Some of the requests from the game manager require user input,
   * followed by a response. Other requests require no input. For
   * responses that require an input, return true. Otherwise, return false.
   */
  def handleRequest(req: Any, sender: ActorRef): Boolean = req match {
    // The game sends two messages for everything that happens (one to
    // each player). So that we don't repeat every action twice,
    // only look at a message if it was directed at the player that it's supposed
    // to affect.
    case (playerId, msg) => {
      if (playerId == senderId) {
        msgSource = sender
        handleRequestPriv(msg)
      } else
        false
    }
  }

  private def handleRequestPriv(msg: Any): Boolean = msg match {
    case ChooseRole => {
      val state = chooseRolesMenu(gameState, senderState, nonSenderState)

      out.println(s"${senderName}, please choose a role.")
      promptForInput(state, msgSource)
      true
    }

    // Messages that cause a state update.
    case GotDoubloons(d) => {
      senderState.doubloons += d
      false
    }

    case GotRole(role) => {
      gameState.removeRole(role)
      false
    }

    case GotColonists(cols) => {
      senderState.colonistsSpare += 1
      false
    }

    case GotBuilding(b) => {
      senderState.addBuilding(b)
      false
    }

    case GotGood(g) => {
      senderState.goods(g) += 1
      false
    }

    // The settler conversation.
    case SelectPlantationExtra => {
      out.println(s"${senderName}, would you like a plantation from the deck?")

      val state = yesOrNoMenu(gameState, senderState, nonSenderState, PlantationExtraAgreed, NoneSelected)
      promptForInput(state, sender)
      true
    }

    case SelectPlantation => {
      out.println(s"${senderName}, please choose a plantation.")
      out.println(showPlantations(gameState))

      val state = choosePlantationMenu(gameState, senderState, nonSenderState)
      promptForInput(state, sender)
      true
    }

    // This appears both in settler, mayor, and builder conversations.
    case SelectColonist => {
      out.println(s"${senderName}, would you like an extra colonist?")

      val state = yesOrNoMenu(gameState, senderState, nonSenderState, ColonistSelected, NoneSelected)
      promptForInput(state, sender)
      true
    }

    // The craftsman conversation.
    case SelectGoodToProduce => {
      out.println(s"${senderName}, please choose an extra good.")

      val state = chooseExtraGoodMenu(gameState, senderState, nonSenderState)
      promptForInput(state, sender)
      true
    }

    case _ => {
      logger.debug(s"Sorry, I don't know how to handle $msg yet...")
      false
    }
  }

  def onEOF() {
    logger.debug(s"end of input, terminating...")
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
      out.println("For help, press ? at any time.")

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

      context.become(waitForMessage)
    }

    case StdinMonitor.EOF => onEOF()
  }

  def waitForMessage: Receive = {
    case StdinMonitor.EOF => onEOF()

    case StdinMonitor.Input(_) => logger.debug(s"Waiting to hear from the game manager...")

    case x => {
      logger.debug(s"Handling message ${x} from ${sender}")
      handleRequest(x, sender)
    }
  }

  def promptForInput(state: MenuState, active: ActorRef) = {
    activePlayer = active
    context.become(waitForUser(state))
    showPrompt()
  }

  def waitForUser(state: MenuState): Receive = {
    case StdinMonitor.Input(input) => {
      if (state.hasAction(input)) {
        val result = state.takeAction(input)
        if (result.display != "") {
          out.println(result.display)
        }
        if (result.message != None) {
          logger.debug(s"sending message ${result.message.get} to game...")

          activePlayer ! result.message.get
        }

        result.newState match {
          case SameState => promptForInput(state, activePlayer)
          case IdleState => handleQueuedRequests()
          case NewState(s) => promptForInput(s, activePlayer)
        }
      } else {
        out.println("I didn't understand that...")
        out.println("For help, press ? at any time.")
        showPrompt()
      }
    }

    case StdinMonitor.EOF => onEOF()

    case x => {
      logger.debug(s"Queueing message ${x} from ${sender}")
      requestQueue = requestQueue :+ (x, sender)
    }
  }
}

