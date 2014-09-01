package puertorico
import akka.actor._
import akka.event.LoggingReceive


class ProspectorMane(gameState: GameState) extends Actor {
  def receive = LoggingReceive{
    case BeginAction => {
      //give money to rolePicker
      gameState.rolePicker.doubloons += gameState.rolesDoubloons(Prospector) + 1
      println(s"done prospecting with ${gameState.rolePicker.doubloons}")
      sender ! EndAction
    }
  }
}

class PlayerMane(gameState: GameState) extends Actor {
  def receive = LoggingReceive {
    case ChooseRole => {
      println("Choosing a role")
      sender ! RoleChosen(Prospector)
    }
  }
}

class RoleMane(gameState: GameState) extends Actor {
  val prospectorMane = context.actorOf(Props(new ProspectorMane(gameState)), name = "prospectorManager")
  val playerOneMane = context.actorOf(Props(new PlayerMane(gameState)), name = "playerOne")

  //has two modes: listenToPlayer and listenToRole
  def receive = listenToRole

  val listenToPlayer: Receive = LoggingReceive {
    case RoleChosen(role) => {
      //TODO: check that the sender is from rolePicker
      println("tell prospector to begin action")
      prospectorMane ! BeginAction
      context.become(listenToRole)
    }
  }

  val listenToRole: Receive = LoggingReceive {
    case EndAction => {
      //TODO: Synchronize gamestate with player's gamestate
      //TODO: swap rolePicker
      println("tell playerOne to choose a role")
      playerOneMane ! ChooseRole
      context.become(listenToPlayer)
    }
  }
}

object Main extends App {
  val system = ActorSystem("SimplePuertoRico")
  val gameState = new GameState
  
  val roleManager = system.actorOf(Props(new RoleMane(gameState)), name = "roleManager")

  println("starting")
  roleManager ! EndAction

}
