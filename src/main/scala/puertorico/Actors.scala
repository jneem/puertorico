package puertorico
import akka.actor._
import akka.event.LoggingReceive
import scala.collection.immutable.HashMap

case object StartGame

class ProspectorMane(gameState: GameState) extends Actor {
  def receive = LoggingReceive{
    case BeginAction(playerMane) => {
      gameState.rolePicker.doubloons += gameState.rolesDoubloons(Prospector) + 1
      gameState.rolesDoubloons(Prospector) = -1
      sender ! EndAction
      println(s"sender is ${sender}")
      println(s"parent is ${context.parent}")
    }
  }
}

class CraftsmanMane(gameState: GameState) extends Actor {
  def receive = listenToRole
  def listenToRole: Receive = LoggingReceive {
    case BeginAction(playerMane) => {
      gameState.rolePicker.doubloons += gameState.rolesDoubloons(Craftsman)
      gameState.rolesDoubloons(Craftsman) = -1
      gameState.craft
      playerMane ! SelectGoodToProduce
      context.become(listenToPlayer)
    }
  }
  def listenToPlayer: Receive = LoggingReceive {
    case GoodSelected(good) => {
      if(gameState.canGetGood(good)){
        gameState.currentPlayer.goods(good) += 1
        context.parent ! EndAction
        context.become(listenToRole)
      }
      else sender ! SelectGoodToProduce
    }
    case NoneSelected => {
      context.parent ! EndAction
      context.become(listenToRole)
    }
  }
}

class SettlerMane(gameState: GameState) extends Actor {
  def receive = listenToRole
  def listenToRole: Receive = LoggingReceive {
    case BeginAction(playerMane) => {
      gameState.rolePicker.doubloons += gameState.rolesDoubloons(Settler)
      gameState.rolesDoubloons(Settler) = -1
      playerMane ! SelectPlantation
      context.become(listenToPlayer)
    }
  }
  def listenToPlayer: Receive = LoggingReceive {
    case PlantationSelected(plant) => {
      if (gameState.canGetPlantation(plant)) {
        //TODO: add plantation to current player
        //TODO: figure out how to ask the other player as well
      }
    }
  }
}

class PlayerMane(gameState: GameState) extends Actor {
  def receive = LoggingReceive {
    case ChooseRole => {
      //trigger some role selection
      val role = Prospector
      sender ! RoleChosen(role)
    }
    case SelectGoodToProduce => {
      //trigger some good selection
      val good = Corn
      sender ! GoodSelected(Corn)
    }
  }
}

class RoleMane(gameState: GameState) extends Actor {
  val prospectorMane = context.actorOf(Props(new ProspectorMane(gameState)), name = "prospectorManager")
  val craftsmanMane = context.actorOf(Props(new CraftsmanMane(gameState)), name = "craftsmanManager")
  val playerOneMane = context.actorOf(Props(new PlayerMane(gameState)), name = "playerOne")
  val playerTwoMane = context.actorOf(Props(new PlayerMane(gameState)), name = "playerTwo")

  //map PlayerState and Roles to their managers
  val stateToMane = HashMap[PlayerState, ActorRef]((gameState.playerOne, playerOneMane), (gameState.playerTwo, playerTwoMane))
  val roleToMane = HashMap[Role, ActorRef](
    Prospector -> prospectorMane, 
    Craftsman -> craftsmanMane
  )

  //has two modes: listenToPlayer and listenToRole
  def receive = {
    case StartGame => {
      stateToMane(gameState.governor) ! ChooseRole
      context.become(listenToPlayer)
    }
  }

  val listenToPlayer: Receive = LoggingReceive {
    case RoleChosen(role) => {
      if (sender == stateToMane(gameState.rolePicker)){
        roleToMane(role) ! BeginAction(sender)
        context.become(listenToRole)
      }
    }
  }

  val listenToRole: Receive = LoggingReceive {
    case EndAction => {
      //TODO: Synchronize gamestate with player's gamestate on client side
      gameState.nextPlayerPickRoles
      stateToMane(gameState.rolePicker) ! ChooseRole
      context.become(listenToPlayer)
    }
  }
}

object Main extends App {
  val system = ActorSystem("SimplePuertoRico")
  val gameState = new GameState
  
  val roleManager = system.actorOf(Props(new RoleMane(gameState)), name = "roleManager")

  roleManager ! StartGame

}
