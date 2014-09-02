package puertorico

import akka.actor.{Actor, ActorRef, FSM}
import scala.collection.immutable.HashMap

sealed trait State
case object RoleProcess extends State
case object ProspectProcess extends State
case object CraftsmanProcess extends State
case object SettlerProcess extends State

sealed trait Data
case class DoOnce(playerMane: ActorRef) extends Data
case class DoOnceEach(playerList: List[ActorRef]) extends Data
case class DoOnceUntilSuccess(playerSet: Set[ActorRef]) extends Data
case class DoUntilSuccess(playerList: List[ActorRef]) extends Data

case object GameStateQuery

class RoleBoss(playerOneMane: ActorRef, playerTwoMane: ActorRef) extends Actor with FSM[State,Data] {

  val gameState = new GameState

  //map PlayerState to their actors
  val stateToMane = HashMap[PlayerState, ActorRef]((gameState.playerOne, playerOneMane), (gameState.playerTwo, playerTwoMane))

  def getNextRolePicker = {
    gameState.nextPlayerPickRoles
    val nextPlayerMane = stateToMane(gameState.rolePicker)
    nextPlayerMane ! ChooseRole
    nextPlayerMane
  }

  def giveRolePickerCard(role: Role){
      gameState.rolePicker.doubloons += gameState.rolesDoubloons(role)
      gameState.rolesDoubloons(role) = -1
  }

  startWith(RoleProcess, DoOnce(playerOneMane))

  when(RoleProcess){
    case Event(Prospector, DoOnce(playerMane)) => {
      //sender wants to be prospector
      //simple enough to deal with here
      if (sender == playerMane) {
        giveRolePickerCard(Prospector)
        gameState.rolePicker.doubloons += 1
        stay using DoOnce(getNextRolePicker)
      } else {
        println("call ignored")
        stay
      }
    }

    case Event(Craftsman, p @ DoOnce(playerMane)) => {
      //sender wants to be craftsman
      if (sender == playerMane) {
        giveRolePickerCard(Craftsman)
        gameState.craft
        sender ! SelectGoodToProduce
        goto(CraftsmanProcess) using p
      } else stay
    }
  }

  when(CraftsmanProcess){
    case Event(GoodSelected(good), DoOnce(playerMane)) => {
      if (sender == playerMane) {
        if (gameState.canGetGood(good)) {
          gameState.currentPlayer.goods(good) += 1
          goto(RoleProcess) using DoOnce(getNextRolePicker)
        } else stay
      } else stay
    }

    case Event(NoneSelected, DoOnce(playerMane)) => {
      if (sender == playerMane)
        goto(RoleProcess) using DoOnce(getNextRolePicker)
      else stay
    }
  }

  whenUnhandled {
    case Event(GameStateQuery, d) => {
      sender ! gameState.otherPlayer.doubloons
      stay
    }
    case Event(e, s) => 
      log.warning("received unhandled request ${e}, data ${s}")
      stay
  }
  initialize()
  
}



