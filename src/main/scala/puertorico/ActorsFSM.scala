package puertorico

import akka.actor.{Actor, ActorRef, FSM}
import scala.collection.immutable.HashMap

sealed trait State
case object RoleProcess extends State
case object CraftsmanProcess extends State
case object SettlerProcessHacienda extends State
case object SettlerProcessHospice extends State
case object SettlerProcess extends State
case object TraderProcess extends State
case object MayorProcess extends State
case object CaptainProcess extends State
case object BuilderProcess extends State
case object BuilderProcessUniversity extends State

sealed trait Data
case class DoOnce(playerMane: ActorRef) extends Data
case class DoOnceEach(playerManeList: List[ActorRef]) extends Data
case class DoOnceUntilSuccess(playerSet: Set[ActorRef]) extends Data
case class DoUntilSuccess(playerManeList: List[ActorRef]) extends Data

case object GameStateQuery


/**
  * Manages the game, implemented as a finite state machine
  * Has the true copy of gameState
  * The players maintain their own version of gameState locally, which get synced with RoleBoss' by 
  * message passing.
  * RoleBoss sends out gameState everytime it asks player to choose a role. 
  * TODO: may write a container version called gameData, to minimize amount of data passed around
  */

class RoleBoss(playerOneMane: ActorRef, playerTwoMane: ActorRef) extends Actor with FSM[State,Data] {

  val gameState = new GameState

  //map PlayerState to their actors, and vice versa
  val stateToMane = HashMap[PlayerState, ActorRef]((gameState.playerOne, playerOneMane), (gameState.playerTwo, playerTwoMane))
  val maneToState = HashMap[ActorRef, PlayerState]((playerOneMane, gameState.playerOne), (playerTwoMane, gameState.playerTwo))


  //Tan ordering on the players with rolePicker at head
  def playerManeOrder = gameState.orderPlayers map (stateToMane(_))

  def getNextRolePicker = {
    gameState.nextPlayerPickRoles
    val nextPlayerMane = stateToMane(gameState.rolePicker)
    nextPlayerMane ! ChooseRole
    nextPlayerMane
  }

  def endRole = goto(RoleProcess) using DoOnce(getNextRolePicker)
 
  //send state to SettlerProcessHacienda if possible
  def handleHacienda(pls: List[ActorRef]): State = {
    if (pls.isEmpty) endRole 
    else {
      val playerState = maneToState(pls.head)
      if (gameState.canAccomodatePlantation(playerState) && 
        playerState.canGetPlantationExtra) {
        pls.head ! SelectPlantationExtra
        goto(SettlerProcessHacienda) using DoOnceEach(pls)
      } else handleNoHacienda(pls.tail)
    }
  }

  //send state to SettlerProcess if possible
  def handleNoHacienda(pls: List[ActorRef]): State = {
    if (pls.isEmpty) endRole
    else {
      val playerState = maneToState(pls.head)
      if (gameState.canAccomodatePlantation(playerState)) {
        pls.head ! SelectPlantation
        goto(SettlerProcess) using DoOnceEach(pls)
      } else handleHacienda(pls.tail)
    }
  }

  //send state to SettlerHospice if possible
  def handleHospice(pls: List[ActorRef]): State = {
    if (pls.isEmpty) endRole
    else {
      val playerState = maneToState(pls.head)
      if (playerState.hasBuilding(Hospice)) {
        pls.head ! SelectColonist
        goto(SettlerProcessHospice) using DoOnceEach(pls)
      } else handleHacienda(pls.tail)
    }
  }


  //send state to TraderProcess if possible
  def handleTrader(pls: List[ActorRef]): State = {
    if (pls.isEmpty) endRole
    else {
      val playerState = maneToState(pls.head)
      if (gameState.canTradeSomeGoods(playerState)){
        pls.head ! SelectGoodToTrade
        goto(TraderProcess) using DoOnceEach(pls)
      } else handleTrader(pls.tail)
    }
  }

  //send state to BuilderProcess if possible
  def handleBuilder(pls: List[ActorRef]): State = {
    if (pls.isEmpty) endRole
    else {
      val playerState = maneToState(pls.head)
      if (playerState.canAccomodateBuilding) {
        pls.head ! SelectBuilding
        goto(BuilderProcess) using DoOnceEach(pls)
      } else handleBuilder(pls.tail)
    }
  }

  def handleBuilderUniversity(pls: List[ActorRef]): State = {
    if (pls.isEmpty) endRole 
    else {
      val playerState = maneToState(pls.head)
      if (playerState.hasActiveBuilding(University)) {
        pls.head ! SelectColonist
        goto(BuilderProcessUniversity) using DoOnceEach(pls)
      } else handleBuilder(pls.tail)
    }
  }

  startWith(RoleProcess, DoOnce(playerOneMane))

  when(RoleProcess){
    case Event(Prospector, DoOnce(playerMane)) => {
      if (sender == playerMane && gameState.isRoleAvailable(Prospector)) {
        gameState.givePickerRole(Prospector)
        gameState.rolePicker.doubloons += 1
        stay using DoOnce(getNextRolePicker)
      } else stay
    }

    case Event(Craftsman, p @ DoOnce(playerMane)) => {
      if (sender == playerMane && gameState.isRoleAvailable(Craftsman)) {
        gameState.givePickerRole(Craftsman)
        gameState.craft
        sender ! SelectGoodToProduce
        goto(CraftsmanProcess) using p
      } else stay
    }

    case Event(Settler, DoOnce(playerMane)) => {
      if (sender == playerMane && gameState.isRoleAvailable(Settler)) {
        gameState.givePickerRole(Settler)
        handleHacienda(playerManeOrder)
      } else stay
    }

    case Event(Trader, DoOnce(playerMane)) => {
      if (sender == playerMane) {
        gameState.givePickerRole(Trader)
        handleTrader(playerManeOrder)
      } else stay
    }


    case Event(Builder, DoOnce(playerMane)) => {
      if (sender == playerMane){
        gameState.givePickerRole(Builder)
        handleBuilder(playerManeOrder)
      } else stay
    }

    case Event(Mayor, DoOnce(playerMane)) => {
      if(sender.equals(playerMane)) {
        gameState.givePickerRole(Mayor)
        playerMane ! SelectColonist
        goto(MayorProcess) using DoOnce(playerMane)
      } else stay
    }

    /* More roles to implement
    case Event(Captain, DoOnce(playerMane)) => {
      if(sender == playerMane) {
        gameState.givePickerRole(Captain)
        //TODO
      }
    }*/

  }

  when(MayorProcess){

    case Event(NoneSelected, DoOnce(playerMane)) => {
      if (sender == playerMane){
        gameState.maymay
        playerOneMane ! RearrangeColonists
        playerTwoMane ! RearrangeColonists
        stay using DoOnceUntilSuccess(Set(playerOneMane, playerTwoMane))
      } else stay
    }

    case Event(ColonistSelected, DoOnce(playerMane)) => {
      if (sender == playerMane) {
        val pl = maneToState(playerMane)
        pl.colonistsSpare += 1
        playerOneMane ! RearrangeColonists
        playerTwoMane ! RearrangeColonists
        stay using DoOnceUntilSuccess(Set(playerOneMane, playerTwoMane))
      } else stay
    }

    case Event(ColonistsRearranged(colP, proB, purB, colS), DoOnceUntilSuccess(pset)) => {
      if (pset.contains(sender)){
        val pl = maneToState(sender) 
        if (gameState.isValidColonistsArrangement(pl, colP, proB, purB, colS)) {
          pl.assignColonistsArrangement(colP, proB, purB, colS)
          val pset2 = pset - sender
          if (pset2.isEmpty) endRole else stay using DoOnceUntilSuccess(pset2)
        } else stay
      } else stay
    }
  }

  when(BuilderProcess){

    case Event(NoneSelected, DoOnceEach(playerManeList)) => handleBuilder(playerManeList.tail)
    case Event(BuildingSelected(building), DoOnceEach(playerManeList)) => {
      val playerState = maneToState(playerManeList.head)
      if (sender == playerManeList.head && gameState.canBuild(building, playerState)) {
        gameState.giveBuilding(building, playerState)
        handleBuilderUniversity(playerManeList)
      } else stay
    }

  }

  when(BuilderProcessUniversity){
    case Event(NoneSelected, DoOnceEach(playerManeList)) => handleBuilder(playerManeList.tail)
    case Event(ColonistSelected, DoOnceEach(playerManeList)) => {
      val playerState = maneToState(playerManeList.head)
      playerState.addColonistByUniversity
      handleBuilder(playerManeList.tail)
    }
  }

  when(TraderProcess){

    case Event(NoneSelected, DoOnceEach(playerManeList)) => handleTrader(playerManeList.tail)

    case Event(GoodSelected(good), DoOnceEach(playerManeList)) => {
      val playerState = maneToState(playerManeList.head)
      if (sender == playerManeList.head && gameState.canTradeGood(good, playerState)) {
        //TODO: convert the good to money
        handleTrader(playerManeList.tail)
      } else stay
    }
  }

  when(SettlerProcessHacienda){
    case Event(NoneSelected, DoOnceEach(playerManeList)) => handleNoHacienda(playerManeList.tail)

    case Event(PlantationExtraAgreed, DoOnceEach(playerManeList)) => {
      if (sender == playerManeList.head) {
        val playerState = maneToState(playerManeList.head)
        val plant = gameState.getRandomPlantation
        playerState.addPlantation(plant)
        handleNoHacienda(playerManeList)
      } else stay
    }
  }

  when(SettlerProcess){
    case Event(NoneSelected, DoOnceEach(playerManeList)) => handleHacienda(playerManeList.tail)
    
    case Event(PlantationSelected(plant), DoOnceEach(playerManeList)) => {
      val playerState = maneToState(playerManeList.head)
      if (sender == playerManeList.head && gameState.canGetPlantation(plant, playerState)) {
        playerState.addPlantation(plant)
        handleHospice(playerManeList)
      } else stay
    }
  }

  when(SettlerProcessHospice){
    case Event(NoneSelected, DoOnceEach(playerManeList)) => handleHacienda(playerManeList.tail)

    case Event(ColonistSelected, DoOnceEach(playerManeList)) => {
      val playerState = maneToState(playerManeList.head)
      playerState.addColonistByHospice
      handleHacienda(playerManeList.tail)
    }
  }

  when(CraftsmanProcess){
    case Event(GoodSelected(good), DoOnce(playerMane)) => {
      if (sender == playerMane) {
        val pl = maneToState(playerMane)
        if (gameState.canGetGood(good, pl)) {
          gameState.giveGood(good, pl)
          endRole
        } else stay
      } else stay
    }

    case Event(NoneSelected, DoOnce(playerMane)) => {
      if (sender == playerMane) endRole else stay
    }
  }


  whenUnhandled {
    case Event(GameStateQuery, d) => {
      //upon request, send a copy of gameState over
      //TOFIX: should only send the data over.
      //Also, can query for: all game data, just player 1 data, just player 2 data
      sender ! gameState
      stay
    }
    case Event(e, s) => 
      log.warning("received unhandled request ${e}, data ${s}")
      stay
  }
  initialize()
  
  onTransition {
    case RoleProcess -> SettlerProcessHacienda => {

    }
  }
}



