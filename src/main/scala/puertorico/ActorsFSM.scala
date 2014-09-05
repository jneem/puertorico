package puertorico

import akka.actor.{Actor, ActorRef, FSM}
import scala.collection.immutable.HashMap

sealed trait State
case object WaitForStart extends State
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
case class DoOnce(player: ActorRef) extends Data
case class DoOnceEach(playerList: List[ActorRef]) extends Data
case class DoOnceUntilSuccess(playerSet: Set[ActorRef]) extends Data
case class DoUntilSuccess(playerList: List[ActorRef]) extends Data

case object GameStateQuery
case object StartGame


/**
  * Manages the game, implemented as a finite state machine
  * Has the true copy of gameState
  * The players maintain their own version of gameState locally, which get synced with RoleBoss' by 
  * message passing.
  * RoleBoss sends out gameState everytime it asks player to choose a role. 
  * TODO: may write a container version called gameData, to minimize amount of data passed around
  */

class RoleBoss(playerOne: ActorRef, playerTwo: ActorRef) extends Actor with FSM[State,Data] {

  val gameState = new GameState

  //map PlayerState to their actors, and vice versa
  val stateToPlayer = HashMap[PlayerState, ActorRef]((gameState.playerOneState, playerOne), (gameState.playerTwoState, playerTwo))
  val playerToState = HashMap[ActorRef, PlayerState]((playerOne, gameState.playerOneState), (playerTwo, gameState.playerTwoState))


  //order the players with rolePicker at head
  def playerOrder = gameState.orderPlayers map (stateToPlayer(_))

  def getNextRolePicker = {
    gameState.nextPlayerPickRoles
    val nextPlayer = stateToPlayer(gameState.rolePicker)
    nextPlayer ! ChooseRole
    nextPlayer
  }

  def endRole = goto(RoleProcess) using DoOnce(getNextRolePicker)
 
  //send state to SettlerProcessHacienda if possible
  def handleHacienda(pls: List[ActorRef]): State = {
    if (pls.isEmpty) endRole 
    else {
      val playerState = playerToState(pls.head)
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
      val playerState = playerToState(pls.head)
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
      val playerState = playerToState(pls.head)
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
      val playerState = playerToState(pls.head)
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
      val playerState = playerToState(pls.head)
      if (playerState.canAccomodateBuilding) {
        pls.head ! SelectBuilding
        goto(BuilderProcess) using DoOnceEach(pls)
      } else handleBuilder(pls.tail)
    }
  }

  def handleBuilderUniversity(pls: List[ActorRef]): State = {
    if (pls.isEmpty) endRole 
    else {
      val playerState = playerToState(pls.head)
      if (playerState.hasActiveBuilding(University)) {
        pls.head ! SelectColonist
        goto(BuilderProcessUniversity) using DoOnceEach(pls)
      } else handleBuilder(pls.tail)
    }
  }

  startWith(WaitForStart, DoOnce(playerOne))

  when(WaitForStart) {
    case Event(StartGame, d @ DoOnce(p)) => {
      p ! ChooseRole
      goto(RoleProcess) using d
    }
  }

  when(RoleProcess){
    case Event(RoleChosen(Prospector), DoOnce(player)) => {
      if (sender == player && gameState.isRoleAvailable(Prospector)) {
        gameState.givePickerRole(Prospector)
        gameState.doProspect
        stay using DoOnce(getNextRolePicker)
      } else stay
    }

    case Event(RoleChosen(Craftsman), p @ DoOnce(player)) => {
      if (sender == player && gameState.isRoleAvailable(Craftsman)) {
        gameState.givePickerRole(Craftsman)
        gameState.craft
        sender ! SelectGoodToProduce
        goto(CraftsmanProcess) using p
      } else stay
    }

    case Event(RoleChosen(Settler), DoOnce(player)) => {
      if (sender == player && gameState.isRoleAvailable(Settler)) {
        gameState.givePickerRole(Settler)
        handleHacienda(playerOrder)
      } else stay
    }

    case Event(RoleChosen(Trader), DoOnce(player)) => {
      if (sender == player) {
        gameState.givePickerRole(Trader)
        handleTrader(playerOrder)
      } else stay
    }


    case Event(RoleChosen(Builder), DoOnce(player)) => {
      if (sender == player){
        gameState.givePickerRole(Builder)
        handleBuilder(playerOrder)
      } else stay
    }

    case Event(RoleChosen(Mayor), DoOnce(player)) => {
      if(sender.equals(player)) {
        gameState.givePickerRole(Mayor)
        player ! SelectColonist
        goto(MayorProcess) using DoOnce(player)
      } else stay
    }

    /* More roles to implement
    case Event(Captain, DoOnce(player)) => {
      if(sender == player) {
        gameState.givePickerRole(Captain)
        //TODO
      }
    }*/

  }

  when(MayorProcess){

    case Event(NoneSelected, DoOnce(player)) => {
      if (sender == player){
        gameState.doMayor
        playerOne ! RearrangeColonists
        playerTwo ! RearrangeColonists
        stay using DoOnceUntilSuccess(Set(playerOne, playerTwo))
      } else stay
    }

    case Event(ColonistSelected, DoOnce(player)) => {
      if (sender == player) {
        val pl = playerToState(player)
        pl.colonistsSpare += 1
        playerOne ! RearrangeColonists
        playerTwo ! RearrangeColonists
        stay using DoOnceUntilSuccess(Set(playerOne, playerTwo))
      } else stay
    }

    case Event(ColonistsRearranged(colP, proB, purB, colS), DoOnceUntilSuccess(pset)) => {
      if (pset.contains(sender)){
        val pl = playerToState(sender) 
        if (gameState.isValidColonistsArrangement(pl, colP, proB, purB, colS)) {
          pl.assignColonistsArrangement(colP, proB, purB, colS)
          val pset2 = pset - sender
          if (pset2.isEmpty) endRole else stay using DoOnceUntilSuccess(pset2)
        } else stay
      } else stay
    }
  }

  when(BuilderProcess){

    case Event(NoneSelected, DoOnceEach(playerList)) => handleBuilder(playerList.tail)
    case Event(BuildingSelected(building), DoOnceEach(playerList)) => {
      val playerState = playerToState(playerList.head)
      if (sender == playerList.head && gameState.canBuild(building, playerState)) {
        gameState.giveBuilding(building, playerState)
        handleBuilderUniversity(playerList)
      } else stay
    }

  }

  when(BuilderProcessUniversity){
    case Event(NoneSelected, DoOnceEach(playerList)) => handleBuilder(playerList.tail)
    case Event(ColonistSelected, DoOnceEach(playerList)) => {
      val playerState = playerToState(playerList.head)
      playerState.addColonistByUniversity
      handleBuilder(playerList.tail)
    }
  }

  when(TraderProcess){

    case Event(NoneSelected, DoOnceEach(playerList)) => handleTrader(playerList.tail)

    case Event(GoodSelected(good), DoOnceEach(playerList)) => {
      val playerState = playerToState(playerList.head)
      if (sender == playerList.head && gameState.canTradeGood(good, playerState)) {
        gameState.doTrade(good, playerState)
        handleTrader(playerList.tail)
      } else stay
    }
  }

  when(SettlerProcessHacienda){
    case Event(NoneSelected, DoOnceEach(playerList)) => handleNoHacienda(playerList.tail)

    case Event(PlantationExtraAgreed, DoOnceEach(playerList)) => {
      if (sender == playerList.head) {
        val playerState = playerToState(playerList.head)
        val plant = gameState.getRandomPlantation
        playerState.addPlantation(plant)
        handleNoHacienda(playerList)
      } else stay
    }
  }

  when(SettlerProcess){
    case Event(NoneSelected, DoOnceEach(playerList)) => handleHacienda(playerList.tail)
    
    case Event(PlantationSelected(plant), DoOnceEach(playerList)) => {
      val playerState = playerToState(playerList.head)
      if (sender == playerList.head && gameState.canGetPlantation(plant, playerState)) {
        playerState.addPlantation(plant)
        handleHospice(playerList)
      } else stay
    }
  }

  when(SettlerProcessHospice){
    case Event(NoneSelected, DoOnceEach(playerList)) => handleHacienda(playerList.tail)

    case Event(ColonistSelected, DoOnceEach(playerList)) => {
      val playerState = playerToState(playerList.head)
      playerState.addColonistByHospice
      handleHacienda(playerList.tail)
    }
  }

  when(CraftsmanProcess){
    case Event(GoodSelected(good), DoOnce(player)) => {
      if (sender == player) {
        val pl = playerToState(player)
        if (gameState.canGetGood(good, pl)) {
          gameState.giveGood(good, pl)
          endRole
        } else stay
      } else stay
    }

    case Event(NoneSelected, DoOnce(player)) => {
      if (sender == player) endRole else stay
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
    /*
    case Event(e, s) => 
      log.warning(s"received unhandled request ${e}, data ${s}")
      stay
      */
  }
  initialize()
  
  //onTransition { }
}



