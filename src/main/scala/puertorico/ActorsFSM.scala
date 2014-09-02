package puertorico

import akka.actor.{Actor, ActorRef, FSM}
import scala.collection.immutable.HashMap

sealed trait State
case object RoleProcess extends State
case object CraftsmanProcess extends State
case object SettlerProcess extends State
case object TraderProcess extends State
case object MayorProcess extends State
case object CaptainProcess extends State
case object BuilderProcess extends State

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


  def playerManeOrder = List(stateToMane(gameState.currentPlayer), stateToMane(gameState.otherPlayer))

  def getNextRolePicker = {
    gameState.nextPlayerPickRoles
    val nextPlayerMane = stateToMane(gameState.rolePicker)
    nextPlayerMane ! ChooseRole
    nextPlayerMane
  }

  def endRole = goto(RoleProcess) using DoOnce(getNextRolePicker)
  
  //determines the message to send to the second player in Settler phase
  def secondSettler = {
    gameState.considerNextPlayer
    if (gameState.canAccomodatePlantation) {
      val pl = stateToMane(gameState.currentPlayer)
      if (gameState.canGetPlantationExtra) 
        pl ! SelectPlantationExtra
      else
        pl ! SelectPlantation
      goto(SettlerProcess) using DoOnce(pl)
    } else {
      //TODO: regenerate the plantation tiles
      endRole
    }
  }

  def secondTrader = {
    gameState.considerNextPlayer
    if (gameState.canTradeAnyGood) {
      val pl = stateToMane(gameState.currentPlayer)
      pl ! SelectGoodToTrade
      goto(TraderProcess) using DoOnce(pl)
    } else {
      //TODO: empty the trading house if needed
      endRole
    }
  }

  def secondBuilder = {
    gameState.considerNextPlayer
    if (gameState.canAccomodateBuilding) {
      val pl = stateToMane(gameState.currentPlayer)
      pl ! SelectBuilding
      goto(BuilderProcess) using DoOnce(pl)
    } else endRole
  }

  startWith(RoleProcess, DoOnce(playerOneMane))

  when(RoleProcess){
    case Event(Prospector, DoOnce(playerMane)) => {
      if (sender == playerMane && gameState.canGetRole(Prospector)) {
        gameState.givePickerRole(Prospector)
        gameState.rolePicker.doubloons += 1
        stay using DoOnce(getNextRolePicker)
      } else stay
    }

    case Event(Craftsman, p @ DoOnce(playerMane)) => {
      if (sender == playerMane && gameState.canGetRole(Craftsman)) {
        gameState.givePickerRole(Craftsman)
        gameState.craft
        sender ! SelectGoodToProduce
        goto(CraftsmanProcess) using p
      } else stay
    }

    case Event(Settler, DoOnce(playerMane)) => {
      if (sender == playerMane && gameState.canGetRole(Settler)) {
        gameState.givePickerRole(Settler)

        if (gameState.canAccomodatePlantation) {
          if (gameState.canGetPlantationExtra)
           stateToMane(gameState.currentPlayer) ! SelectPlantationExtra
         else 
           stateToMane(gameState.currentPlayer) ! SelectPlantation
       
          goto(SettlerProcess) using DoOnceEach(playerManeOrder)
        } else secondSettler
      } else stay
    }

    case Event(Trader, DoOnce(playerMane)) => {
      if (sender == playerMane) {
        gameState.givePickerRole(Trader)
        if (gameState.canTradeAnyGood) {
          playerMane ! SelectGoodToTrade
          goto(TraderProcess) using DoOnceEach(playerManeOrder)
        } else secondTrader
      } else stay
    }


    case Event(Builder, DoOnce(playerMane)) => {
      if (sender == playerMane){
        gameState.givePickerRole(Builder)
        if(gameState.canAccomodateBuilding){
          playerMane ! SelectBuilding
          goto(BuilderProcess) using DoOnceEach(playerManeOrder)
        } else secondBuilder
      } else stay
    }

    case Event(Mayor, DoOnce(playerMane)) => {
      if(sender.equals(playerMane)) {
        gameState.givePickerRole(Mayor)
        playerMane ! SelectColonist
        playerOneMane ! RearrangeColonists
        playerTwoMane ! RearrangeColonists
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
        stay using DoOnceUntilSuccess(Set(playerOneMane, playerTwoMane))
      } else stay
    }

    case Event(ColonistSelected, DoOnce(playerMane)) => {
      if (sender == playerMane) {
        gameState.currentPlayer.colonistsSpare += 1
        stay using DoOnceUntilSuccess(Set(playerOneMane, playerTwoMane))
      } else stay
    }

    case Event(ColonistsRearranged(colP, proB, purB, colS), DoOnceUntilSuccess(pset)) => {
      if (pset.contains(sender)){
        val pl = maneToState(sender) 
        if (gameState.isValidColonistsArrangement(pl, colP, proB, purB, colS)) {
          gameState.assignColonistsArrangement(pl, colP, proB, purB, colS)
          val pset2 = pset - sender
          if (pset2.isEmpty) endRole else stay using DoOnceUntilSuccess(pset2)
        } else stay
      } else stay
    }
  }

  when(BuilderProcess){

    case Event(NoneSelected, DoOnceEach(_)) => secondBuilder
    
    case Event(NoneSelected, DoOnce(playerMane)) => endRole

    case Event(BuildingSelected(building), DoOnceEach(playerManeList)) => {
      if (sender == playerManeList.head && gameState.canBuild(building)) {
        gameState.currentPlayer.addBuilding(building)
        secondBuilder
      } else stay
    }

    case Event(BuildingSelected(building), DoOnce(playerMane)) => {
      if(sender == playerMane && gameState.canBuild(building)){
        gameState.currentPlayer.addBuilding(building)
        endRole
      } else stay
    }

  }

  when(TraderProcess){

    case Event(NoneSelected, DoOnceEach(_)) => secondTrader

    case Event(NoneSelected, DoOnce(playerMane)) => endRole

    case Event(GoodSelected(good), DoOnceEach(playerManeList)) => {
      if (sender == playerManeList.head && gameState.canTradeGood(good)) {
        //TODO: convert the good to money
        secondTrader
      } else stay
    }

    case Event(GoodSelected(good), DoOnce(playerMane)) => {
      if(sender == playerMane && gameState.canTradeGood(good)){
        //TODO: convert the good to money
        endRole
      } else stay
    }

  }

  when(SettlerProcess){
    //TODO: add a flag to check if Hacidena effect has been taken into account
    //to avoid concurrency problems
    case Event(NoneSelected, DoOnceEach(_)) => secondSettler

    case Event(NoneSelected, DoOnce(playerMane)) => endRole
    
    case Event(PlantationSelected(plant), DoOnceEach(playerManeList)) => {
      if (sender == playerManeList.head && gameState.canGetPlantation(plant)) {
        gameState.currentPlayer.island.plantations(plant) += 1
        secondSettler
      } else stay
    }

    case Event(PlantationSelected(plant), DoOnce(playerMane)) => {
      if (sender == playerMane && gameState.canGetPlantation(plant)) {
        gameState.currentPlayer.island.plantations(plant) += 1
        endRole
      } else stay
    }

    case Event(PlantationExtraSelected(plant), DoOnceEach(playerManeList)) => {
      if (sender == playerManeList.head && gameState.canGetPlantation(plant)) {
        gameState.currentPlayer.island.plantations(plant) += 1
        if (gameState.canAccomodatePlantation){
          sender ! SelectPlantation
          stay using DoOnceEach(playerManeList)
        } else secondSettler
      } else stay
    }

    case Event(PlantationExtraSelected(plant), DoOnce(playerMane)) => {
      if (sender == playerMane && gameState.canGetPlantation(plant)) {
        gameState.currentPlayer.island.plantations(plant) += 1
        if (gameState.canAccomodatePlantation) {
          sender ! SelectPlantation
          stay using DoOnce(playerMane)
        } else endRole
      } else stay
    }

  }


  when(CraftsmanProcess){
    case Event(GoodSelected(good), DoOnce(playerMane)) => {
      if (sender == playerMane) {
        if (gameState.canGetGood(good)) {
          gameState.currentPlayer.goods(good) += 1
          endRole
        } else stay
      } else stay
    }

    case Event(NoneSelected, DoOnce(playerMane)) => {
      if (sender == playerMane)
        endRole
      else stay
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
  
}



