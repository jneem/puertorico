package org.puertorico

import akka.actor.{ Actor, ActorRef, FSM }
import scala.collection.immutable.{ HashMap => ImHashMap }
import akka.actor.LoggingFSM

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
 * Each player has an integer.
 * RoleBoss sends messages of the form (number, message)
 * TODO: may write a container version called gameData, to minimize amount of data passed around
 */

class RoleBoss(playerOne: ActorRef, playerTwo: ActorRef) extends Actor with LoggingFSM[State, Data] {

  override def logDepth = 2
  val gameState = new GameState

  //map PlayerState to their actors, and vice versa
  val stateToPlayer = ImHashMap[PlayerState, ActorRef]((gameState.playerOneState, playerOne), (gameState.playerTwoState, playerTwo))
  val playerToState = ImHashMap[ActorRef, PlayerState]((playerOne, gameState.playerOneState), (playerTwo, gameState.playerTwoState))

  //list of all players
  def allPlayers = playerToState.keys

  //assign each player an integer
  def playerNum: ImHashMap[ActorRef, Int] = gameState.playerNum map {
    case (playerState, num) => (stateToPlayer(playerState), num)
  }

  //order the players with rolePicker at head
  def playersOrder = gameState.orderPlayers map (stateToPlayer(_))

  def tellAll(player: ActorRef, message: Any) = {
    for (pl <- allPlayers) pl ! (playerNum(player), message)
    val pn = playerNum(player)
    //log.info(s"told all players that $pn got message $message")
  }

  def tellEach(message: Any) = {
    for (pl <- allPlayers) pl ! (playerNum(pl), message)
  }

  def getNextRolePicker = {
    gameState.nextPlayerPickRoles
    val nextPlayer = stateToPlayer(gameState.rolePicker)
    tellAll(nextPlayer, ChooseRole)
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
        tellAll(pls.head, SelectPlantationExtra)
        goto(SettlerProcessHacienda) using DoOnceEach(pls)
      } else handleNoHacienda(pls)
    }
  }

  //send state to SettlerProcess if possible
  def handleNoHacienda(pls: List[ActorRef]): State = {
    if (pls.isEmpty) endRole
    else {
      val playerState = playerToState(pls.head)
      if (gameState.canAccomodatePlantation(playerState)) {
        tellAll(pls.head, SelectPlantation)
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
        tellAll(pls.head, SelectColonist)
        goto(SettlerProcessHospice) using DoOnceEach(pls)
      } else handleHacienda(pls.tail)
    }
  }

  //send state to TraderProcess if possible
  def handleTrader(pls: List[ActorRef]): State = {
    if (pls.isEmpty) endRole
    else {
      val playerState = playerToState(pls.head)
      if (gameState.canTradeSomeGoods(playerState)) {
        tellAll(pls.head, SelectGoodToTrade)
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
        tellAll(pls.head, SelectBuilding)
        goto(BuilderProcess) using DoOnceEach(pls)
      } else handleBuilder(pls.tail)
    }
  }

  def handleBuilderUniversity(pls: List[ActorRef]): State = {
    if (pls.isEmpty) endRole
    else {
      val playerState = playerToState(pls.head)
      if (playerState.hasActiveBuilding(University)) {
        tellAll(pls.head, SelectColonist)
        goto(BuilderProcessUniversity) using DoOnceEach(pls)
      } else handleBuilder(pls.tail)
    }
  }

  def endCaptainRole: State = {
    gameState.clearShips
    //tell player to clear ships. Logic check is done by local gameState
    tellEach(GotShipToClear)
    endRole
  }

  def handleMayorStart: State = {
    val colPerPlayer = gameState.doMayor
    tellAll(playersOrder(0), GotColonists(colPerPlayer._1))
    tellAll(playersOrder(1), GotColonists(colPerPlayer._2))
    tellEach(RearrangeColonists)
    goto(MayorProcess) using DoOnceUntilSuccess(Set(playerOne, playerTwo))
  }

  def handleCaptain(pls: List[ActorRef]): State = {
    if (pls.isEmpty) {
      val playersStateToKeepGoods = gameState.orderPlayers.filter(_.goods.sum > 0)
      val playersToKeepGoods = (playersStateToKeepGoods map stateToPlayer).toSet
      if (playersToKeepGoods.isEmpty)
        endCaptainRole
      else {
        for (pl <- playersToKeepGoods) tellAll(pl, SelectGoodToKeep)
        goto(CaptainProcess) using DoOnceUntilSuccess(playersToKeepGoods)
      }
    } else {
      val player = pls.head
      val playerState = playerToState(player)
      if (gameState.canShipGoodsSomewhere(playerState)) {
        tellAll(player, SelectGoodToShip)
        goto(CaptainProcess) using DoUntilSuccess(pls)
      } else handleCaptain(pls.tail)
    }
  }

  startWith(WaitForStart, DoOnce(playerOne))

  when(WaitForStart) {
    case Event(StartGame, d @ DoOnce(p)) => {
      tellAll(p, ChooseRole)
      goto(RoleProcess) using d
    }
  }

  def handoutRole(player: ActorRef, role: Role) = {
    val money = gameState.givePickerRole(role)
    tellAll(player, GotRole(role))
    if (role == Prospector)
      tellAll(player, GotDoubloons(money + 1))
    else
      tellAll(player, GotDoubloons(money))
  }

  when(RoleProcess) {
    case Event(Prospector, DoOnce(player)) => {
      if (sender == player && gameState.isRoleAvailable(Prospector)) {
        handoutRole(player, Prospector)
        stay using DoOnce(getNextRolePicker)
      } else stay
    }

    case Event(Craftsman, p @ DoOnce(player)) => {
      if (sender == player && gameState.isRoleAvailable(Craftsman)) {
        handoutRole(player, Craftsman)
        val gbpair = gameState.craft
        tellAll(playerOne, GotGoods(gbpair._1))
        tellAll(playerTwo, GotGoods(gbpair._2))
        tellAll(player, SelectGoodToProduce)
        goto(CraftsmanProcess) using p
      } else stay
    }

    case Event(Settler, DoOnce(player)) => {
      if (sender == player && gameState.isRoleAvailable(Settler)) {
        handoutRole(player, Settler)
        handleHacienda(playersOrder)
      } else stay
    }

    case Event(Trader, DoOnce(player)) => {
      if (sender == player) {
        handoutRole(player, Trader)
        handleTrader(playersOrder)
      } else stay
    }

    case Event(Builder, DoOnce(player)) => {
      if (sender == player) {
        handoutRole(player, Builder)
        handleBuilder(playersOrder)
      } else stay
    }

    case Event(Mayor, DoOnce(player)) => {
      if (sender.equals(player)) {
        handoutRole(player, Mayor)
        tellAll(player, SelectColonist)
        goto(MayorProcess) using DoOnce(player)
      } else stay
    }

    case Event(Captain, DoOnce(player)) => {
      if (sender == player) {
        handoutRole(player, Captain)
        handleCaptain(playersOrder)
      } else stay
    }

  }

  when(CaptainProcess) {

    /*
     * Deal with player chose not to ship despite having something to ship
     * Only allowed if the only valid ship is player's own wharf
     */
    case Event(NoneSelected, DoUntilSuccess(playerList)) => {
      val player = playerList.head
      if (sender == player) {
        val playerState = playerToState(player)
        if (gameState.canShipWharfOnly(playerState))
          handleCaptain(playerList.tail)
        else {
          player ! (playerNum(player), SelectGoodToShip)
          stay
        }
      } else stay
    }

    case Event(GoodAndShipSelected(good, ship), DoUntilSuccess(playerList)) => {
      val player = playerList.head
      val playerState = playerToState(player)
      if (sender == player) {
        if (gameState.canShipGoods(good, ship, playerState)) {
          val (numGood, victoryPoints) = gameState.doShipGoods(good, ship, playerState)
          tellAll(player, GotGoodShipped(good, numGood))
          tellAll(player, GotVictoryPoints(victoryPoints))
          handleCaptain(playerList.tail :+ player)
        } else {
          player ! (playerNum(player), SelectGoodToShip)
          stay
        }
      } else stay
    }

    case Event(GoodToKeepSelected(goodList), DoOnceUntilSuccess(playerSet)) => {
      if (playerSet.contains(sender)) {
        val pl = playerToState(sender)
        if (gameState.canKeepGoods(pl, goodList)) {
          val goodThrown = gameState.doKeepGoods(pl, goodList)
          tellAll(sender, GotGoodToKeep(goodList))
          val pset2 = playerSet - sender
          if (pset2.isEmpty) endCaptainRole else stay using DoOnceUntilSuccess(pset2)
        } else {
          sender ! (playerNum(sender), SelectGoodToKeep)
          stay
        }
      } else stay

    }
  }

  when(MayorProcess) {

    case Event(NoneSelected, DoOnce(player)) => {
      if (sender == player) handleMayorStart else stay
    }

    case Event(ColonistSelected, DoOnce(player)) => {
      if (sender == player) {
        val plState = playerToState(player)
        plState.colonistsSpare += 1
        tellAll(sender, GotColonists(1))
        handleMayorStart
      } else stay
    }

    case Event(ColonistsRearranged(colP, proB, purB, colS), DoOnceUntilSuccess(pset)) => {
      if (pset.contains(sender)) {
        val pl = playerToState(sender)
        if (gameState.isValidColonistsArrangement(pl, colP, proB, purB, colS)) {
          pl.assignColonistsArrangement(colP, proB, purB, colS)
          tellAll(sender, GotColonistsRearranged(colP, proB, purB, colS))
          val pset2 = pset - sender
          if (pset2.isEmpty) endRole else stay using DoOnceUntilSuccess(pset2)
        } else {
          sender ! (playerNum(sender), RearrangeColonists)
          stay
        }
      } else stay
    }
  }

  when(BuilderProcess) {

    case Event(NoneSelected, DoOnceEach(playerList)) => handleBuilder(playerList.tail)
    case Event(BuildingSelected(building), DoOnceEach(playerList)) => {
      val player = playerList.head
      val playerState = playerToState(player)
      if (sender == player && gameState.canBuild(building, playerState)) {
        val buildingCost = gameState.giveBuilding(building, playerState)
        tellAll(player, GotBuilding(building))
        tellAll(player, GotDoubloons(-buildingCost))
        handleBuilderUniversity(playerList)
      } else stay
    }

  }

  when(BuilderProcessUniversity) {
    case Event(NoneSelected, DoOnceEach(playerList)) => handleBuilder(playerList.tail)
    case Event(ColonistSelected, DoOnceEach(playerList)) => {
      val player = playerList.head
      val playerState = playerToState(player)
      playerState.addColonistByUniversity
      tellAll(player, GotColonistByUniversity)
      handleBuilder(playerList.tail)
    }
  }

  when(TraderProcess) {

    case Event(NoneSelected, DoOnceEach(playerList)) => handleTrader(playerList.tail)

    case Event(GoodSelected(good), DoOnceEach(playerList)) => {
      val player = playerList.head
      val playerState = playerToState(player)
      if (sender == player && gameState.canTradeGood(good, playerState)) {
        val money = gameState.doTrade(good, playerState)
        tellAll(player, GotTrade(good))
        tellAll(player, GotDoubloons(money))
        handleTrader(playerList.tail)
      } else stay
    }
  }

  when(SettlerProcessHacienda) {
    case Event(NoneSelected, DoOnceEach(playerList)) => handleNoHacienda(playerList.tail)

    case Event(PlantationExtraAgreed, DoOnceEach(playerList)) => {
      val player = playerList.head
      if (sender == player) {
        val playerState = playerToState(player)
        val plant = gameState.getRandomPlantation
        playerState.addPlantation(plant)
        tellAll(player, GotPlantation(plant))
        handleNoHacienda(playerList)
      } else stay
    }
  }

  when(SettlerProcess) {
    case Event(NoneSelected, DoOnceEach(playerList)) => handleHacienda(playerList.tail)

    case Event(PlantationSelected(plant), DoOnceEach(playerList)) => {
      val player = playerList.head
      val playerState = playerToState(player)
      if (sender == player && gameState.canGetPlantation(plant, playerState)) {
        gameState.givePlantationToPlayer(plant, playerState)
        tellAll(player, GotPlantation(plant))
        handleHospice(playerList)
      } else stay
    }
  }

  when(SettlerProcessHospice) {
    case Event(NoneSelected, DoOnceEach(playerList)) => handleHacienda(playerList.tail)

    case Event(ColonistSelected, DoOnceEach(playerList)) => {
      val player = playerList.head
      val playerState = playerToState(player)
      playerState.addColonistByHospice
      tellAll(player, GotColonistByHospice)
      handleHacienda(playerList.tail)
    }
  }

  when(CraftsmanProcess) {
    case Event(GoodSelected(good), DoOnce(player)) => {
      if (sender == player) {
        val pl = playerToState(player)
        if (gameState.canGetGood(good, pl)) {
          gameState.giveGood(good, pl)
          tellAll(player, GotGood(good))
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

