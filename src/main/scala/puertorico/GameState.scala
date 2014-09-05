
package puertorico
import scala.collection.mutable.HashMap
import scala.util.Random


class GameState {

  import GameState._

  val playerOneState = new PlayerState
  val playerTwoState = new PlayerState
  //will soon be obsolete, replaced by getPlayer
  var governor = playerOneState
  var rolePicker = playerOneState

  val victoryPointsMax = 75
  def victoryPointsUsed = playerOneState.victoryPoints + playerTwoState.victoryPoints
  def victoryPointsLeft = (victoryPointsMax - victoryPointsUsed) max 0

  val colonistsMax = 75
  def colonistsUsed = playerOneState.colonistsUsed + playerTwoState.colonistsUsed
  def colonistsLeft = (colonistsMax - colonistsUsed) max 0 
  def colonistsOnBoat = 2 max {
    playerOneState.buildings.colonistsNeeded + playerTwoState.buildings.colonistsNeeded
  }

  val ships = List[Ship](new Ship(4), new Ship(6))
  val wharf = new Ship(100)
  def otherShip(ship: Ship) = ships.filter(_ != ship).head

  val tradingHouse = new TradingHouse
  val plantationsVisible = new PlantationBundle(1,1,1,1,1,1)
  val plantationsDiscarded = PlantationBundle.empty
  val plantationsMax = new PlantationBundle(5,24,24,24,24,24)
  val goodsRemain = new GoodBundle(7,7,7,7,7)

  //List goods price
  val goodsPrice: Map[Good, Int] = Map(
    Corn -> 0,
    Indigo -> 1,
    Sugar -> 2,
    Tobacco -> 3,
    Coffee -> 4 )

  //List all buildings and buildings remaining

  val buildingsAll: Map[Building, Int] = Map(
    (SmallIndigo, 2), (SmallSugar, 2), (SmallMarket, 1), (Hacienda, 1), (ConstructionHut, 1), (SmallWarehouse, 1),
    (BigIndigo, 2), (BigSugar, 2), (BigMarket, 1), (BigWarehouse, 1), (Hospice, 1), (Office, 1),
    (BigCoffee, 2), (BigTobacco, 2), (Factory, 1), (University, 1), (Wharf, 1), (Harbor, 1), 
    (TownHall, 1), (Residence, 1), (Fortress, 1), (GuildHall, 1), (CustomsHouse, 1) )

  val buildingsRemaining = new HashMap[Building, Int]
  buildingsRemaining ++= buildingsAll

  //List all roles and $ on role

  val rolesAll = List[Role](Prospector, Mayor, Captain, Craftsman, Settler, Builder, Trader)
  //-1 means chosen in this round
  val rolesDoubloons: HashMap[Role, Int] = new HashMap[Role, Int] 
  rolesDoubloons ++= rolesAll map (_ -> 0)

  //Logic functions (Rule check)
  def isEndGame: Boolean = {
    colonistsLeft == 0 ||
    victoryPointsLeft == 0 || 
    playerOneState.buildings.spaceRemaining == 0 ||
    playerTwoState.buildings.spaceRemaining == 0
  }

  //Role getting logic

  def orderPlayers: List[PlayerState] = {
    if (rolePicker == playerOneState) List(playerOneState, playerTwoState) else List(playerTwoState, playerOneState)
  }
  
  def isRoleAvailable(role: Role): Boolean = rolesDoubloons(role) > -1

  def givePickerRole(role: Role){
      rolePicker.doubloons += rolesDoubloons(role)
      rolesDoubloons(role) = -1
  }
  def resetRoles = {
    for (role <- rolesDoubloons.keys) rolesDoubloons(role) += 1 
    //tell players to reset temporary parameters
    for(playerState <- orderPlayers) playerState.resetTemporaryParam
  }
  
  def nextPlayerPickRoles = {
    rolePicker = orderPlayers(1)
    //start new round if needed
    val rolesRemain = rolesDoubloons.count(_._2 == -1)
    if (rolesRemain < 3){
      resetRoles
      governor = rolePicker
    }
  }

  def doProspect = rolePicker.doubloons += 1

  //Craftsman logic
  def canGetGood(good: Good, currentPlayerState: PlayerState): Boolean = 
    currentPlayerState.productionBundle(good) > 0 && goodsRemain(good) > 0
  
  def craft = for { good <- goodsAll ; player <- orderPlayers} {
    val produce = player.productionBundle(good) min goodsRemain(good)
    player.goods(good) += produce
    goodsRemain(good) -= produce
  }

  def giveGood(good: Good, pl: PlayerState) = {
    pl.goods(good) += 1
    goodsRemain(good) -= 1
  }

  //Builder logic
  
  def canBuild(b: Building, pl: PlayerState) = pl.buildings.spaceRemaining >= b.size && 
    !pl.hasBuilding(b) && pl.doubloons >= b.cost && buildingsRemaining(b) > 0

  def giveBuilding(b: Building, pl: PlayerState) = {
    pl.addBuilding(b)
    pl.doubloons -= b.cost
    buildingsRemaining(b) -= 1
  }

  //Trader logic
  def canTradeSomeGoods(currentPlayerState: PlayerState) = {
    val tradeAny = goodsAll map (good => canTradeGood(good, currentPlayerState))
    tradeAny reduce (_ || _)
  }

  def canTradeGood(good: Good, currentPlayerState: PlayerState): Boolean = 
    currentPlayerState.goods(good) > 0 && !tradingHouse.isFull && 
   (!tradingHouse.contains(good) || currentPlayerState.hasActiveBuilding(Office))

  def doTrade(good: Good, pl: PlayerState) = {
    pl.goods(good) -= 1
    tradingHouse.goods(good) += 1
    pl.doubloons += goodsPrice(good)
    if (rolePicker == pl) pl.doubloons += 1
  }
  
  //Settler logic
  def canGetPlantation(plant: Plantation, playerState: PlayerState): Boolean = 
    plantationsVisible(plant) > 0 &&
      playerState.island.spaceRemaining > 0 &&
      (playerState == rolePicker || playerState.hasActiveBuilding(ConstructionHut))

  def canAccomodatePlantation(playerState: PlayerState) = playerState.island.spaceRemaining > 0

  def getRandomPlantation = {
    val int = Random.nextInt(5)
    int match {
      case 0 => CornPlantation
      case 1 => IndigoPlantation
      case 2 => SugarPlantation
      case 3 => TobaccoPlantation
      case 4 => CoffeePlantation
    }
  }

  //Mayor logic
  def colonistsPerPlayer: (Int, Int) = {
    val secondPlayerGet = colonistsOnBoat/2
    val firstPlayerGet = colonistsOnBoat - secondPlayerGet
    (firstPlayerGet, secondPlayerGet)
  }

  def doMayor = {
    val cpl = colonistsPerPlayer
    val playerOrder = orderPlayers
    playerOrder(0).colonistsSpare += cpl._1
    playerOrder(1).colonistsSpare += cpl._2
  }

  //since this is done by the server, better to leave in gameState
  //although, this can be done in PlayerState
  def isValidColonistsArrangement(player: PlayerState, colonistsPlantation: PlantationBundle, 
                                  productionBuildings: List[(ProductionBuilding, Int)], 
                                  purpleBuildings: List[(PurpleBuilding, Int)], 
                                  colonistsSpare: Int): Boolean = {
    
    //total number of colonists is preserved
    val prodCol = (productionBuildings map (_._2)).sum
    val purpCol = (purpleBuildings map (_._2)).sum
    val planCol = colonistsPlantation.sum
    val totalOk = colonistsSpare + planCol + prodCol + purpCol  == player.colonistsUsed + player.colonistsSpare
    //each building is present and does not exceed capacity
    val prodB = productionBuildings map {
      case (building, colonist) => player.hasBuilding(building) && building.colonistsMax >= colonist
    }
    val purpB = purpleBuildings map {
      case (building, colonist) => player.hasBuilding(building) && building.colonistsMax >= colonist
    }
    val prodOk = prodB.reduce(_ && _) 
    val purpOk = purpB.reduce(_ && _)
    totalOk && prodOk && purpOk
  }

  //Captain logic
  def canShipGoods(good: Good, ship: Ship, currentPlayerState: PlayerState): Boolean = {
    val theOtherShip = otherShip(ship)
    val isBestBoat = !ship.good.isEmpty || !theOtherShip.good.isEmpty || theOtherShip.maxLoadable(good) < ship.maxLoadable(good)
    currentPlayerState.goods(good) > 0 && ship.maxLoadable(good) > 0 && isBestBoat
  }

  def canKeepGoods(player: PlayerState, goodList: List[(Good, Int)]): Boolean = {
    //has the right number of good
    val goodCount = goodList map {
      case (good, count) => player.goods(good) >= count
    }
    val goodCountOk = goodCount.reduce(_ && _)
    
    //tally types of goods can keep with warehouse
    val swNum = if (player.hasBuilding(SmallWarehouse)) 1 else 0
    val bwNum = if (player.hasBuilding(BigWarehouse)) 2 else 0
    val warehouseSpace = swNum + bwNum
    
    val spaceOk = goodList.length match {
      case 1 => warehouseSpace > 0 || goodList.head._2 == 1
      case 2 => warehouseSpace > 1 || (warehouseSpace == 1 && goodList.count(_._2 == 1) > 0)
      case 3 => warehouseSpace > 2 || (warehouseSpace == 2 && goodList.count(_._2 == 1) > 0)
      case 4 => warehouseSpace == 3 && goodList.count(_._2 == 1) > 0
    }

    goodCountOk && spaceOk
  }


}

object GameState {
  
  //List all goods and plantations
  val goodsAll = List[Good](Corn, Indigo, Sugar, Tobacco, Coffee)
  val plantationsAll = List[Plantation](Quarry, CornPlantation, IndigoPlantation, 
    SugarPlantation, TobaccoPlantation, CoffeePlantation)

}


