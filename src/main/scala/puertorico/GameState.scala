
package puertorico
import scala.collection.mutable.HashMap
import scala.util.Random


class GameState {

  val playerOne = new PlayerState
  val playerTwo = new PlayerState
  var currentPlayer = playerOne
  var otherPlayer = playerTwo
  var governor = playerOne
  var rolePicker = playerOne

  val victoryPointsMax = 75
  def victoryPointsUsed = playerOne.victoryPoints + playerTwo.victoryPoints
  def victoryPointsLeft = (victoryPointsMax - victoryPointsUsed) max 0

  val colonistsMax = 75
  def colonistsUsed = playerOne.colonistsUsed + playerTwo.colonistsUsed
  def colonistsLeft = (colonistsMax - colonistsUsed) max 0 
  def colonistsOnBoat = 2 max {
    playerOne.buildings.colonistsNeeded + playerTwo.buildings.colonistsNeeded
  }

  val ships = List(new Ship(4), new Ship(6))
  val wharf = new Ship(100)

  val tradingHouse = new TradingHouse
  val plantationsVisible = new PlantationBundle(1,1,1,1,1,1)
  val plantationsDiscarded = PlantationBundle.empty
  val plantationsMax = new PlantationBundle(5,24,24,24,24,24)
  val goodsRemain = new GoodBundle(7,7,7,7,7)

  //List all goods
  val goodsAll = List[Good](Corn, Indigo, Sugar, Tobacco, Coffee)

  //List all buildings and buildings remaining

  val buildingsAll: Map[Building, Int] = Map(
    (SmallIndigo, 2), (SmallSugar, 2), (SmallMarket, 1), (Hacienda, 1), (ConstructionHut, 1), (SmallWarehouse, 1),
    (BigIndigo, 2), (BigSugar, 2), (BigMarket, 1), (BigWarehouse, 1), (Hospice, 1), (Office, 1),
    (BigCoffee, 2), (BigTobacco, 2), (Factory, 1), (University, 1), (Wharf, 1), (Harbor, 1), 
    (TownHall, 1), (Residence, 1), (Fortress, 1), (GuildHall, 1), (CustomHouse, 1) )

  val buildingsRemaining = new HashMap[Building, Int]

  //List all roles and $ on role

  val rolesAll = List[Role](Prospector, Mayor, Captain, Craftsman, Settler, Builder, Trader)
  //-1 means chosen in this round
  val rolesDoubloons: HashMap[Role, Int] = new HashMap[Role, Int] 
  rolesDoubloons ++= rolesAll map (_ -> 0)

  //Logic functions (Rule check)
  def isEndGame: Boolean = {
    colonistsLeft == 0 ||
    victoryPointsLeft == 0 || 
    playerOne.buildings.spaceRemaining == 0 ||
    playerTwo.buildings.spaceRemaining == 0
  }

  //Role getting logic
  def canGetRole(role: Role): Boolean = rolesDoubloons(role) > -1
  def getRole(role: Role) = {
    val doubloonOnRole = rolesDoubloons(role)
    rolesDoubloons(role) = -1
    doubloonOnRole
  }
  def resetRoles = rolesDoubloons.keys foreach {
    role => rolesDoubloons(role) += 1
  }
  def nextPlayerPickRoles = {
    val tmp = currentPlayer
    currentPlayer = otherPlayer
    otherPlayer = tmp
    rolePicker = currentPlayer
    //start new round if needed
    val rolesRemain = rolesDoubloons.count(_._2 == -1)
    if (rolesRemain < 3){
      resetRoles
      governor = currentPlayer
    }
  }

  //Craftsman logic
  def canGetGood(good: Good): Boolean = 
    currentPlayer.productionBundle(good) > 0 && goodsRemain(good) > 0
  
  def craft = goodsAll foreach {
    good => { 
      val p1produce = currentPlayer.productionBundle(good) min goodsRemain(good)
      currentPlayer.goods(good) += p1produce
      goodsRemain(good) -= p1produce
      val p2produce = otherPlayer.productionBundle(good) min goodsRemain(good)
      otherPlayer.goods(good) += p2produce
      goodsRemain(good) -= p2produce
    }
  }

  //Trader logic
  def canTradeGood(good: Good): Boolean = 
    currentPlayer.goods(good) > 0 && !tradingHouse.isFull && 
   (!tradingHouse.contains(good) || currentPlayer.hasActiveBuilding(Office))
  
  //Settler logic
  def canGetPlantation(plant: Plantation): Boolean = 
    plantationsVisible(plant) > 0 &&
      currentPlayer.island.spaceRemaining > 0 &&
      (currentPlayer == rolePicker || currentPlayer.hasActiveBuilding(ConstructionHut))

  def canGetPlantationExtra(plant: Plantation) = currentPlayer.island.spaceRemaining > 0 
  def getRandomPlantation = {
    val int = Random.nextInt(5)
    int match {
      case 0 => Corn
      case 1 => Indigo
      case 2 => Sugar
      case 3 => Tobacco
      case 4 => Coffee
    }
  }
  //TODO: handle the case of hospice 

  //Mayor logic
  def colonistsPerPlayer: (Int, Int) = {
    val secondPlayerGet = colonistsOnBoat/2
    val firstPlayerGet = colonistsOnBoat - secondPlayerGet
    (firstPlayerGet, secondPlayerGet)
  }

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
  def canShipGoods(good: Good, ship: Ship): Boolean = {
    currentPlayer.goods(good) > 0 && ship.maxLoadable(good) > 0 
    //TODO: need to check that no other ships has this good
  }

  def canKeepGoods(player: PlayerState, goodList: List[(Good, Int)]): Boolean = {
    //has the right number of goods
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
  

}


