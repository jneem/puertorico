package puertorico
import scala.collection.mutable.MutableList

class IslandState {
  val size: Int = 12
  var plantationCount = GoodBundle.empty
  var colonistCount = GoodBundle.empty
  var quarryCount: Int = 0
  var quarryColonistCount: Int = 0

  def spaceRemaining: Int = size - plantationCount.sum - quarryCount
}

class BuildingState {
  val size: Int = 12
  val productionBuildings = new MutableList[(ProductionBuilding, Int)]
  val purpleBuildings = new MutableList[(PurpleBuilding, Int)]
  
  def spaceUsed = (productionBuildings map (_._1.size)).sum + (purpleBuildings map (_._1.size)).sum
  def spaceRemaining = size - spaceUsed
  def productionBundle: GoodBundle = {
    var pbundle = GoodBundle.empty
    productionBuildings foreach {
      case (building, colonists) => pbundle(building.good) += colonists
    }
    pbundle
  }

  def colonistsMax = (productionBuildings map (_._1.colonistsMax)).sum + purpleBuildings.size
  def colonistsUsed = (productionBuildings map (_._2)).sum + (purpleBuildings map (_._2)).sum
  def colonistsNeeded = colonistsMax - colonistsUsed
}


class PlayerState {
  var island = new IslandState
  var buildings = new BuildingState
  var goods = GoodBundle.empty

  var victoryPoints = 0
  var doubloons = 0
  var spareColonists = 0 // colonists in San Juan
}
