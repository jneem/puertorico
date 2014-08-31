package puertorico
import scala.collection.mutable.HashMap

class IslandState {
  val size: Int = 12
  val plantations = PlantationBundle.empty
  val colonistsPlantation = PlantationBundle.empty

  def spaceRemaining: Int = size - plantations.sum
  def colonistsMax = plantations.sum
  def colonistsUsed = colonistsPlantation.sum
}


class BuildingBundle[T <: Building] {
  val buildingMap = new HashMap[T, Int]
  def spaceUsed = (buildingMap.keys.toList map (_.size)).sum
  def colonistsUsed = buildingMap.values.sum
  def colonistsMax = (buildingMap.keys.toList map (_.colonistsMax)).sum
}


class BuildingState {
  val size: Int = 12
  val productionBuildings = new BuildingBundle[ProductionBuilding]
  val purpleBuildings = new BuildingBundle[PurpleBuilding]
  
  def spaceUsed = productionBuildings.spaceUsed + purpleBuildings.spaceUsed
  def spaceRemaining = size - spaceUsed

  /**
   * Tally active production by good type.
   */
  def productionBundle: GoodBundle = {
    var pbundle = GoodBundle.empty
    productionBuildings.buildingMap foreach {
      case (building, colonists) => pbundle(building.good) += colonists
    }
    pbundle
  }

  def colonistsMax = productionBuildings.colonistsMax + purpleBuildings.colonistsMax
  def colonistsUsed = productionBuildings.colonistsUsed + purpleBuildings.colonistsUsed
  def colonistsNeeded = colonistsMax - colonistsUsed
}


class PlayerState {
  var island = new IslandState
  var buildings = new BuildingState
  var goods = GoodBundle.empty

  var victoryPoints = 0
  var doubloons = 0
  var colonistsSpare = 0 // colonists in San Juan

  def colonistsMax = colonistsSpare + island.colonistsMax + buildings.colonistsMax 
  def colonistsUsed = island.colonistsUsed + buildings.colonistsUsed
  
}

