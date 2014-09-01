package puertorico
import scala.collection.mutable.HashMap

class IslandState {
  val size: Int = 12
  val plantations = PlantationBundle.empty
  val colonistsPlantation = PlantationBundle.empty

  def spaceRemaining: Int = size - plantations.sum
  def colonistsMax = plantations.sum
  def colonistsUsed = colonistsPlantation.sum

  /**
   * Tally active production of raw materials by good type
   */
  def productionRawBundle: GoodBundle = colonistsPlantation.goodsOnly
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
  def colonistsNeeded = 0 max (colonistsMax - colonistsUsed)

  def hasActiveBuilding(b: Building): Boolean = b match {
    case (x: ProductionBuilding) => productionBuildings.buildingMap(x) > 0
    case (x: PurpleBuilding) => purpleBuildings.buildingMap(x) > 0
  }

  def hasBuilding(b: Building): Boolean = b match {
    case (x: ProductionBuilding) => productionBuildings.buildingMap.contains(x)
    case (x: PurpleBuilding) => purpleBuildings.buildingMap.contains(x)
  }
    
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

  /**
   * Tally active production by good type.
   */
  def productionBundle = GoodBundle(
    island.productionRawBundle(Corn), island.productionRawBundle(Indigo) min buildings.productionBundle(Indigo), island.productionRawBundle(Sugar) min buildings.productionBundle(Sugar), island.productionRawBundle(Tobacco) min buildings.productionBundle(Tobacco), island.productionRawBundle(Coffee) min buildings.productionBundle(Coffee)
    )

  def hasBuilding(b: Building): Boolean = buildings.hasBuilding(b)
  def hasActiveBuilding(b: Building): Boolean = buildings.hasActiveBuilding(b)

}

