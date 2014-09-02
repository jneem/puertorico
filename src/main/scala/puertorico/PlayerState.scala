package puertorico
import scala.collection.mutable.HashMap

//changed to case class for easy copy
case class IslandState(
  val plantations: PlantationBundle = PlantationBundle.empty, 
  val colonistsPlantation: PlantationBundle = PlantationBundle.empty) {

  val size: Int = 12

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

  //hard copy
  def copy = {
    val newone = new BuildingBundle[T]
    newone.buildingMap ++= buildingMap
    newone
  }

  //copy values from other
  def copyFrom(other: BuildingBundle[T]) = {
    buildingMap.clear()
    buildingMap ++= other.buildingMap
  }

  def copyFromList(other: List[(T, Int)]) = {
    buildingMap.clear()
    buildingMap ++= other
  }
}


class BuildingState {
  val size: Int = 12
  val productionBuildings = new BuildingBundle[ProductionBuilding]
  val purpleBuildings = new BuildingBundle[PurpleBuilding]
  
  def spaceUsed = productionBuildings.spaceUsed + purpleBuildings.spaceUsed
  def spaceRemaining = size - spaceUsed

  //hard copy
  def copy = {
    val newone = new BuildingState
    newone.productionBuildings.copyFrom(productionBuildings)
    newone.purpleBuildings.copyFrom(purpleBuildings)
    newone
  }

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

  def addBuilding(b: Building) = b match {
    case (x: ProductionBuilding) => productionBuildings.buildingMap(x) += 1
    case (x: PurpleBuilding) => purpleBuildings.buildingMap(x) += 1
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

  //Make hard copy
  def copy = {
    val newone = new PlayerState
    newone.island = island.copy()
    newone.buildings = buildings.copy
    newone.goods = goods.copy()
    newone.victoryPoints = victoryPoints
    newone.doubloons = doubloons
    newone.colonistsSpare = colonistsSpare
    newone
  }

  /**
   * Tally active production by good type.
   */
  def productionBundle = GoodBundle(
    island.productionRawBundle(Corn), island.productionRawBundle(Indigo) min buildings.productionBundle(Indigo), island.productionRawBundle(Sugar) min buildings.productionBundle(Sugar), island.productionRawBundle(Tobacco) min buildings.productionBundle(Tobacco), island.productionRawBundle(Coffee) min buildings.productionBundle(Coffee)
    )

  def hasBuilding(b: Building): Boolean = buildings.hasBuilding(b)
  def hasActiveBuilding(b: Building): Boolean = buildings.hasActiveBuilding(b)

  def addBuilding(b: Building) = buildings.addBuilding(b)

}

