package puertorico

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class BuildingStateTest extends FlatSpec {
  behavior of "BuildingState"

  it should "add up correctly" in {
    val bs = new BuildingState
    assert(bs.spaceUsed === 0)
    assert(bs.spaceRemaining === 12)
    assert(bs.colonistsMax === 0)
    assert(bs.colonistsUsed === 0)
    assert(bs.colonistsNeeded === 0)

    bs.productionBuildings.buildingMap += ((BigSugar, 2))
    bs.productionBuildings.buildingMap += ((SmallIndigo, 1))
    bs.productionBuildings.buildingMap += ((SmallSugar, 1))

    assert(bs.spaceUsed === 3)
    assert(bs.spaceRemaining === 9)
    assert(bs.colonistsMax === 5)
    assert(bs.colonistsUsed === 4)
    assert(bs.colonistsNeeded === 1)

    bs.purpleBuildings.buildingMap += ((SmallMarket, 0))
    assert(bs.spaceUsed === 4)
    assert(bs.colonistsMax === 6)
    assert(bs.colonistsUsed === 4)

    assert(bs.productionBundle === GoodBundle(0, 1, 3, 0, 0))
  }

  it should "add buildings" in {
    val bs = new BuildingState
    bs.addBuilding(BigSugar)

    assert(bs.hasBuilding(BigSugar) === true)

    bs.addBuilding(Residence)
    assert(bs.hasBuilding(Residence) === true)
    assert(bs.hasBuilding(SmallSugar) === false)
  }

  it should "add colonists" in {
    val bs = new BuildingState
    bs.addBuilding(BigSugar)
    bs.addColonistOnBuilding(BigSugar)

    assert(bs.productionBuildings.buildingMap(BigSugar) === 1)
  }
}

class PlayerStateTest extends FlatSpec {
  behavior of "PlayerState"

  it should "add buildings" in {
    val playerState = new PlayerState
    assert(playerState.hasBuilding(BigSugar) === false)
    playerState.addBuilding(BigSugar)
    playerState.addBuilding(SmallSugar)

    assert(playerState.buildings.hasBuilding(BigSugar) === true)
    assert(playerState.buildings.hasBuilding(SmallSugar) === true)
  }

  it should "add colonists" in {
    val playerState = new PlayerState
    playerState.addBuilding(BigSugar)
    playerState.addBuilding(Residence)
    playerState.addColonistOnBuilding(BigSugar)
    playerState.addColonistOnBuilding(Residence)

    assert(playerState.buildings.productionBuildings.buildingMap(BigSugar) === 1)
    assert(playerState.buildings.purpleBuildings.buildingMap(Residence) === 1)

  }

}
