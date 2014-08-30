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

    bs.productionBuildings += ((BigSugar, 2))
    bs.productionBuildings += ((SmallIndigo, 1))
    bs.productionBuildings += ((SmallSugar, 1))
    
    assert(bs.spaceUsed === 3)
    assert(bs.spaceRemaining === 9)
    assert(bs.colonistsMax === 5)
    assert(bs.colonistsUsed === 4)
    assert(bs.colonistsNeeded === 1)

    bs.purpleBuildings += ((SmallMarket, 0))
    assert(bs.spaceUsed === 4)
    assert(bs.colonistsMax === 6)
    assert(bs.colonistsUsed === 4)

    assert(bs.productionBundle === GoodBundle(0,1,3,0,0))
  }
}
