package puertorico

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class GameStateTest extends FlatSpec {
  behavior of "GameState"

  val gameState = new GameState
  val p1 = gameState.playerOneState
  val p2 = gameState.playerTwoState

  it should "give roles correctly" in {
    val gameState2 = new GameState
    gameState2.rolesDoubloons(Craftsman) = 2
    gameState2.givePickerRole(Craftsman)
    assert(gameState2.isRoleAvailable(Craftsman) === false)
    assert(gameState2.playerOneState.doubloons === 2)
  }

  it should "craft correctly" in {
    p1.addBuilding(BigSugar)
    p1.addBuilding(SmallSugar)
    p1.addPlantation(SugarPlantation)
    p1.addPlantation(SugarPlantation)
    p1.addPlantation(SugarPlantation)
    p1.island.colonistsPlantation(SugarPlantation) += 3
    p1.addColonistOnBuilding(BigSugar)

    p2.addBuilding(SmallIndigo)
    p2.addPlantation(IndigoPlantation)

    gameState.craft

    assert(p1.goods(Sugar) == 1)
    assert(p2.goods(Indigo) == 0)

    p1.addColonistOnBuilding(BigSugar)
    gameState.craft

    assert(p1.goods(Sugar) == 3)

  }

  it should "build correctly" in {
    p1.doubloons = 2
    p2.doubloons = 3

    assert(gameState.canBuild(Hacienda, p1) === true)
    assert(gameState.canBuild(Hacienda, p2) === true)

    gameState.giveBuilding(Hacienda, p1)
    assert(p1.hasBuilding(Hacienda) === true)
    //got one for builder discount
    assert(p1.doubloons === 1)
    assert(gameState.canBuild(Hacienda, p1) === false)
    assert(gameState.canBuild(Hacienda, p2) === false)
  }

  it should "trade correctly" in {
    assert(gameState.canTradeSomeGoods(p2) === false)
    assert(p1.goods(Sugar) == 3)
    assert(gameState.canTradeSomeGoods(p1) === true)
    assert(gameState.canTradeGood(Sugar, p1) === true)

    gameState.doTrade(Sugar, p1)
    assert(gameState.tradingHouse.contains(Sugar) === true)
    assert(gameState.canTradeSomeGoods(p1) === false)
    assert(p1.doubloons == 4)
  }

  it should "handles construction huts correctly" in {
    assert(gameState.canGetPlantation(Quarry, p1) === true)
    assert(gameState.canGetPlantation(Quarry, p2) === false)
    p2.addBuilding(ConstructionHut)
    p2.addColonistOnBuilding(ConstructionHut)
    assert(gameState.canGetPlantation(Quarry, p2) === true)
  }

  it should "switch role pickers correctly" in {
    //correct ordering
    assert(gameState.rolePicker === p1)
    gameState.nextPlayerPickRoles
    assert(gameState.rolePicker === p2)
    assert(gameState.orderPlayers.head === p2)
    assert(gameState.orderPlayers.tail.head === p1)

    gameState.resetRoles
    //temporary buildings are cleared
    assert(p1.recentlyAddedBuilding === EmptyBuilding)
    assert(p1.recentlyAddedPlantations.isEmpty === true)
  }

}
