package puertorico
import akka.actor._
import scala.collection.immutable.HashMap

//RoleBoss and Player conversation

case object ChooseRole
case class GotRole(role: Role)

case class GotDoubloons(doubloons: Int)
case class GotColonists(col: Int)
case class GotBuilding(b: Building)
case class GotPlantation(pl: Plantation)
case class GotTrade(good: Good)
case object GotColonistByHospice
case object GotColonistByUniversity
case class GotGood(good: Good)
case class GotGoods(good: GoodBundle)
case class GotGoodShipped(good: Good, num: Int)
case class GotVictoryPoints(num: Int)
case object GotShipToClear

//Craftsman and Player conversation
case object SelectGoodToProduce
case class GoodSelected(good: Good)

//Settler and Player conversation
case object SelectPlantation //allow empty
case object SelectPlantationExtra
case class PlantationSelected(plant: Plantation)
case object PlantationExtraAgreed
case object NoneSelected //common to many

//Mayor and Player conversation
case object SelectColonist //common to Settler
case object ColonistSelected
case object RearrangeColonists
case class ColonistsRearranged(
  colonistsPlantation: PlantationBundle,
  productionBuildings: List[(ProductionBuilding, Int)], purpleBuildings: List[(PurpleBuilding, Int)], colonistsSpare: Int
)
case class GotColonistsRearranged(
  colonistsPlantation: PlantationBundle,
  productionBuildings: List[(ProductionBuilding, Int)], purpleBuildings: List[(PurpleBuilding, Int)], colonistsSpare: Int
)

//Trader and Player conversation
case object SelectGoodToTrade

//Builer and Player conversation
case object SelectBuilding
case class BuildingSelected(b: Building)

//Captain and Player conversation
case object SelectGoodToShip
case class GoodAndShipSelected(good: Good, ship: Ship)
case object SelectGoodToKeep
case class GoodToKeepSelected(goodList: HashMap[Good, Int])
case class GotGoodToKeep(goodList: HashMap[Good, Int])

