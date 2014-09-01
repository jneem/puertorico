package puertorico
import akka.actor._


//RoleManager and Player conversation

case object ChooseRole 
case class RoleChosen(role: Role)

//RoleManager and RoleActors conversation

case class BeginAction(player: ActorRef)
case object EndAction

//Craftsman and Player conversation
case object SelectGoodToProduce 
case class GoodSelected(good: Good)

//Settler and Player conversation
case object SelectPlantation //allow empty
case class PlantationSelected(plant: Plantation)
case object NoneSelected //common to many

//Mayor and Player conversation
case object SelectColonist //common to Settler
case object ColonistSelected
case object RearrangeColonists
case class ColonistsRearranged(colonistsPlantation: PlantationBundle, 
  productionBuildings: List[(ProductionBuilding, Int)], purpleBuildings: List[(PurpleBuilding, Int)], colonistsSpare: Int) 

//Trader and Player conversation
case object SelectGoodToTrade

//Captain and Player conversation
case object SelectGoodToShip
case class GoodAndShipSelected(good: Good, ship: Ship)
case object SelectGoodToKeep
case class GoodToKeepSelected(goodList: List[(Good, Int)])





