package object puertorico {

  // Data structures related to goods.

  trait Good
  case object Corn extends Good
  case object Indigo extends Good
  case object Sugar extends Good
  case object Tobacco extends Good
  case object Coffee extends Good

  case class GoodBundle(
    var corn: Int,
    var indigo: Int,
    var sugar: Int,
    var tobacco: Int,
    var coffee: Int) {
    
    def apply(good: Good): Int = good match {
      case Corn => corn
      case Indigo => indigo
      case Sugar => sugar
      case Tobacco => tobacco
      case Coffee => coffee
    }

    def update(good: Good, count: Int) = good match {
      case Corn => corn = count
      case Indigo => indigo = count
      case Sugar => sugar = count
      case Tobacco => tobacco = count
      case Coffee => coffee = count
    }

    def sum: Int = corn + indigo + sugar + tobacco + coffee

    def isEmpty: Boolean = sum == 0
  }

  object GoodBundle {
    def empty = GoodBundle(0,0,0,0,0)
  }

  // Data structures related to trading house

  class TradingHouse {
    val goods = GoodBundle.empty
    val size = 4
    def isFull: Boolean = goods.sum == size
    def contains(good: Good): Boolean = goods(good) > 0
  }

  // Data structures related to plantations

  trait Plantation
  case object CornPlantation extends Plantation
  case object IndigoPlantation extends Plantation
  case object SugarPlantation extends Plantation
  case object TobaccoPlantation extends Plantation
  case object CoffeePlantation extends Plantation
  case object Quarry extends Plantation


  case class PlantationBundle(
    var quarry: Int, 
    var corn: Int,
    var indigo: Int,
    var sugar: Int,
    var tobacco: Int,
    var coffee: Int) {

    def apply(plant: Plantation): Int = plant match {
      case CornPlantation => corn
      case IndigoPlantation => indigo
      case SugarPlantation => sugar
      case TobaccoPlantation => tobacco
      case CoffeePlantation => coffee
      case Quarry => quarry
    }

    def update(plant: Plantation, count: Int) = plant match {
      case CornPlantation => corn = count
      case IndigoPlantation => indigo = count
      case SugarPlantation => sugar = count
      case TobaccoPlantation => tobacco = count
      case CoffeePlantation => coffee = count
      case Quarry => quarry = count
    }

    def sum: Int = corn + indigo + sugar + tobacco + coffee + quarry
  }

  object PlantationBundle {
    def empty = PlantationBundle(0,0,0,0,0,0)
  }

  // Data structures related to buildings.

  trait Building {
    def cost: Int
    def victoryPoints: Int
    def maxQuarryDiscount = victoryPoints
    def colonistsMax = 1
    def size: Int = 1
    def description: String
  }

  trait ProductionBuilding extends Building {
    def colonistsMax: Int
    def good: Good
  }

  trait PurpleBuilding extends Building 

  case object SmallIndigo extends ProductionBuilding {
    val cost = 1
    val victoryPoints = 1
    val good = Indigo
    val description = "Produces indigo"
  }

  case object SmallSugar extends ProductionBuilding {
    val cost = 2
    val victoryPoints = 1
    val good = Sugar
    val description = "Produces sugar"
  }

  case object BigSugar extends ProductionBuilding {
    val cost = 4
    val victoryPoints = 2
    val good = Sugar
    override val colonistsMax = 3
    val description = "Produces lots of sugar"
  }

  case object SmallMarket extends PurpleBuilding {
    val cost = 1
    val victoryPoints = 1
    val description = "Gives one extra doubloon per trade"
  }

  case object BigMarket extends PurpleBuilding {
    val cost = 1
    val victoryPoints = 1
    val description = "Gives two extra doubloons per trade"
  }

  case object Factory extends PurpleBuilding {
    val cost = 8
    val victoryPoints = 3
    val description = "Gives 0/1/2/3/5 extra doubloons for different good production"
  }

  //TODO: more building cards
  class UnfinishedBuilding extends Building { 
    val cost = 1
    val victoryPoints = 1
    val description = "Unfinished Poohead"
  }
  case object BigIndigo extends UnfinishedBuilding
  case object Hacienda extends UnfinishedBuilding
  case object SmallWarehouse extends UnfinishedBuilding
  case object BigWarehouse extends UnfinishedBuilding
  case object ConstructionHut extends UnfinishedBuilding
  case object Office extends UnfinishedBuilding
  case object BigCoffee extends UnfinishedBuilding
  case object BigTobacco extends UnfinishedBuilding
  case object Hospice extends UnfinishedBuilding
  case object University extends UnfinishedBuilding
  case object Wharf extends UnfinishedBuilding
  case object Harbor extends UnfinishedBuilding
  case object TownHall extends UnfinishedBuilding
  case object CustomHouse extends UnfinishedBuilding
  case object Residence extends UnfinishedBuilding
  case object GuildHall extends UnfinishedBuilding
  case object Fortress extends UnfinishedBuilding
  // Data structures related to ships.

  class Ship(val size: Int) {
    var good: Option[Good] = None
    var spaceUsed: Int = 0
    def spaceRemaining: Int = size - spaceUsed
  }

  trait Role
  case object Prospector extends Role
  case object Mayor extends Role
  case object Captain extends Role
  case object Craftsman extends Role
  case object Settler extends Role
  case object Builder extends Role
  case object Trader extends Role



}

