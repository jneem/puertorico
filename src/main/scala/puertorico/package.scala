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
    def goodsOnly: GoodBundle = GoodBundle(corn, indigo, sugar, tobacco, coffee)
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

  case object EmptyBuilding extends Building {
    val cost = 0
    val victoryPoints = 0
    override val colonistsMax = 0
    override val size = 0
    val description = "Empty Building"
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

  case object BigIndigo extends ProductionBuilding {
    val cost = 3
    val victoryPoints = 2
    val good = Indigo
    override val colonistsMax = 3
    val description = "Produces lots of indigo"
  }

  case object Hacienda extends PurpleBuilding {
    val cost = 2
    val victoryPoints = 1
    val description = "Gives one random plantation in settler round"
  }

  case object SmallWarehouse extends PurpleBuilding {
    val cost = 3
    val victoryPoints = 1
    val description = "Stores one type of good at the end of captain round"
  }

  case object BigWarehouse extends PurpleBuilding {
    val cost = 5
    val victoryPoints = 2
    val description = "Stores two types of good at the end of captain round"
  }

  case object ConstructionHut extends PurpleBuilding {
    val cost = 2
    val victoryPoints = 1
    val description = "Can choose quarry over plantation in settler round"
  }

  case object Office extends PurpleBuilding {
    val cost = 5
    val victoryPoints = 2
    val description = "Can trade in any type of good"
  }

  case object BigCoffee extends ProductionBuilding {
    val cost = 6
    val victoryPoints = 3
    val good = Coffee
    override val colonistsMax = 2
    val description = "Produces coffee"
  }
  case object BigTobacco extends ProductionBuilding {
    val cost = 5
    val victoryPoints = 3
    val good = Tobacco
    override val colonistsMax = 3
    val description = "Produces tobacco"
  }
  case object Hospice extends PurpleBuilding {
    val cost = 4
    val victoryPoints = 2
    val description = "One extra colonist in settler round"
  }

  case object University extends PurpleBuilding {
    val cost = 7
    val victoryPoints = 3
    val description = "One extra colonist in builder round"
  }
  case object Wharf extends PurpleBuilding {
    val cost = 9
    val victoryPoints = 3
    val description = "Private ship"
  }

  case object Harbor extends PurpleBuilding{
    val cost = 8
    val victoryPoints = 3
    val description = "Gives one extra victory points per load"
  }

  case object TownHall extends PurpleBuilding {
    val cost = 10
    val victoryPoints = 4
    val description = "4 points per purple building"
  }

  case object CustomsHouse extends PurpleBuilding {
    val cost = 10
    val victoryPoints = 4
    val description = "4 points per victory points"
  }
  case object Residence extends PurpleBuilding {
    val cost = 10
    val victoryPoints = 4
    val description = "4/5/6/7 points for occupied island space <9/10/11/12"
  }

  case object GuildHall extends PurpleBuilding {
    val cost = 10
    val victoryPoints = 4
    val description = "1 point per small production, 2 points per big production building"
  }
  case object Fortress extends PurpleBuilding {
    val cost = 10
    val victoryPoints = 4
    val description = "1 point per 3 colonists"
  }
  // Data structures related to ships.

  class Ship(val size: Int) {
    private var _good: Option[Good] = None
    def good = _good
    private var _spaceUsed: Int = 0
    def spaceUsed = _spaceUsed

    def spaceRemaining: Int = size - spaceUsed

    def canLoad(loadGood: Good) = good.isEmpty || good == loadGood
    /**
     * The maximum amount of the given good that can be loaded.
     */
    def maxLoadable(loadGood: Good): Int = good match {
      case Some(g) => if (g == loadGood) spaceRemaining else 0
      case None => spaceRemaining
    }

    def clear: Unit = {
      _good = None
      _spaceUsed = 0
    }

    def load(loadGood: Good, quantity: Int): Unit = {
      // TODO: if it is an invalid load, log an error
      _good = Some(loadGood)
      _spaceUsed += quantity
    }
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

