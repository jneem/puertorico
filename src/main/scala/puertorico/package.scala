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
  }

  object GoodBundle {
    def empty = GoodBundle(0,0,0,0,0)
  }


  // Data structures related to buildings.

  trait Building {
    def cost: Int
    def victoryPoints: Int
    def maxQuarryDiscount = victoryPoints
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
    val colonistsMax = 1
    val description = "Produces indigo"
  }

  case object SmallSugar extends ProductionBuilding {
    val cost = 2
    val victoryPoints = 1
    val good = Sugar
    val colonistsMax = 1
    val description = "Produces sugar"
  }

  case object BigSugar extends ProductionBuilding {
    val cost = 4
    val victoryPoints = 2
    val good = Sugar
    val colonistsMax = 3
    val description = "Produces lots of sugar"
  }

  case object SmallMarket extends PurpleBuilding {
    val cost = 1
    val victoryPoints = 1
    val description = "Gives one extra doubloon per trade"
  }

  //TODO: more building cards

  // Data structures related to ships.

  class Ship(val size: Int) {
    var good: Option[Good] = None
    var spaceUsed: Int = 0
    def spaceRemaining: Int = size - spaceUsed
  }


}

