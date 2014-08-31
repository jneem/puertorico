
package puertorico
import scala.collection.mutable.HashMap


class GameState {

  val playerOne = new PlayerState
  val playerTwo = new PlayerState
  var _currentPlayer = playerOne
  var _otherPlayer = playerTwo
  var governor = playerOne
  var rolePicker = playerOne

  val victoryPointsMax = 75
  def victoryPointsUsed = playerOne.victoryPoints + playerTwo.victoryPoints
  def victoryPointsLeft = (victoryPointsMax - victoryPointsUsed) max 0

  val colonistsMax = 75
  def colonistsUsed = playerOne.colonistsUsed + playerTwo.colonistsUsed
  def colonistsLeft = (colonistsMax - colonistsUsed) max 0 

  val ship4 = new Ship(4)
  val ship5 = new Ship(5)
  val ship6 = new Ship(6)
  val wharf = new Ship(100)

  val tradingHouse = new TradingHouse
  val plantationsVisible = new PlantationBundle(1,1,1,1,1,1)
  val plantationsDiscarded = PlantationBundle.empty
  val plantationsMax = new PlantationBundle(5,24,24,24,24,24)


  val buildingsAll: Map[Building, Int] = Map(
    (SmallIndigo, 2), (SmallSugar, 2), (SmallMarket, 1), (Hacienda, 1), (ConstructionHut, 1), (SmallWarehouse, 1),
    (BigIndigo, 2), (BigSugar, 2), (BigMarket, 1), (BigWarehouse, 1), (Hospice, 1), (Office, 1),
    (BigCoffee, 2), (BigTobacco, 2), (Factory, 1), (University, 1), (Wharf, 1), (Harbor, 1), 
    (TownHall, 1), (Residence, 1), (Fortress, 1), (GuildHall, 1), (CustomHouse, 1) )

  val buildingsRemaining = new HashMap[Building, Int]




  def isEndGame: Boolean = {
    colonistsLeft == 0 || 
    victoryPointsLeft == 0 || 
    playerOne.buildings.spaceRemaining == 0 ||
    playerTwo.buildings.spaceRemaining == 0
  }
}

object GameState {
  

}


