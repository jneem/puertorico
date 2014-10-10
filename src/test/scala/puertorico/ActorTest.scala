package org.puertorico

import akka.testkit.{ TestFSMRef, TestKit, TestProbe }
import akka.actor._
import org.scalatest.WordSpecLike
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FlatSpec
import scala.concurrent.duration._
import scala.collection.immutable.{ HashMap => ImHashMap }

class RoleBossTest(_system: ActorSystem) extends TestKit(_system) with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("SimplePuertoRico"))

  //val playerOne = system.actorOf(Props(new Player), name = "playerOne")
  //val playerTwo = system.actorOf(Props(new Player), name = "playerTwo")
  //val roleBoss = system.actorOf(Props(new RoleBoss(playerOne, playerTwo)), name = "roleBoss")
  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }
  //test for one role boss

  def startGame = {
    val probe1 = TestProbe()
    val probe2 = TestProbe()
    val roleBoss = TestFSMRef(new RoleBoss(probe1.ref, probe2.ref))

    //tell the players to ignore all messages received
    probe1.ignoreMsg({ case _ => true })
    probe2.ignoreMsg({ case _ => true })

    val probe3 = TestProbe() //used to query game state
    roleBoss.receive(GameStateQuery, probe3.ref)
    val gameState = probe3.receiveOne(1.second) match { case (x: GameState) => x }

    roleBoss ! StartGame

    probe1.ignoreNoMsg()
    probe2.ignoreNoMsg()
    (probe1, probe2, roleBoss, gameState)

  }

  "a single role boss" must {
    "start correctly" in {

      val probe1 = TestProbe()
      val probe2 = TestProbe()
      val roleBoss = TestFSMRef(new RoleBoss(probe1.ref, probe2.ref))
      val playerOne = probe1.ref
      val playerTwo = probe2.ref

      assert(roleBoss.stateName === WaitForStart)

      roleBoss ! StartGame
      assert(roleBoss.stateName === RoleProcess)
      assert(roleBoss.stateData === DoOnce(playerOne))

      val msg = (0, ChooseRole)

      probe1.expectMsg(msg)
      probe2.expectMsg(msg)

    }

    "prospect correctly" in {
      val (probe1, probe2, roleBoss, gameState) = startGame

      roleBoss.receive(Prospector, probe1.ref)

      val msg1 = (0, GotRole(Prospector))
      val msg2 = (0, GotDoubloons(1))

      probe1.expectMsg(msg1)
      probe1.expectMsg(msg2)

      assert(roleBoss.stateName === RoleProcess)
      assert(roleBoss.stateData === DoOnce(probe2.ref))

      //role prospector should be unavailable to the second player
      roleBoss.receive(Prospector, probe2.ref)
      //role boss stays at current state
      assert(roleBoss.stateName === RoleProcess)
      assert(roleBoss.stateData === DoOnce(probe2.ref))

    }

    "settle without hacienda correctly" in {
      val (probe1, probe2, roleBoss, gameState) = startGame

      //initialize game data
      roleBoss.receive(Settler, probe1.ref)

      //tell p2 to ignore all messages
      probe2.ignoreMsg({ case _ => true })

      val msg1 = (0, GotRole(Settler))
      val msg2 = (0, GotDoubloons(0))
      val msg3 = (0, SelectPlantation)

      probe1.expectMsg(msg1)
      probe1.expectMsg(msg2)
      probe1.expectMsg(msg3)

      //go straight to asking for a plantation
      assert(roleBoss.stateName === SettlerProcess)

      //p1 picks Quarry
      roleBoss.receive(PlantationSelected(Quarry), probe1.ref)

      probe1.expectMsg((0, GotPlantation(Quarry)))
      assert(roleBoss.stateName === SettlerProcess)
      assert(gameState.plantationsVisible(Quarry) === 4)
      probe1.expectMsg((1, SelectPlantation))

      //p2 tries to pick an illegal plantation
      roleBoss.receive(PlantationSelected(Quarry), probe2.ref)
      probe1.expectNoMsg(1.millisecond)
      assert(gameState.playerTwoState.island.plantations(Quarry) == 0)
      assert(roleBoss.stateName === SettlerProcess)

      //now player2 pick an allowed plantation
      roleBoss.receive(PlantationSelected(CornPlantation), probe2.ref)
      assert(gameState.plantationsVisible(CornPlantation) === 0)
      assert(gameState.playerTwoState.island.plantations(CornPlantation) == 1)
      assert(roleBoss.stateName === RoleProcess)
      assert(roleBoss.stateData === DoOnce(probe2.ref))

    }

    "build correctly" in {
      val (probe1, probe2, roleBoss, gameState) = startGame
      gameState.playerOneState.doubloons = 5
      gameState.playerTwoState.doubloons = 5

      roleBoss.receive(Builder, probe1.ref)

      val msg1 = (0, GotRole(Builder))
      val msg2 = (0, GotDoubloons(0))
      val msg3 = (0, SelectBuilding)

      probe1.expectMsg(msg1)
      probe1.expectMsg(msg2)
      probe1.expectMsg(msg3)

      roleBoss.receive(BuildingSelected(Hacienda), probe1.ref)
      //p1 receives the correct messages
      probe1.expectMsg((0, GotBuilding(Hacienda)))
      probe1.expectMsg((0, GotDoubloons(-1)))
      probe1.expectMsg((1, SelectBuilding))

      //game data is correct
      assert(gameState.playerOneState.doubloons === 4)
      assert(gameState.playerOneState.hasBuilding(Hacienda) === true)
      assert(gameState.buildingsRemaining(Hacienda) === 0)

      //p2 tries to pick illegal building
      roleBoss.receive(BuildingSelected(University), probe2.ref)
      probe1.expectNoMsg(1.millisecond)
      assert(gameState.playerTwoState.hasBuilding(University) === false)

      //p2 picks a valid building
      roleBoss.receive(BuildingSelected(Hospice), probe2.ref)
      assert(gameState.playerTwoState.doubloons === 1)
      assert(gameState.buildingsRemaining(Hospice) === 0)
      assert(gameState.playerTwoState.hasBuilding(Hospice) === true)

    }

    "trade correctly" in {
      val (probe1, probe2, roleBoss, gameState) = startGame
      gameState.playerOneState.goods = GoodBundle(1, 0, 0, 0, 1)
      gameState.playerTwoState.goods = GoodBundle(0, 2, 0, 0, 2)

      roleBoss.receive(Trader, probe1.ref)

      val msg1 = (0, GotRole(Trader))
      val msg2 = (0, GotDoubloons(0))
      val msg3 = (0, SelectGoodToTrade)

      probe1.expectMsg(msg1)
      probe1.expectMsg(msg2)
      probe1.expectMsg(msg3)

      //p1 tries to select a good that they don't have
      roleBoss.receive(GoodSelected(Indigo), probe1.ref)
      probe1.expectNoMsg(1.millisecond)
      assert(gameState.tradingHouse.contains(Indigo) === false)

      //p1 trades in a coffee
      roleBoss.receive(GoodSelected(Coffee), probe1.ref)
      probe1.expectMsg((0, GotTrade(Coffee)))
      probe1.expectMsg((0, GotDoubloons(5)))
      probe1.expectMsg((1, SelectGoodToTrade))

      //p2 tries to select coffee again
      roleBoss.receive(GoodSelected(Coffee), probe2.ref)
      probe1.expectNoMsg(1.millisecond)

      //p2 select nothing to trade
      roleBoss.receive(NoneSelected, probe2.ref)
      assert(gameState.playerTwoState.goods === GoodBundle(0, 2, 0, 0, 2))
      assert(roleBoss.stateName === RoleProcess)
      assert(roleBoss.stateData === DoOnce(probe2.ref))
    }

    "doMayor and craft correctly" in {
      //initialize
      val (probe1, probe2, roleBoss, gameState) = startGame
      gameState.playerOneState.doubloons = 10
      gameState.playerTwoState.doubloons = 10
      gameState.playerOneState.colonistsSpare = 1
      gameState.playerTwoState.colonistsSpare = 2
      gameState.givePlantationToPlayer(CornPlantation, gameState.playerOneState)
      gameState.givePlantationToPlayer(Quarry, gameState.playerOneState)
      gameState.givePlantationToPlayer(IndigoPlantation, gameState.playerTwoState)
      gameState.giveBuilding(BigIndigo, gameState.playerTwoState)

      roleBoss.receive(Mayor, probe1.ref)
      probe1.expectMsg((0, GotRole(Mayor)))
      probe1.expectMsg((0, GotDoubloons(0)))
      probe1.expectMsg((0, SelectColonist))

      //have not handed out the colonists until p1 decides if they got extra or not
      assert(gameState.playerOneState.colonistsSpare === 1)
      assert(gameState.playerTwoState.colonistsSpare === 2)

      //p2 tries to select a colonist
      roleBoss.receive(ColonistSelected, probe2.ref)
      probe1.expectNoMsg(1.millisecond)

      //p1 accepts
      roleBoss.receive(ColonistSelected, probe1.ref)
      probe1.expectMsg((0, GotColonists(1)))
      //there are three empty slots on buildings
      probe1.expectMsg((0, GotColonists(2)))
      probe1.expectMsg((1, GotColonists(1)))
      probe1.expectMsg((0, RearrangeColonists))
      assert(gameState.playerOneState.colonistsSpare === 4)
      assert(gameState.playerTwoState.colonistsSpare === 3)

      //hypothetical arrangement
      val cp1 = PlantationBundle(1, 1, 0, 0, 0, 0)
      val cp2 = PlantationBundle(0, 0, 1, 0, 0, 0)
      val proB = List((BigIndigo, 2))
      val purB = List.empty

      //p2 finishes assigning first
      val msg = ColonistsRearranged(cp2, proB, purB, 0)
      roleBoss.receive(msg, probe2.ref)
      assert(gameState.isValidColonistsArrangement(gameState.playerTwoState, cp2, proB, purB, 0) === true)
      probe1.expectMsg((1, GotColonistsRearranged(cp2, proB, purB, 0)))

      //p1 tries an invalid rearrangement
      roleBoss.receive(msg, probe1.ref)
      probe1.expectMsg((0, RearrangeColonists))

      roleBoss.receive(ColonistsRearranged(cp1, List.empty, List.empty, 2), probe1.ref)
      probe1.expectMsg((0, GotColonistsRearranged(cp1, List.empty, List.empty, 2)))

      //check gameState directly
      assert(gameState.playerOneState.numberActiveQuarry === 1)
      assert(gameState.playerOneState.productionBundle(Corn) === 1)
      assert(gameState.playerTwoState.productionBundle(Indigo) === 1)

      //player 2 initiates crafting
      roleBoss.receive(Craftsman, probe2.ref)
      assert(gameState.playerOneState.goods === GoodBundle(1, 0, 0, 0, 0))
      assert(gameState.playerTwoState.goods === GoodBundle(0, 1, 0, 0, 0))
      assert(gameState.goodsRemain(Corn) === 6)
      assert(gameState.goodsRemain(Indigo) === 6)

      //player2 tries to select good that he doesn't have
      roleBoss.receive(GoodSelected(Corn), probe2.ref)
      assert(gameState.playerTwoState.goods(Corn) === 0)

      //player2 selects a valid good
      roleBoss.receive(GoodSelected(Indigo), probe2.ref)
      assert(gameState.playerTwoState.goods(Indigo) === 2)
      assert(gameState.goodsRemain(Indigo) === 5)

    }

    "do captain correctly" in {
      val (probe1, probe2, roleBoss, gameState) = startGame
      gameState.playerOneState.goods = GoodBundle(1, 2, 2, 1, 2)
      gameState.playerTwoState.goods = GoodBundle(2, 1, 0, 0, 3)
      val ship4 = gameState.ships.head
      val ship6 = gameState.ships.tail.head

      roleBoss.receive(Captain, probe1.ref)
      probe1.expectMsg((0, GotRole(Captain)))
      probe1.expectMsg((0, GotDoubloons(0)))
      probe1.expectMsg((0, SelectGoodToShip))

      val shipment = GoodAndShipSelected(Corn, ship6)
      roleBoss.receive(shipment, probe1.ref)
      //ship status: 4: empty, 6: 1 corn
      probe1.expectMsg((0, GotGoodShipped(Corn, 1)))
      probe1.expectMsg((0, GotVictoryPoints(2)))
      probe1.expectMsg((1, SelectGoodToShip))

      val shipment2 = GoodAndShipSelected(Coffee, ship4)
      roleBoss.receive(shipment2, probe2.ref)
      //ship status: 4: 3 coffee, 6: 1 corn
      probe1.expectMsg((1, GotGoodShipped(Coffee, 3)))
      probe1.expectMsg((1, GotVictoryPoints(3)))
      probe1.expectMsg((0, SelectGoodToShip))

      //invalid shipment from p1
      val shipment3 = GoodAndShipSelected(Indigo, ship4)
      roleBoss.receive(shipment3, probe1.ref)
      probe1.expectMsg((0, SelectGoodToShip))
      assert(gameState.playerOneState.goods(Indigo) === 2)

      val shipment4 = GoodAndShipSelected(Coffee, ship4)
      roleBoss.receive(shipment4, probe1.ref)
      //ship status: 4: 4 coffee, 6: 1 corn
      probe1.expectMsg((0, GotGoodShipped(Coffee, 1)))
      probe1.expectMsg((0, GotVictoryPoints(1)))
      probe1.expectMsg((1, SelectGoodToShip))

      val shipment5 = GoodAndShipSelected(Corn, ship6)
      roleBoss.receive(shipment5, probe2.ref)
      //ship status: 4: 4 coffee, 6: 3 corn
      probe1.expectMsg((1, GotGoodShipped(Corn, 2)))
      probe1.expectMsg((1, GotVictoryPoints(2)))
      assert(gameState.playerOneState.goods === GoodBundle(0, 2, 2, 1, 1))
      assert(gameState.playerTwoState.goods === GoodBundle(0, 1, 0, 0, 0))
      probe1.expectMsg((0, SelectGoodToKeep))
      probe1.expectMsg((1, SelectGoodToKeep))

      val goodList1 = ImHashMap[Good, Int](Coffee -> 1)
      val goodList2 = ImHashMap[Good, Int](Indigo -> 1)
      //p2 sends in the message first
      roleBoss.receive(GoodToKeepSelected(goodList2), probe2.ref)
      roleBoss.receive(GoodToKeepSelected(goodList1), probe1.ref)
      assert(gameState.playerOneState.goods === GoodBundle(0, 0, 0, 0, 1))
      assert(gameState.playerTwoState.goods === GoodBundle(0, 1, 0, 0, 0))

      //captain role ended
      assert(roleBoss.stateName === RoleProcess)
      assert(roleBoss.stateData === DoOnce(probe2.ref))
      //ship4 got cleared but not ship6
      assert(ship4.spaceRemaining === 4)
      assert(ship4.good.isEmpty === true)
      assert(ship6.spaceRemaining === 3)
      assert(ship6.good.get === Corn)
    }

  }

}

