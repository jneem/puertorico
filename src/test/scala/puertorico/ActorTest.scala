package puertorico

import akka.testkit.{TestFSMRef, TestKit, TestProbe}
import akka.actor.{Actor, ActorSystem, Props}
import org.scalatest.WordSpecLike
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FlatSpec
import scala.concurrent.duration._

class RoleBossTest(_system: ActorSystem) extends TestKit(_system) with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("SimplePuertoRico"))

  //val playerOne = system.actorOf(Props(new Player), name = "playerOne")
  //val playerTwo = system.actorOf(Props(new Player), name = "playerTwo")
  //val roleBoss = system.actorOf(Props(new RoleBoss(playerOne, playerTwo)), name = "roleBoss")

  //test for one role boss

  def startGame = {
      val probe1 = TestProbe()
      val probe2 = TestProbe()
      val roleBoss = TestFSMRef(new RoleBoss(probe1.ref, probe2.ref))

      //tell the players to ignore all messages received
      probe1.ignoreMsg({case _ => true})
      probe2.ignoreMsg({case _ => true})

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
      probe2.ignoreMsg({case _ => true})

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
      probe1.expectNoMsg()
      assert(gameState.playerTwoState.island.plantations(Quarry) == 0)
      assert(roleBoss.stateName === SettlerProcess)
      
      //now player2 pick an allowed plantation
      roleBoss.receive(PlantationSelected(CornPlantation), probe2.ref)
      //TODO: this test fails somehow
      //assert(gameState.plantationsVisible(CornPlantation) === 0)
      //assert(gameState.playerTwoState.island.plantations(CornPlantation) == 1)
      //assert(roleBoss.stateName === ChooseRole)
      //assert(roleBoss.stateData === DoOnce(probe1.ref))
    }


  }

}

