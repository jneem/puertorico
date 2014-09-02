package puertorico

import akka.testkit.{TestFSMRef, TestKit, TestProbe}
import akka.actor.{Actor, ActorSystem, Props}
import org.scalatest.WordSpecLike
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll

//test behavior of one RoleBoss first

class RoleBossTest(_system: ActorSystem) extends TestKit(_system) with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("SimplePuertoRico"))

  val playerOneMane = system.actorOf(Props(new PlayerMane), name = "playerOne")
  val playerTwoMane = system.actorOf(Props(new PlayerMane), name = "playerTwo")
  val roleBoss = system.actorOf(Props(new RoleBoss(playerOneMane, playerTwoMane)), name = "roleBoss")

  "role boss" must {

    "respond correctly" in {
      val probe = TestProbe()
      playerOneMane.tell(ChooseRole, roleBoss)
      roleBoss.tell(Prospector, playerOneMane)
      roleBoss.tell(GameStateQuery, probe.ref)
      probe.expectMsg(1)
      //assert(gameState.rolesDoubloons(Prospector) == -1)
      //assert(gameState.playerOne.doubloons === 1)
      //assert(gameState.currentPlayer === gameState.playerTwo)
    }

  }
}
