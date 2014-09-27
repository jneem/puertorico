package org.puertorico

import akka.testkit.{ TestProbe, TestKit }
import akka.actor._
import org.scalatest.{ FlatSpecLike, BeforeAndAfterAll }

class PassthroughActorTest(_system: ActorSystem) extends TestKit(_system) with FlatSpecLike with BeforeAndAfterAll {

  behavior of "PassthroughActor"

  def this() = this(ActorSystem("PassthroughActorTest"))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  def setup() = {
    val pt = system.actorOf(Props[PassthroughActor])
    val alice = TestProbe()
    val bob = TestProbe()

    pt ! PassthroughActor.SetAlice(alice.ref)
    pt ! PassthroughActor.SetBob(bob.ref)

    (alice, pt, bob)
  }

  it should "correctly pass through messages" in {
    val (alice, eve, bob) = setup()

    eve.tell("Hello from bob", bob.ref)
    alice.expectMsg("Hello from bob")
    eve.tell("Hello from alice", alice.ref)
    bob.expectMsg("Hello from alice")
  }

  it should "refuse to pass through messages from unknown senders" in {
    val (alice, eve, bob) = setup()

    eve ! "Hello from the system"
    eve.tell("Hello from bob", bob.ref)
    alice.expectMsg("Hello from bob")
  }
}

