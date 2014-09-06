package puertorico

import akka.actor.{ Actor, ActorRef }
import org.slf4j.{ Logger, LoggerFactory }

object PassthroughActor {
  case class SetAlice(a: ActorRef)
  case class SetBob(b: ActorRef)
}

/**
 * Forwards any message from alice along to bob, and vice versa.
 */
class PassthroughActor extends Actor {
  import PassthroughActor._

  lazy val logger = LoggerFactory.getLogger("PassthroughActor")

  var maybeAlice: Option[ActorRef] = None
  var maybeBob: Option[ActorRef] = None

  def receive = {
    case SetAlice(a) => maybeAlice = Some(a)
    case SetBob(b) => maybeBob = Some(b)

    case x => {
      (maybeAlice, maybeBob) match {
        case (Some(alice), Some(bob)) => {
          if (sender == alice) {
            bob ! x
          } else if (sender == bob) {
            alice ! x
          } else {
            logger.warn("got a message from a sender I don't recognize")
          }
        }

        case _ => {
          logger.warn("tried to pass through a message before connecting the endpoints")
        }
      }
    }
  }
}

