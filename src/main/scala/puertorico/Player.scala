package puertorico
import akka.actor._
import akka.event.LoggingReceive

class Player extends Actor {
  def receive = LoggingReceive {
    case ChooseRole => {
      //trigger some role selection
      val role = Prospector
      sender ! role
    }
    case SelectGoodToProduce => {
      //trigger some good selection
      //val good = Corn
      //sender ! GoodSelected(Corn)
    }
  }
}

