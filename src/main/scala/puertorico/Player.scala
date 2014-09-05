package puertorico
import akka.actor._
import akka.event.LoggingReceive

class Player extends Actor {
  def receive = LoggingReceive {
    case ChooseRole => {
      //trigger some role selection
      println("I need to pick a role")
      //val role = Prospector
      //sender ! RoleChosen(role)
    }
    case SelectGoodToProduce => {
      //trigger some good selection
      println("I need to pick a good")
      //val good = Corn
      //sender ! GoodSelected(Corn)
    }
  }
}

