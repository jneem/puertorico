package puertorico_cli

import java.io.{BufferedReader, PrintStream}
import collection.mutable.HashMap
import puertorico._

// There are various things that could happen as the result of
// an action:
//  - some text could be displayed
//  - we could enter a new MenuState
//  - we could send a message to the PlayerActor.
case class ActionResult(val display: String,
                        val newState: Option[MenuState],
                        val message: Option[Object])


// The result is Option[MenuState] because the special
// value None means that the current state should be repeated.
case class MenuChoice(val commandString: String,
                      val description: String,
                      val action: () => ActionResult)

class MenuState(private val initChoices: List[MenuChoice],
                val idle: Boolean = true) {
  private lazy val choices = initChoices :+ helpChoice

  // TODO: warn if initChoices contains a binding for '?',
  // which should be reserved for the help action.

  private lazy val actionMap =
    (choices map (x => x.commandString -> x)).toMap

  // To generate the help text for a given state, we need to
  // know all the other menu choices.
  def helpChoice: MenuChoice = {
    val cmd = "?"
    val desc = "Show the currently available commands."
    lazy val ret = MenuChoice(cmd, desc, helpAction)

    def helpAction() = {
      def oneHelpLine(mc: MenuChoice) = mc.commandString + "\t" + mc.description

      val display = (choices map oneHelpLine).mkString("\n")
      ActionResult(display, None, None)
    }

    ret
  }

  def hasAction(act: String) = actionMap contains act

  def takeAction(act: String) = actionMap(act).action()

  def +(other: MenuState) = {
    // TODO: warn if a binding collides.
    new MenuState(initChoices ++ other.initChoices, idle && other.idle)
  }
}

object PuertoRicoCLIUtils {
  case object Repeat

  val bldgNames = Map[Building, (String, String)](
    SmallIndigo -> ("Small indigo plant", "si"),
    BigIndigo -> ("Big indigo plant", "bi"),
    SmallSugar -> ("Small sugar mill", "ss"),
    BigSugar -> ("Big sugar mill", "bs"),
    SmallMarket -> ("Small market", "sm"),
    BigMarket -> ("Big market", "bm"),
    Factory -> ("Factory", "f"),
    Hacienda -> ("Hacienda", "ha"),
    SmallWarehouse -> ("Small warehouse", "sw"),
    BigWarehouse -> ("Big warehouse", "bw"),
    ConstructionHut -> ("Construction hut", "ch"),
    Office -> ("Office", "o"),
    Hospice -> ("Hospice", "ho"),
    University -> ("University", "u"),
    Harbor -> ("Harbor", "hr"),
    Wharf -> ("Wharf", "w"),
    TownHall -> ("Town Hall", "th"),
    CustomsHouse -> ("Customs House", "cu"),
    Residence -> ("Residence", "r"),
    GuildHall -> ("Guild hall", "gh"),
    Fortress -> ("Fortress", "fo")
  )

  val roleNames = Map[Role, (String, String)](
    Prospector -> ("Prospector", "p"),
    Captain -> ("Captain", "c"),
    Mayor -> ("Mayor", "m"),
    Trader -> ("Trader", "t"),
    Builder -> ("Builder", "b"),
    Settler -> ("Settler", "s"),
    Craftsman -> ("Craftsman", "c")
  )


  def showBuildingState(bs: BuildingState): String = {
    val intro = s"Building space used: ${bs.spaceUsed}/${bs.size}."

    val prodBs = bs.productionBuildings.buildingMap
    val purpBs = bs.purpleBuildings.buildingMap
    val showBuilding = (x: (Building, Int)) => x match {
      case (b, cs) =>
        s"\t${bldgNames(b)._2}\t${bldgNames(b)._1}\t${cs}/${b.colonistsMax}"
    }
    val prod = "Production buildings:\n" + (prodBs map showBuilding mkString "\n")
    val purp = "Other buildings:\n" + (purpBs map showBuilding mkString "\n")

    s"$intro\n\n$prod\n\n$purp\n\n"
  }

  def showIslandState(is: IslandState): String = {
    val intro = s"Island space used: ${is.colonistsUsed}/${is.size}."

    val ps = is.plantations
    val cs = is.colonistsPlantation

    intro + "\n\n" +
      s"\tcorn:\t${cs.corn}/${ps.corn}\n" +
      s"\tindigo:\t${cs.indigo}/${ps.indigo}\n" +
      s"\tsugar:\t${cs.sugar}/${ps.sugar}\n" +
      s"\ttobacco:\t${cs.tobacco}/${ps.tobacco}\n" +
      s"\tcoffee:\t${cs.coffee}/${ps.coffee}\n" +
      s"\tquarry:\t${cs.quarry}/${ps.quarry}\n"
  }

  def defaultMenu(gs: GameState, me: PlayerState, other: PlayerState): MenuState = {
    val choices = List(
      MenuChoice("pb",
                 "Print my buildings.",
                 () => ActionResult(showBuildingState(me.buildings), None, None)),
      MenuChoice("pi",
                 "Print my island.",
                 () => ActionResult(showIslandState(me.island), None, None)),
      MenuChoice("pob",
                 "Print the other player's buildings.",
                 () => ActionResult(showBuildingState(other.buildings), None, None)),
      MenuChoice("poi",
                 "Print the other player's island.",
                 () => ActionResult(showIslandState(other.island), None, None))
    )

    new MenuState(choices)
  }

  def chooseRolesMenu(gs: GameState, me: PlayerState, other: PlayerState): MenuState = {
    def roleChoice(r: Role) = {
      val (name, cmd) = roleNames(r)
      val nextState = defaultMenu(gs, me, other)
      MenuChoice(cmd,
                 s"Choose the ${name} role.",
                 () => ActionResult(s"You chose ${name}.",
                                    Some(nextState),
                                    Some(RoleChosen(r))))
    }

    val choices = List(
      MenuChoice("li",
                 "List the available roles.",
                 () => ActionResult(showAvailableRoles(gs), None, None))
    )
    val roleChoices = gs.rolesDoubloons.keys.toList map roleChoice

    new MenuState(choices ++ roleChoices, false) + defaultMenu(gs, me, other)
  }

  def showAvailableRoles(gs: GameState): String = {
    val roles = gs.rolesDoubloons map { case (role, money) =>
      val (name, cmd) = roleNames(role)
      s"\t${cmd} (${money})\t${name}"
    }

    "Available roles:\n" + roles.mkString("\n")
  }
}

class MenuIO(in: BufferedReader, out: PrintStream) {
  val globalCommands = List(
    )

  // TODO: put this in a util file.
  def safeStringToInt(str: String): Option[Int] = {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt str.toInt
  }

  /**
   * Asks the user to choose from a list of alternatives.
   *
   * Blocks until the user reponds with a proper answer.
   */
  def chooseFromList[T](alternatives: List[T], toString: T => String): T = {
    printNumberedList(alternatives map toString)
    val max = alternatives.length + 1
    
    val validResponse = (s: String) => {
      safeStringToInt(s) flatMap { i =>
        if (1 <= i && i <= max) Some(i) else None
      }
    }

    val responseIndex = expectResponse[Int](validResponse)
    alternatives(responseIndex)
  }

  def expectResponse[T](pred: String => Option[T]): T = {
    pred(in.readLine()) match {
      case Some(x) => x
      case None => {
        println("I didn't understand that response.")
        expectResponse(pred)
      }
    }
  }

  def printNumberedList(xs: List[String]) = {
    xs.zipWithIndex foreach { case (s, i) =>
      println(f"$i%3d. $s%s")
    }
  }
}

