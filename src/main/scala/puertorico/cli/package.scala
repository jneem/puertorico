package org.puertorico

import java.io.{ BufferedReader, PrintStream }
import org.slf4j.{ Logger, LoggerFactory }
import collection.mutable.HashMap
import org.puertorico._

package object cli {
  // There are various things that could happen as the result of
  // an action:
  //  - some text could be displayed
  //  - we could enter a new MenuState
  //  - we could send a message to the PlayerActor.
  case class ActionResult(
    val display:  String,
    val newState: NextState,
    val message:  Option[Any]
  )

  // The result is Option[MenuState] because the special
  // value None means that the current state should be repeated.
  case class MenuChoice(
    val commandString: String,
    val description:   String,
    val action:        () => ActionResult
  )

  /**
   * A MenuChoice needs to say what state to enter next.
   * There are three choices: go to a specified state,
   * stay in the same state, or go to an idle state.
   *
   * An idle state means that we are waiting for a response from the game
   * manager, and not accepting user input.
   */
  sealed trait NextState
  case object SameState extends NextState
  case object IdleState extends NextState
  case class NewState(menu: MenuState) extends NextState

  class MenuState(private val initChoices: List[MenuChoice]) {
    lazy val logger = LoggerFactory.getLogger("MenuState")

    if (initChoices exists (_.commandString == "?")) {
      logger.warn("Trying to re-assign the help shortcut.")
    }
    private lazy val choices = initChoices :+ helpChoice

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
        ActionResult(display, SameState, None)
      }

      ret
    }

    def hasAction(act: String) = actionMap contains act

    def takeAction(act: String) = actionMap(act).action()

    def +(other: MenuState) = {
      // warn if a binding collides.
      val myBindings = (initChoices map (_.commandString)).toSet
      val otherBindings = (other.initChoices map (_.commandString)).toSet
      if (!(myBindings intersect otherBindings).isEmpty) {
        logger.warn("Found a colliding binding.")
      }

      new MenuState(initChoices ++ other.initChoices)
    }
  }

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

  val goodNames = Map[Good, (String, String)](
    Corn -> ("Corn", "c"),
    Indigo -> ("Indigo", "i"),
    Sugar -> ("Sugar", "s"),
    Tobacco -> ("Tobacco", "t"),
    Coffee -> ("Coffee", "cf")
  )

  val plantationNames = Map[Plantation, (String, String)](
    Quarry -> ("Quarry", "q"),
    CornPlantation -> ("Corn", "c"),
    IndigoPlantation -> ("Indigo", "i"),
    SugarPlantation -> ("Sugar", "s"),
    TobaccoPlantation -> ("Tobacco", "t"),
    CoffeePlantation -> ("Coffee", "cf")
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
      s"\ttobac.:\t${cs.tobacco}/${ps.tobacco}\n" +
      s"\tcoffee:\t${cs.coffee}/${ps.coffee}\n" +
      s"\tquarry:\t${cs.quarry}/${ps.quarry}\n"
  }

  def showPlantationBundle(pb: PlantationBundle) = {
    val lines = pb map {
      case (p, count) =>
        s"\t${count} ${plantationNames(p)._1.toLowerCase}"
    }
    lines mkString "\n"
  }

  def showPlantations(gs: GameState): String = {
    "Available plantations:\n" + showPlantationBundle(gs.plantationsVisible)
  }

  def defaultMenu(gs: GameState, me: PlayerState, other: PlayerState): MenuState = {
    val choices = List(
      MenuChoice(
        "pp",
        "Print available plantations.",
        () => ActionResult(showPlantations(gs), SameState, None)
      ),
      MenuChoice(
        "pb",
        "Print my buildings.",
        () => ActionResult(showBuildingState(me.buildings), SameState, None)
      ),
      MenuChoice(
        "pi",
        "Print my island.",
        () => ActionResult(showIslandState(me.island), SameState, None)
      ),
      MenuChoice(
        "pob",
        "Print the other player's buildings.",
        () => ActionResult(showBuildingState(other.buildings), SameState, None)
      ),
      MenuChoice(
        "poi",
        "Print the other player's island.",
        () => ActionResult(showIslandState(other.island), SameState, None)
      )
    )

    new MenuState(choices)
  }

  def chooseRolesMenu(gs: GameState, me: PlayerState, other: PlayerState): MenuState = {
    def roleChoice(r: Role) = {
      val (name, cmd) = roleNames(r)
      MenuChoice(
        cmd,
        s"Choose the ${name} role.",
        () => ActionResult(s"You chose ${name}.", IdleState, Some(r))
      )
    }

    val choices = List(
      MenuChoice(
        "li",
        "List the available roles.",
        () => ActionResult(showAvailableRoles(gs), SameState, None)
      )
    )
    val roleChoices = gs.availableRolesDoubloons.keys.toList map roleChoice

    new MenuState(choices ++ roleChoices) + defaultMenu(gs, me, other)
  }

  def choosePlantationMenu(gs: GameState, me: PlayerState, other: PlayerState): MenuState = {
    // FIXME
    defaultMenu(gs, me, other)
  }

  def showAvailableRoles(gs: GameState): String = {
    val roles = gs.availableRolesDoubloons map {
      case (role, money) =>
        val (name, cmd) = roleNames(role)
        s"\t${cmd} (${money})\t${name}"
    }

    "Available roles:\n" + roles.mkString("\n")
  }

  /**
   * Returns a MenuState allowing the player to select between any non-zero goods in the bundle.
   */
  def chooseGoodMenu(gb: GoodBundle, nextState: Option[Good => MenuState]): MenuState = {
    def goodChoice(g: Good) = {
      val (name, cmd) = goodNames(g)
      val next = nextState match {
        case Some(f) => NewState(f(g))
        case None => IdleState
      }

      MenuChoice(
        cmd,
        s"Select ${name}.",
        () => ActionResult(s"You chose ${name}.", next, Some(g))
      )
    }

    val choices = gb map (x => goodChoice(x._1))
    new MenuState(choices.toList)
  }

  // TODO: be more helpful by only listing possibilities that are actually valid.
  def chooseGoodToLoadMenu(gs: GameState, me: PlayerState, other: PlayerState): MenuState = {
    val nextState = (g: Good) => chooseShipToLoadMenu(gs, me, other, g)

    chooseGoodMenu(me.goods, Some(nextState))
  }

  // TODO: be more helpful by only listing possibilities that are actually valid.
  def chooseExtraGoodMenu(gs: GameState, me: PlayerState, other: PlayerState): MenuState = {
    chooseGoodMenu(gs.goodsRemain, None)
  }

  def chooseShipToLoadMenu(gs: GameState, me: PlayerState, other: PlayerState, good: Good): MenuState = {
    def shipChoice(s: Ship) = {
      val (name, cmd) = s.size match {
        case 4 => ("4", "Ship of 4.")
        case 6 => ("6", "Ship of 6.")
        case 100 => ("w", "Wharf")
      }

      MenuChoice(cmd, name, () => ActionResult(s"You chose the ${name}.", IdleState, Some(s)))
    }

    val ships = gs.wharf +: gs.ships
    val validShips = ships filter (gs.canShipGoods(good, _, me))
    val choices = validShips map shipChoice
    new MenuState(choices) + defaultMenu(gs, me, other)
  }

  def yesOrNoMenu(gs: GameState, me: PlayerState, other: PlayerState, yesMsg: Any, noMsg: Any): MenuState = {
    val yes = MenuChoice("y", "Yes.", () => ActionResult("", IdleState, Some(yesMsg)))
    val no = MenuChoice("n", "No.", () => ActionResult("", IdleState, Some(noMsg)))

    new MenuState(List(yes, no)) + defaultMenu(gs, me, other)
  }
}

