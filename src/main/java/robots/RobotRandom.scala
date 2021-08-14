
import hexagony._
import heuristic._
import hsearch._
import scala.util.Random

class RobotRandom(model: Model, timelimit: Long, pierule: Boolean, colour: Colour)
  extends Robot(model: Model, timelimit: Long, pierule: Boolean, colour: Colour) {

  private def myMove(): Cell = {
    //Choose a random cell from open tiles
    val rnd = new Random
    model.myCells(O).toVector(rnd.nextInt(model.myCells(O).size))
  }


  // Your method for deciding whether to play the pie rule
  private def myPie(firstmove: Cell): Boolean = {
    //Random boolean
    val rnd = new Random
    rnd.nextBoolean()
  }

  // ------------------------------------------------------------------------------------------------
  /* The parameters passed to the robot are:
   * model - a blank copy of the game board model, complete with useful methods, updated by the controller
   * timelimit - the time (in milliseconds) that your robot has to complete their move
   * pierule - whether or not the pie rule is available to use after the first move
   * colour - the colour of your robot (R or B)
   *
   * Other values that are accessible to the robot include:
   * board - the game board in its current state
   * N - the size of the board
   * count - the number of tokens on the board
   * piePlayed - whether or not the pie rule has been played in this game
   * lastCell - the Cell on which the last move was played
   * othercolour - the colour of the other player
   *
   * You should reference the cell at coordinate (i, j) by the expression board(i)(j).
   * This value represents the cell at the given location on the board.
   * Coordinates are zero-indexed, meaning that they range from 0 to N - 1 in each direction.
   * board(i)(j) has attribute colour; model.colour(cell) also returns the colour of cell.
   * Many other helpful methods are available in the model.
   *
   */
  // ------------------------------------------------------------------------------------------------

  var move: Cell = null // this should hold the move that will be returned
  var pie = false // this should hold the pie rule decision
  var stop = false // used to end computation at completion of turn
  val lag = 50 // used for self-imposed time limit


  def makeMove(): Cell = {
    stop = false
    // Execute your move method with the given time restriction
    try {
      move = timedRun[Cell](timelimit - lag)(myMove())
    }
    catch {
      case ex: Exception =>
    } // something has gone wrong, such as a timeout
    stop = true // stop the computation within the method
    val rnd = new Random

    println(move)
    if (!model.legal(move)) move = randomMove(model)
    return move
  }

  def pieRule(firstmove: Cell): Boolean = {
    stop = false
    // Execute your pie method with the given time restriction
    try {
      pie = timedRun[Boolean](timelimit - lag)(myPie(firstmove))
    }
    catch {
      case ex: Exception =>
    } // something has gone wrong, such as a timeout
    stop = true // stop the computation within the method
    return pie
  }

  private def randomMove(mod: Model): Cell = {
    val open = mod.myCells(O)
    val randmove = open((Math.random() * open.length).toInt)
    println("Move chosen randomly: " + randmove.toString())
    randmove
  }
}
