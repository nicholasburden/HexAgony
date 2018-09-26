
import hexagony._
import Heuristic._

class RobotAlphaBeta(model: Model, timelimit: Long, pierule: Boolean, colour: Colour)
  extends Robot(model: Model, timelimit: Long, pierule: Boolean, colour: Colour) {
  val DEPTH = 3

  private def myMove(): Cell = {

    move = null
    val mod = model.copy()
    val open = mod.myCells(O)
    val alpha = Float.NegativeInfinity
    val beta = Float.PositiveInfinity

    var topScore = Float.NegativeInfinity
    println("GOING")
    for (cell <- open) {

      val mod2 = result(mod, cell, colour)
      println("GOING2")
      if (!stop) {
        println("GOING3")
        val score = min(mod2, DEPTH-1, alpha, beta)
        println("GOING4")
        println(cell + " score = " + score)
        if ((score >= topScore)) { // cell is a winning move
          topScore = score
          move = cell
        }
        println("GOING5")

      }
    }
    println(move.i + ", " + move.j)


    return move

  }
  def min(model : Model, depth : Int, _alpha : Float, _beta : Float) : Float = {
    println(depth)
    val alpha = _alpha
    var beta = _beta
   // println("Start next" + depth)
    if(model.solution(colour)){
      return Float.PositiveInfinity
    }
    else if(model.solution(othercolour)){
      return Float.NegativeInfinity
    }

    else if(depth == 0){
      println("Heuristic")
      val heuristic = new ResistanceHeuristic

      return heuristic.evaluate(model, colour)

    }
    else{

      //println("Finished checking if leaf")
      var bestVal = Float.PositiveInfinity
      for (cell <- model.myCells(O)){

        val value = max(result(model, cell, othercolour), depth - 1, alpha, beta)
        bestVal = Math.min(bestVal, value)
        beta = Math.min(beta, bestVal)
        if (beta <= alpha){
          return bestVal
        }

      }
      return bestVal
    }
  }

  def max(model : Model, depth : Int, _alpha : Float, _beta : Float) : Float = {
    println(depth)
    var alpha = _alpha
    val beta = _beta
   // println("Start next" + depth)
    if(model.solution(colour)){
      return Float.PositiveInfinity
    }
    else if(model.solution(othercolour)){
      return Float.NegativeInfinity
    }
    else if(depth == 0){
      val heuristic = new ResistanceHeuristic

      val x = heuristic.evaluate(model, colour)
      println("HELLO")
      x
    }
    else{
     // println("Finished checking if leaf")
      var bestVal = Float.NegativeInfinity
      for (cell <- model.myCells(O)){
        val value = min(result(model, cell, colour), depth - 1, alpha, beta)
        bestVal = Math.max(bestVal, value)
        alpha = Math.max(alpha, bestVal)
        if (beta <= alpha){
          return bestVal
        }

      }
      return bestVal
    }
  }

  // Your method for deciding whether to play the pie rule
  private def myPie(firstmove: Cell): Boolean = false

  private def result(mod: Model, cell: Cell, col: Colour): Model = {
    val mod2 = mod.copy()
    mod2.playMove(cell, col)
    return mod2
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
    try { move = timedRun[Cell](timelimit - lag)(myMove()) }
    catch { case ex: Exception => } // something has gone wrong, such as a timeout
    stop = true // stop the computation within the method
    println(move)
    if (!model.legal(move)) move = randomMove(model)
    return move
  }

  def pieRule(firstmove: Cell): Boolean = {
    stop = false
    // Execute your pie method with the given time restriction
    try { pie = timedRun[Boolean](timelimit - lag)(myPie(firstmove)) }
    catch { case ex: Exception => } // something has gone wrong, such as a timeout
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
