package experiments

import graph.HexGraph
import moveordering.MoveOrdering
import hexagony._
import heuristic._
import pierule._

class RobotAlphaBetaFlow(model: Model, timelimit: Long, pierule: Boolean, colour: Colour)
  extends Robot(model: Model, timelimit: Long, pierule: Boolean, colour: Colour) {
  val DEPTH = 2
  val pieRule = new PieRule(model.N)
  val pieRuleTable = pieRule.getTable

  private def myMove(): Cell = {
    try{val mod = model.copy()
    //Get list of possible moves in order of strength
    val moveOrdering = new MoveOrdering

    moveOrdering.initial(mod)
    val open = moveOrdering.getOrdering(mod)

    val alpha = Double.NegativeInfinity
    val beta = Double.PositiveInfinity
    var topScore = Double.NegativeInfinity
    val graphRed = new HexGraph(model.N, R)
    val graphBlue = new HexGraph(model.N, B)
    graphRed.initialise(model)
    graphBlue.initialise(model)

    //LOOP INVARIANT: move has the highest minimax value considered so far
    for (cell <- open) {
      //Play move
      val mod2 = result(mod, cell, colour)
      if (!stop) {
        var score = 0.0d
        val(graphRedCopy, graphBlueCopy) = alterGraph(cell, colour, graphRed, graphBlue)
        //Update move selection order for recursive calls
        val mo = moveOrdering.addMovesFor(cell, mod)
        score = min(mod2, DEPTH - 1, alpha, beta, mo, graphRedCopy, graphBlueCopy)

        //check for case where opponent uses pie rule
        if (othercolour.equals(B) && mod2.count == 1 && pierule) {

          //Play pie rule
          val modPie = result(mod, cell, B)

          modPie.pie = true




          val(graphRedCopy, graphBlueCopy) = alterGraph(cell, othercolour, graphBlue, graphRed) //Swap colours
          //Get value of board after pie rule is played
          val value = max(modPie, DEPTH - 1, alpha, beta, mo, graphRedCopy, graphBlueCopy)
          //undo pie rule

          modPie.pie = false
          score = Math.min(score, value)

        }
        println(cell + " " + score)

        if (score > topScore) {
          move = cell
          topScore = score
        }
      }

    }


    move


    }
    catch{
      case e : Exception => e.printStackTrace(); null
    }




  }

  def min(model: Model, depth: Int, _alpha: Double, _beta: Double, mo: MoveOrdering, graphRed : HexGraph, graphBlue : HexGraph): Double = {

    val alpha = _alpha
    var beta = _beta

    if (model.solution(colour)) {
      //Winning move found
      return Double.PositiveInfinity
    }
    else if (model.solution(othercolour)) {
      //Losing move found
      return Double.MinValue
    }

    else if (depth == 0) {
      //Leaf node, use heuristic
      val heuristic = new FlowHeuristic
      return heuristic.evaluate(model, colour, graphRed, graphBlue)

    }
    else {

      var bestVal = Double.PositiveInfinity

      //LOOP INVARIANT: bestVal is the smallest minimax value found so far
      for (cell1 <- mo.getOrdering(model)) {

        val cell = model.board(cell1.i)(cell1.j)
        val (graphRedCopy, graphBlueCopy) = alterGraph(cell, othercolour, graphRed, graphBlue)
        //Recursive call
        val value = max(result(model, cell, othercolour), depth - 1, alpha, beta, mo.addMovesFor(cell, model), graphRedCopy, graphBlueCopy)

        bestVal = Math.min(bestVal, value)
        beta = Math.min(beta, bestVal)
        if (beta <= alpha) {
          //Prune tree
          return bestVal
        }

      }


      return bestVal
    }
  }

  def max(model: Model, depth: Int, _alpha: Double, _beta: Double, mo: MoveOrdering, graphRed : HexGraph, graphBlue : HexGraph): Double = {

    var alpha = _alpha
    val beta = _beta

    if (model.solution(colour)) {
      //Winning move found
      return Double.PositiveInfinity
    }
    else if (model.solution(othercolour)) {
      //Losing move found
      return Double.MinValue
    }
    else if (depth == 0) {
      //Leaf node, use heuristic
      val heuristic = new FlowHeuristic

      return heuristic.evaluate(model, colour, graphRed, graphBlue)

    }
    else {

      var bestVal = Double.NegativeInfinity


      //LOOP INVARIANT: bestVal is the largest minimax value found so far
      for (cell1 <- mo.getOrdering(model)) {

        val cell = model.board(cell1.i)(cell1.j)

        val (graphRedCopy, graphBlueCopy) = alterGraph(cell, colour, graphRed, graphBlue)
        //Recursive call
        val value = min(result(model, cell, colour), depth - 1, alpha, beta, mo.addMovesFor(cell, model), graphRedCopy, graphBlueCopy)
        bestVal = Math.max(bestVal, value)
        alpha = Math.max(alpha, bestVal)
        if (beta <= alpha) {
          //Prune tree
          return bestVal
        }

      }
      bestVal
    }
  }


  private def myPie(firstmove: Cell): Boolean = model.N <= 5 && pieRuleTable(firstmove.i)(firstmove.j)


  private def result(mod: Model, cell: Cell, col: Colour): Model = {
    val mod2 = mod.copy()
    mod2.playMove(cell, col)
    return mod2
  }


  private def alterGraph(cell: Cell, c : Colour, redGraph : HexGraph, blueGraph : HexGraph) : (HexGraph, HexGraph) = {
    val redGraph2 = redGraph.clone()
    val blueGraph2 = blueGraph.clone()

    if (c.equals(R)){
      redGraph2.placeYours(cell.i, cell.j)
      blueGraph2.placeTheirs(cell.i, cell.j)
    }
    else{
      redGraph2.placeTheirs(cell.i, cell.j)
      blueGraph2.placeYours(cell.i, cell.j)
    }



    (redGraph2, blueGraph2)
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
