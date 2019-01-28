package experiments

import moveordering.MoveOrdering
import hexagony._
import heuristic._
import hsearch._
import pierule._


class RobotAlphaBetaResistance(model: Model, timelimit: Long, pierule: Boolean, colour: Colour)
  extends Robot(model: Model, timelimit: Long, pierule: Boolean, colour: Colour) {
  val DEPTH = 2
  val pieRule = new PieRule(model.N)
  val pieRuleTable = pieRule.getTable

  private def myMove(): Cell = {

    val mod = model.copy()
    val moveOrdering = new MoveOrdering

    moveOrdering.initial(mod)

    //Get a list of open moves in order of strength
    var open = moveOrdering.getOrdering(mod)


    //Swap board if pie rule has been played
    if (model.pie && !HSearch.p) HSearch.pie

    val alpha = Double.NegativeInfinity
    val beta = Double.PositiveInfinity
    var topScore = Double.NegativeInfinity

    //H-Search objects for respective colours
    val hme = new HSearch(mod, colour)
    val hthem = new HSearch(mod, othercolour)

    //Perform initialisation
    hme.initial
    hthem.initial



    //Search for strong and weak connections
    hme.search(timelimit/3)
    hthem.search(timelimit/3)

    //Get set of cells that are in a carrier of an opponent semi-connection
    val weakCarrier = hthem.getUnionOfWeakConnections

    //Reduce move set to cells in opponent weak carriers
    if (weakCarrier.nonEmpty) open = weakCarrier.toList


    val boundaries = hme.boundarySet.toList

    //Check if there is a strong carrier from one boundary to another, restricting moves to that carrier if so
    val strongCarrier = hme.getStrongCarriers(boundaries(0), boundaries(1), true)
    if (strongCarrier.nonEmpty) open = strongCarrier.toList

    //Filter out cells in an opponent's strong carrier, since playing one is useless
    val ordering = open.filter(x => !hthem.strong.contains(x))

    //LOOP INVARIANT: move has the highest minimax value considered so far
    for (cell <- ordering) {
      //Play move
      val mod2 = result(mod, cell, colour)
      val hme2 = hme.makeMove(cell.i, cell.j, colour)
      val hthem2 = hthem.makeMove(cell.i, cell.j, colour)

      if (!stop) {

        var score = 0.0d

        //Update move selection order for recursive calls
        val mo = moveOrdering.addMovesFor(cell, mod)
        score = min(mod2, DEPTH - 1, alpha, beta, hme2, hthem2, mo)

        //check for case where opponent uses pie rule
        if (othercolour.equals(B) && mod2.count == 1 && pierule) {


          //play pie rule
          val modPie = result(mod, cell, B)
          HSearch.pie
          modPie.pie = true
          hme.model.pie = true
          hthem.model.pie = true
          hthem.colour = R
          hme.colour = B

          //Get value of board after pie rule is played
          val value = max(modPie, DEPTH - 1, alpha, beta, hthem.makeMove(cell.i, cell.j, B), hme.makeMove(cell.i, cell.j, B), mo)

          //undo pie rule
          hthem.colour = B
          hme.colour = R
          hme.model.pie = false
          hthem.model.pie = false
          modPie.pie = false
          HSearch.pie
          score = Math.min(score, value)
        }

        if (score > topScore) {
          move = cell
          topScore = score
        }
      }
    }
    println(move)
    return move



  }

  def min(model: Model, depth: Int, _alpha: Double, _beta: Double, hme: HSearch, hthem: HSearch, mo: MoveOrdering): Double = {

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
      val heuristic = new ResistanceHeuristic
      return heuristic.evaluate(model, colour, hme, hthem)

    }
    else {
      var bestVal = Double.PositiveInfinity

      //LOOP INVARIANT: bestVal is the smallest minimax value found so far
      for (cell1 <- mo.getOrdering(model)) {

        val cell = model.board(cell1.i)(cell1.j)

        //Recursive call
        val value = max(result(model, cell, othercolour), depth - 1, alpha, beta, hme.makeMove(cell.i, cell.j, othercolour), hthem.makeMove(cell.i, cell.j, othercolour), mo.addMovesFor(cell, model))

        bestVal = Math.min(bestVal, value)
        beta = Math.min(beta, bestVal)
        if (beta <= alpha) {
          //Prune tree
          return bestVal
        }

      }


      bestVal
    }
  }

  def max(model: Model, depth: Int, _alpha: Double, _beta: Double, hme: HSearch, hthem: HSearch, mo: MoveOrdering): Double = {

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
      //Reached leaf, use heuristic
      val heuristic = new ResistanceHeuristic

      return heuristic.evaluate(model, colour, hme, hthem)

    }
    else {

      var bestVal = Double.NegativeInfinity
      val ordering = mo.getOrdering(model).filter(x => !hthem.strong.contains(x))

      //LOOP INVARIANT: bestVal is the largest minimax value found so far
      for (cell1 <- ordering) {

        val cell = model.board(cell1.i)(cell1.j)

        //Recursive call
        val value = min(result(model, cell, colour), depth - 1, alpha, beta, hme.makeMove(cell.i, cell.j, colour), hthem.makeMove(cell.i, cell.j, colour), mo.addMovesFor(cell, model))
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
