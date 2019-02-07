

import hexagony._
import hsearch.HSearch
import montecarlo.NodeSimple
import pierule.PieRule


class RobotMonteCarloSimple(model: Model, timelimit: Long, pierule: Boolean, colour: Colour)
  extends Robot(model: Model, timelimit: Long, pierule: Boolean, colour: Colour) {
  val pieRule = new PieRule(model.N)
  val pieRuleTable = pieRule.getTable
  var ohercolour = colour match {
    case R => B
    case B => R

  }
  //Score added to each node when node is a winning node
  var WIN_SCORE = 10

  //Total time for main loop of algorithm
  var MCTS_TIME = 10000

  //Time in ms allowed for a run of HSEARCH

  var (player, otherPlayer) = colour match {
    case R => (0, 1)
    case B => (1, 0)
  }

  def myMove(): Cell = {





    val mod = model.copy()

    //Set time allowed to run
    val start = System.currentTimeMillis()
    val end = start + MCTS_TIME


    //Initialise game tree with initial board as root (with opponent as state player since they moved last)
    val tree: NodeSimple = new NodeSimple()
    val rootNode: NodeSimple = tree

    rootNode.state.setBoard(mod)
    rootNode.state.setPlayer(otherPlayer)

    //LOOP INVARIANT: Every node in the tree is labelled with the total number of times it has been visited
    //Every node also labelled with the total number of times it has led to a win for its corresponding player multiplied by WIN
    while (System.currentTimeMillis() < end) {
      //Select a good node according to UCT
      val goodNode = findGoodNode(rootNode)

      //Expand this node to its children if it is not terminal
      if (goodNode.state.mod.checkIfFinished == -1) {
        val states = goodNode.state.getNextStates()
        states.foreach(state => {
          val next = new NodeSimple(state)
          next.setParent(goodNode)
          next.state.setPlayer(1 - goodNode.state.player)
          goodNode.childArray += next
        })
      }

      //Simulate a random game
      var node = goodNode
      if (goodNode.childArray.nonEmpty) {
        node = goodNode.getRandomChildNode()
      }
      var tempN = new NodeSimple(node)
      var tempS = tempN.state
      var modelVal = tempS.mod.checkIfFinished()
      if (modelVal == otherPlayer) {
        tempN.parent.state.setWinScore(Integer.MIN_VALUE)
      }
      while (modelVal == -1) {
        tempS.changePlayer
        tempS.randomPlay
        modelVal = tempS.mod.checkIfFinished
      }
      val playOut = modelVal

      //Update using back-propogation to reflect result
      backProp(node, playOut)
    }
    val best: NodeSimple = rootNode.getChildWithMaxScore()


    //Find last cell played in the best new state (ie the best move to be played)
    for (cell <- best.state.mod.myCells(colour)) {
      if (!mod.myCells(colour).contains(cell))
        return cell
    }
    //If no move is chosen, return null
    null
  }

  private def findGoodNode(root: NodeSimple): NodeSimple = {
    //Greedily choose best leaf node according to UCT
    var node = root
    while (node.childArray.nonEmpty) {
      //Find best child according to UCT
      node = findBestNode(node)
    }
    node
  }

  private def backProp(node: NodeSimple, player: Int) = {
    var temp = node
    while (temp != null) {
      //Update total number of visits
      temp.state.visit
      if (temp.state.player == player) {
        //Add win score to winning player
        temp.state.addScore(WIN_SCORE)
      }
      temp = temp.parent
    }
  }

  private def uct(visitTotal: Int, nodeScore: Double, nodeVCount: Int): Double = {
    if (nodeVCount == 0) {
      return Integer.MAX_VALUE
    }
    (nodeScore / nodeVCount.asInstanceOf[Double]) + 1.4143 * Math.sqrt(Math.log(visitTotal) / nodeVCount.asInstanceOf[Double])
  }

  private def findBestNode(node: NodeSimple) = {
    //Find the best child with respect to its UCT value
    val parentVCount = node.state.visits
    var max: Double = Double.NegativeInfinity
    var n: NodeSimple = null
    for (i <- node.childArray.indices) {
      val c = node.childArray(i)
      val v = uct(parentVCount, c.state.score, c.state.visits)
      if (v > max) {
        max = v;
        n = c
      }
    }
    n
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
