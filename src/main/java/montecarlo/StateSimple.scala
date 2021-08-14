package montecarlo

import hexagony._
import hsearch.HSearch

import scala.collection.mutable.ListBuffer

class StateSimple(var mod: Model, var player: Int, var visits: Int, var score: Double) extends Const {
  //Flag to prune node from tree
  var inferior = false

  def this() = this(new Model(5), 0, 0, 0)

  def this(board: Model) = this(board.copy(), 0, 0, 0)

  def this(state: StateSimple) = this(state.mod.copy(), state.player, state.visits, state.score)

  def setBoard(newMod: Model) = mod = newMod

  def setPlayer(p: Int) = player = p


  def setWinScore(newScore: Double) = score = newScore

  def getNextStates(): ListBuffer[StateSimple] = {
    //Returns list of next possible states, exluding any pruned (inferior) nodes
    val states: ListBuffer[StateSimple] = new ListBuffer[StateSimple]()
    val cells: ListBuffer[Cell] = this.mod.myCells(O).to[ListBuffer]
    for (i <- cells.indices) {


      val newState = new StateSimple(this.mod)
      newState.setPlayer(1 - this.player)
      newState.mod.playMove(cells(i), StateSimple.getColour(newState.player))
      states += newState

    }
    if (mod.count == 1 && StateSimple.getColour(this.player).equals(R)) {
      //Consider playing the pie rule
      val newState = new StateSimple(this.mod)
      val onlyRedMove = mod.myCells(R)(0)
      newState.setPlayer(1 - this.player)
      newState.mod.playPieRule(onlyRedMove)
      states += newState

    }
    states
  }

  def visit = this.visits += 1

  def addScore(s: Double) = {
    if (score != Integer.MIN_VALUE) score += s
  }

  def randomPlay = {
    //Play a random move
    val availablePositions = this.mod.myCells(O)
    val totalPossibilities = availablePositions.size
    val selectRandom = (Math.random() * totalPossibilities).asInstanceOf[Int]
    this.mod.playMove(availablePositions(selectRandom), StateSimple.getColour(this.player))
  }

  def changePlayer = player = 1 - player
}


object StateSimple extends Const {

  def getColour(playerNum: Int): Colour = {
    playerNum match {
      case 0 => R
      case 1 => B
    }
  }

  def checkCell(i: Int, j: Int, model: Model): Colour = {
    if (i < 0 || j < 0 || i >= model.N || j >= model.N) return null
    else return model.board(i)(j).colour
  }
}
