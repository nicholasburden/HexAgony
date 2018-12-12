package montecarlo

import hexagony._
import scala.collection.mutable.ListBuffer
class State(var mod : Model, var player : Int, var visits : Int, var score : Double) extends Const {
  def this() = this(new Model(5), 0, 0, 0)
  def this(board : Model) = this(board.copy(), 0, 0, 0)
  def this(state : State) = this(state.mod.copy(), state.player, state.visits, state.score)
  def setBoard(newMod : Model) = mod = newMod

  def setPlayer(p : Int) = player = p


  def setWinScore(newScore : Double) = score = newScore
  def getNextStates(): ListBuffer[State] = {
    val states : ListBuffer[State] = new ListBuffer[State]()
    val cells : ListBuffer[Cell] = this.mod.myCells(O).to[ListBuffer]
    for(i <- 0 until cells.size){
      val newState = new State(this.mod)
      newState.setPlayer(1-this.player)
      newState.mod.playMove(cells(i), State.getColour(newState.player))
      states += newState
    }
    states

  }
  def visit = this.visits+=1
  def addScore(s : Double) = {
    if(score != Integer.MIN_VALUE) score += s
  }
  def randomPlay = {
    val availablePositions = this.mod.myCells(O)
    val totalPossibilities = availablePositions.size
    val selectRandom = (Math.random() * totalPossibilities).asInstanceOf[Int]
    this.mod.playMove(availablePositions(selectRandom), State.getColour(this.player))
  }
  def changePlayer = player = 1 - player

}

object State extends Const{
  def getColour(playerNum : Int) : Colour = {
    playerNum match {
      case 0 => R
      case 1 => B
    }
  }
}
