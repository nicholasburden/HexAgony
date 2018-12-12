package montecarlo

import hexagony._
import scala.collection.mutable.ListBuffer
class State(var model : Model, var colour : Colour, var visit : Int, var score : Double) extends Const {
  def this(N : Int) = this(new Model(N), null, 0, 0)
  def this(mod : Model) = this(mod.copy(), null, 0, 0)
  def this(state : State) = this(state.model.copy(), state.colour, state.visit, state.score)
  //def getBoard() = board
  def setModel(mod : Model) : Unit = model = mod
  //def getPlayerNo = playerNo
  def setColour(c : Colour): Unit = colour = c
  def getOtherColour(c : Colour) : Colour = {
    c match{
      case R => B
      case B => R
    }
  }

  def setScore(newWinScore : Double) = score = newWinScore
  def getNextStates(): ListBuffer[State] = {
    val states : ListBuffer[State] = new ListBuffer[State]()
    val cells : ListBuffer[Cell] = this.model.myCells(O).to[ListBuffer]
    for(i <- 0 until cells.size){
      val newState = new State(this.model)
      newState.setColour(getOtherColour(this.colour))
      newState.model.playMove(cells(i), newState.colour)
      states += newState
    }
    states

  }
  def doVisit = this.visit+=1
  def addScore(s : Double) = {
    if(score != Integer.MIN_VALUE) score += s
  }
  def playARandomMove = {
    val availablePositions = this.model.myCells(O)
    val totalPossibilities = availablePositions.size
    val selectRandom = (Math.random() * totalPossibilities).asInstanceOf[Int]
    this.model.playMove(availablePositions(selectRandom), this.colour)
  }
  def changePlayer = colour = getOtherColour(colour)

}


