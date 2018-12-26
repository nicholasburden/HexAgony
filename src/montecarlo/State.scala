package montecarlo

import hexagony._
import hsearch.HSearch

import scala.collection.mutable.ListBuffer
class State(var mod : Model, var player : Int, var visits : Int, var score : Double) extends Const {
  final val KNOWLEDGE_THRESHOLD = 30
  var inferior = false
  def this() = this(new Model(5), 0, 0, 0)
  def this(board : Model) = this(board.copy(), 0, 0, 0)
  def this(state : State) = this(state.mod.copy(), state.player, state.visits, state.score)
  def setBoard(newMod : Model) = mod = newMod

  def setPlayer(p : Int) = player = p


  def setWinScore(newScore : Double) = score = newScore
  def getNextStates(hRed : HSearch, hBlue : HSearch): ListBuffer[State] = {
    val states : ListBuffer[State] = new ListBuffer[State]()
    val cells : ListBuffer[Cell] = this.mod.myCells(O).to[ListBuffer]
    for(i <- 0 until cells.size){

      if(visits < KNOWLEDGE_THRESHOLD || !cellInFillin(this.mod, cells(i), hRed, hBlue)){
        val newState = new State(this.mod)
        newState.setPlayer(1-this.player)
        newState.mod.playMove(cells(i), State.getColour(newState.player))
        states += newState
      }

    }
    if(mod.count == 1 && State.getColour(this.player).equals(R)){

      val newState = new State(this.mod)
      val onlyRedMove = mod.myCells(R)(0)
      newState.setPlayer(1-this.player)
      newState.mod.playPieRule(onlyRedMove)
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
  def cellInFillin(model : Model, cell : Cell, hRed : HSearch, hBlue : HSearch) : Boolean = {
    var hsearch = player match{
      case 0 => hRed
      case 1 => hBlue
    }
    for(cell1 <- model.myCells(hsearch.colour); cell2 <- model.myCells(hsearch.colour)){
      if(hsearch.getStrongCarriers(cell1, cell2, false).nonEmpty){
        this.inferior = true
        return true
      }
      val i = cell.i
      val j = cell.j
      //DEAD CELLS
      val colour = player match{
        case 0 => B
        case 1 => R
      }
      if(colour.equals(R)){
        if(State.checkCell(i,j-1,model) == R && State.checkCell(i+1,j,model) == R && State.checkCell(i+1,j+1,model) == R){
          this.inferior = true
          return true
        }
        if(State.checkCell(i,j-11,model) == R && State.checkCell(i+1,j,model) == R && State.checkCell(i-1,j-1,model) == R){
          this.inferior = true
          return true
        }
        if(State.checkCell(i+1,j+1,model) == R && State.checkCell(i+1,j,model) == R && State.checkCell(i,j+1,model) == R){
          this.inferior = true
          return true
        }
        if(State.checkCell(i+1,j+1,model) == R && State.checkCell(i,j+1,model) == R && State.checkCell(i-1,j,model) == R){
          this.inferior = true
          return true
        }
        if(State.checkCell(i,j+1,model) == R && State.checkCell(i-1,j,model) == R && State.checkCell(i-1,j-1,model) == R){
          this.inferior = true
          return true
        }
        if(State.checkCell(i-1,j-11,model) == R && State.checkCell(i-1,j,model) == R && State.checkCell(i,j-1,model) == R){
          this.inferior = true
          return true
        }
      }
      if(colour.equals(B)){
        if(State.checkCell(i,j-1,model) == B && State.checkCell(i+1,j,model) == B && State.checkCell(i+1,j+1,model) == B){
          this.inferior = true
          return true
        }
        if(State.checkCell(i,j-11,model) == B && State.checkCell(i+1,j,model) == B && State.checkCell(i-1,j-1,model) == B){
          this.inferior = true
          return true
        }
        if(State.checkCell(i+1,j+1,model) == B && State.checkCell(i+1,j,model) == B && State.checkCell(i,j+1,model) == B){
          this.inferior = true
          return true
        }
        if(State.checkCell(i+1,j+1,model) == B && State.checkCell(i,j+1,model) == B && State.checkCell(i-1,j,model) == B){
          this.inferior = true
          return true
        }
        if(State.checkCell(i,j+1,model) == B && State.checkCell(i-1,j,model) == B && State.checkCell(i-1,j-1,model) == B){
          this.inferior = true
          return true
        }
        if(State.checkCell(i-1,j-11,model) == B && State.checkCell(i-1,j,model) == B && State.checkCell(i,j-1,model) == B){
          this.inferior = true
          return true
        }
      }

    }
    return false
  }
}

object State extends Const{
  def getColour(playerNum : Int) : Colour = {
    playerNum match {
      case 0 => R
      case 1 => B
    }
  }
  def checkCell(i : Int, j : Int, model : Model) : Colour = {
    if(i < 0 || j < 0 || i >= model.N || j >= model.N) return null
    else return model.board(i)(j).colour
  }


}
