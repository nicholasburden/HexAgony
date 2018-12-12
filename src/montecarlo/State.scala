package montecarlo

import hexagony._
import scala.collection.mutable.ListBuffer
class State(var board : Model, var playerNo : Int, var visitCount : Int, var winScore : Double) extends Const {
  def this() = this(new Model(5), 0, 0, 0)
  def this(board : Model) = this(board.copy(), 0, 0, 0)
  def this(state : State) = this(state.getBoard.copy(), state.playerNo, state.getVisitCount, state.getWinScore)
  def getBoard() = board
  def setBoard(newBoard : Model) = board = newBoard
  def getPlayerNo = playerNo
  def setPlayerNo(pNum : Int) = playerNo = pNum
  def getOpponent = 3-playerNo
  def getVisitCount = visitCount
  def getWinScore = winScore
  def setWinScore(newWinScore : Double) = winScore = newWinScore
  def getAllPossibleStates(): ListBuffer[State] = {
    val possibleStates : ListBuffer[State] = new ListBuffer[State]()
    val availablePositions : ListBuffer[Cell] = this.board.myCells(O).to[ListBuffer]
    for(i <- 0 until availablePositions.size){
      val newState = new State(this.board)
      newState.setPlayerNo(3-this.playerNo)
      newState.getBoard.playMove(availablePositions(i), State.getColour(newState.getPlayerNo))
      possibleStates += newState
    }
    possibleStates

  }
  def incrementVisit = this.visitCount+=1
  def addScore(score : Double) = {
    if(winScore != Int.MinValue) winScore += score
  }
  def randomPlay = {
    val availablePositions = this.board.myCells(O)
    val totalPossibilities = availablePositions.size
    val selectRandom = (Math.random() * totalPossibilities).asInstanceOf[Int]
    this.board.playMove(availablePositions(selectRandom), State.getColour(this.playerNo))
  }
  def togglePlayer = playerNo = 3 - playerNo

}

object State extends Const{
  def getColour(playerNum : Int) : Colour = {
    playerNum match {
      case 1 => R
      case 2 => B
    }
  }
}
