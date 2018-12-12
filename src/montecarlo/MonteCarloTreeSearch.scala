package montecarlo

import hexagony.Model
import tree.{Node, Tree}

class MonteCarloTreeSearch(var level : Int) {
  final val WIN_SCORE = 10
  var opponent = 0
  def this() = this(3)
  def getLevel = level
  def setLevel(l : Int) = level = l
  def getMillisForCurrentLevel() = 2 * (level - 1) + 1
  def findNextMove(board : Model, playerNo : Int) = {
    val start = System.currentTimeMillis()
    val end = start + 60 * getMillisForCurrentLevel()
    opponent = 3-playerNo
    val tree : Tree = new Tree()
    val rootNode : Node = tree.getRoot
    rootNode.getState().setBoard(board)
    rootNode.getState().setPlayerNo(opponent)
    while(System.currentTimeMillis() < end) {
      //Selection
      val promisingNode = selectPromisingNode(rootNode)

      //expansion
      if(promisingNode.getState.getBoard.checkStatus == Model.IN_PROGRESS){
        expandNode(promisingNode)
      }

      //Simulation
      var nodeToExplore = promisingNode
      if(promisingNode.getChildArray.size > 0){
        nodeToExplore = promisingNode.getRandomChildNode()
      }
      val playoutResult = simulateRandomPlayout(nodeToExplore)

      //update
      backPropogation(nodeToExplore, playoutResult)
    }
    val winnerNode : Node = rootNode.getChildWithMaxScore()
    tree.setRoot(winnerNode)
    winnerNode.getState().getBoard()
  }

  private def selectPromisingNode(rootNode : Node) : Node = {
    var node = rootNode
    while(node.getChildArray.size != 0){
      node = UCT.findBestNodeWithUCT(node)
    }
    node
  }
  private def expandNode(node : Node): Unit ={
    val possibleStates = node.getState.getAllPossibleStates()
    possibleStates.foreach(state => {
      val newNode = new Node(state)
      newNode.setParent(node)
      newNode.getState.setPlayerNo(node.getState.getOpponent)
      node.getChildArray += newNode
    })
  }

  private def backPropogation(nodeToExplore : Node, playerNo : Int) = {
    var tempNode = nodeToExplore
    while(tempNode != null){
      tempNode.getState.incrementVisit
      if(tempNode.getState().getPlayerNo == playerNo){
        tempNode.getState.addScore(WIN_SCORE)
      }
      tempNode = tempNode.getParent
    }
  }
  private def simulateRandomPlayout(node : Node) : Int = {
    var tempNode = new Node(node)
    var tempState = tempNode.getState()
    var boardStatus = tempState.getBoard.checkStatus
    if(boardStatus == opponent) {
      tempNode.getParent.getState.setWinScore(Integer.MIN_VALUE)
      return boardStatus
    }
    while(boardStatus == Model.IN_PROGRESS){
      tempState.togglePlayer
      tempState.randomPlay
      boardStatus = tempState.getBoard.checkStatus
    }
    return boardStatus
  }
}
