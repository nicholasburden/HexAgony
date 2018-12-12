package montecarlo

import hexagony._
import tree.{Node, Tree}

class MonteCarloTreeSearch(N : Int) extends Const{
  final val WIN = 10
  var otherColour : Colour = null


  def findNextMove(model : Model, colour : Colour, timelimit : Long) = {
    otherColour = getOtherColour(colour)
    val start = System.currentTimeMillis()
    val end = start + 20000//Math.min(15000, timelimit/1.5)

    var tree : Tree = new Tree(new Node(N))
    val root : Node = tree.root
    root.state.setModel(model)
    root.state.setColour(otherColour)
    while(System.currentTimeMillis() < end) {
      //Selection
      val goodNode = selectGoodNode(root)

      //expansion
      if(goodNode.state.model.winner == O){ //no winner yet
        expand(goodNode)
      }

      //Simulation
      var nodeToExplore = goodNode
      if(goodNode.childArray.nonEmpty){
        nodeToExplore = goodNode.getRandChild()
      }
      val playoutResult = simPlayout(nodeToExplore)

      //update
      backProp(nodeToExplore, playoutResult)
    }
    val best : Node = root.getMaxChild()
    tree.setRoot(best)
    best.state.model
  }

  private def selectGoodNode(rootNode : Node) : Node = {
    var node = rootNode
    while(node.childArray.size != 0){
      node = findBestNode(node)
    }
    node
  }
  private def expand(node : Node): Unit ={
    val possibleStates = node.state.getNextStates()
    possibleStates.foreach(state => {
      val newNode = new Node(state)
      newNode.setParent(node)
      newNode.state.setColour(node.state.getOtherColour(state.colour))
      node.childArray += newNode
    })
  }

  private def backProp(nodeToExplore : Node, c : Colour) = {
    var tempNode = nodeToExplore
    while(tempNode != null){
      tempNode.state.doVisit
      if(tempNode.state.colour == c){
        tempNode.state.addScore(WIN)
      }
      tempNode = tempNode.parent
    }
  }
  private def simPlayout(node : Node) : Colour = {
    var tempNode = new Node(node)
    var tempState = tempNode.state
    var winner = tempState.model.winner
    if(winner == otherColour) {
      tempNode.parent.state.setScore(Integer.MIN_VALUE)
      return winner
    }
    while(winner == O){
      tempState.changePlayer
      tempState.playARandomMove
      winner = tempState.model.winner
    }
    return winner
  }
  private def getOtherColour(c : Colour) : Colour = {
    c match{
      case R => B
      case B => R

    }
  }
  private def uct(visitTotal : Int, nodeScore : Double, nodeVisit : Int) : Double = {
    if(nodeVisit == 0){
      return Integer.MAX_VALUE
    }
    (nodeScore / nodeVisit.asInstanceOf[Double]) + 1.41 * Math.sqrt(Math.log(visitTotal) / nodeVisit.asInstanceOf[Double])
  }
  private def findBestNode(node : Node) = {
    val parentCount = node.state.visit
    var max : Double = Double.NegativeInfinity
    var n : Node = null
    for(i <- 0 until node.childArray.size){
      val c = node.childArray(i)
      val v = uct(parentCount, c.state.score, c.state.visit)
      if(v > max){
        max = v; n = c
      }
    }
    n
  }
}
