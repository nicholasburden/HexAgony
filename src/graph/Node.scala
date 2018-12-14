package graph
//may not need id
class Node(var id : Int){
  private var adjacencies : Set[Node] = Set()
  def addAdjacent(node : Node) = {
    adjacencies += node
  }
  def addAdjacencies(nodes : Set[Node]) = {
    adjacencies = adjacencies | nodes
  }
  def removeAdjacent(node: Node) = {
    adjacencies -= node
  }
  def getAdjacencies : Set[Node] = adjacencies
  def pretty(size : Int) : String = {
    val x = ((id-1) % size) + 1
    val y = ((id-1) / size) + 1
    return "(" + x + ", " + y + ")"
  }
  def adjacentTo(i : Int) : Boolean ={
    for (node <- adjacencies){
      if(node.id == i){
        return true
      }
    }
    return false
  }
}