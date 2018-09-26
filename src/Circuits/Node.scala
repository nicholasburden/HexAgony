package Circuits

class Node(var id : Int) {
  private var adjacencies : Set[Node] = Set()
  def getAdjacencies : Set[Node] = adjacencies
  def addAdjacencies(adjs : Set[Node]) = {
    adjacencies = adjacencies ++ adjs
  }
}
