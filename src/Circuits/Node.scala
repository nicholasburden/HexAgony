package Circuits

class Node(val id : Int) {
  private var adjacencies : Set[Node] = Set()
  def getAdjacencies : Set[Node] = adjacencies
  def addAdjacencies(adjs : Set[Node]) = {
    adjacencies = adjacencies ++ adjs
  }
}
