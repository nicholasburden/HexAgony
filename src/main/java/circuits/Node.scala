package circuits

class Node(var id : Int) {
  private var adjacencies : Set[Node] = Set()

  def getAdjacencies : Set[Node] = adjacencies

  def addAdjacencies(adjs : Set[Node]) = {
    adjacencies = adjacencies ++ adjs
  }

  def addAdjacency(adj : Node) = {
    adjacencies = adjacencies + adj
  }
}
