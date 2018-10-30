package Circuits

abstract class Circuit {
  private val resistance : collection.mutable.Map[(Node, Node), Float] = collection.mutable.Map()
  def setResistance(node1 : Node, node2 : Node, res : Float) = {
    resistance((node1, node2)) = res
    resistance((node2, node1)) = res
  }
  def deleteNode(id: Int) = {
    nodes = nodes.filterNot(elm => elm.id == id)
    for(i <- 0 until nodes.size){
      nodes(i).id = i
    }

  }
  def getResistance(node1: Node, node2 : Node) : Float = {
    try {
      resistance((node1, node2))
    }
    catch{
      case _ =>
        val result : Float = Float.PositiveInfinity
        return result
    }
  }
  var nodes : List[Node]
  def getNodes : List[Node] = nodes
  def maxDegree : Int = {
    var max = 0
    for(node <- nodes){
      max = Math.max(max, node.getAdjacencies.size)
    }
    max
  }
  def addLink(id1 : Int, id2 : Int): Unit ={
    nodes(id1).addAdjacency(nodes(id2))
    nodes(id2).addAdjacency(nodes(id1))
  }

}
