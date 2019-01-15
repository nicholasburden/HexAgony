package circuits
import hexagony._
class HexCircuit(size :Int, colour : Colour) extends Circuit with Const{
  //A specific electrical circuit in the format of a hex board
  var nodes : List[Node] = List()

  //First boundary
  val firstNode = new Node(0)
  //Second boundary
  val lastNode = new Node(size*size + 1)
  nodes = lastNode :: nodes

  //Create nodes list
  for(i <- size*size to 1 by -1){
    nodes = new Node(i) :: nodes
  }
  nodes = firstNode :: nodes

  //Orient direction
  if(colour == B) {
    for (i <- 1 to size) {
      nodes(i).addAdjacency(firstNode)
    }
    firstNode.addAdjacencies(nodes.slice(1, size + 1).toSet)
    for (i <- (size - 1) * size + 1 to size * size) {
      nodes(i).addAdjacency(lastNode)
    }
    lastNode.addAdjacencies(nodes.slice((size - 1) * size + 1, size * size + 1).toSet)
  }
  else{
    for (i <- 1 to (size-1)*size + 1 by size) {
      nodes(i).addAdjacency(firstNode)
      firstNode.addAdjacency(nodes(i))
    }

    for (i <- size to size * size by size) {
      nodes(i).addAdjacency(lastNode)
      lastNode.addAdjacency(nodes(i))
    }
  }
  for(i <- 1 to size*size){
    if(isValid(i+1) && i % size != 0){
      nodes(i).addAdjacency(nodes(i+1))
    }
    if(isValid(i-1) && i % size != 1){
      nodes(i).addAdjacency(nodes(i-1))
    }
    if(isValid(i+size)){
      nodes(i).addAdjacency(nodes(i+size))
    }
    if(isValid(i-size)){
      nodes(i).addAdjacency(nodes(i-size))
    }
    if(isValid(i+size+1) && i % size != 0){
      nodes(i).addAdjacency(nodes(i+size+1))
    }
    if(isValid(i-size-1) && i % size != 1){
      nodes(i).addAdjacency(nodes(i-size-1))
    }
  }
  def isValid(i : Int): Boolean = i <= size*size && i >= 1

}
