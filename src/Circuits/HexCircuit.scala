package Circuits
import hexagony._
class HexCircuit(size :Int, colour : Colour) extends Circuit with Const{
  var nodes : List[Node] = List()
  val firstNode = new Node(0)
  val lastNode = new Node(size*size + 1)
  nodes = lastNode :: nodes
  for(i <- size*size to 1 by -1){
    nodes = new Node(i) :: nodes
  }
  nodes = firstNode :: nodes
  if(colour == B) {
    for (i <- 1 to size) {
      nodes(i).addAdjacencies(Set(firstNode))
    }
    firstNode.addAdjacencies(nodes.slice(1, size + 1).toSet)
    for (i <- (size - 1) * size + 1 to size * size) {
      nodes(i).addAdjacencies(Set(lastNode))
    }
    lastNode.addAdjacencies(nodes.slice((size - 1) * size + 1, size * size + 1).toSet)
  }
  else{
    for (i <- 1 to (size-1)*size + 1 by size) {
      nodes(i).addAdjacencies(Set(firstNode))
      firstNode.addAdjacencies(Set(nodes(i)))
    }

    for (i <- size to size * size by size) {
      nodes(i).addAdjacencies(Set(lastNode))
      lastNode.addAdjacencies(Set(nodes(i)))
    }
  }
  for(i <- 1 to size*size){
    if(isValid(i+1) && i % size != 0){
      nodes(i).addAdjacencies(Set(nodes(i+1)))
    }
    if(isValid(i-1) && i % size != 1){
      nodes(i).addAdjacencies(Set(nodes(i-1)))
    }
    if(isValid(i+size)){
      nodes(i).addAdjacencies(Set(nodes(i+size)))
    }
    if(isValid(i-size)){
      nodes(i).addAdjacencies(Set(nodes(i-size)))
    }
    if(isValid(i+size+1) && i % size != 0){
      nodes(i).addAdjacencies(Set(nodes(i+size+1)))
    }
    if(isValid(i-size-1) && i % size != 1){
      nodes(i).addAdjacencies(Set(nodes(i-size-1)))
    }
  }
  def isValid(i : Int): Boolean = i <= size*size && i >= 1

}
