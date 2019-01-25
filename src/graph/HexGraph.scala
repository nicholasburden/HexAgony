package graph
import hexagony._
class HexGraph(size : Int, colour : Colour) extends Graph(size) with Const{

  var nodes: Array[Node] = Array.ofDim(size * size + 2)
  var t : Node = null
  var s : Node = null
  val othercolour = colour match {
    case R => B
    case B => R
  }
  val col = true match {
    case true => /*other*/ colour
    case false => colour
  }

  def initialise(model : Model) {

    s = new Node(0)
    t = new Node(size * size + 1)
    for (i <- 1 to size * size) {
      nodes(i) = new Node(i)
    }
    nodes(0) = s
    nodes(size * size + 1) = t
    for (i <- 1 to size * size) {
      nodes(i).addAdjacencies(generateAdjacencies(i))
    }
    if(colour.equals(B)){
      for (i <- 1 to size) {
        s.addAdjacent(nodes(i))
      }
      for (i <- (size - 1) * size + 1 to size * size) {
        nodes(i).addAdjacent(t)
      }
    }
    else{
      for (i <- 1 to (size-1)*size + 1 by size) {
        s.addAdjacent(nodes(i))
      }
      for (i <- size to size * size by size) {
        nodes(i).addAdjacent(t)
      }
    }

    for(i <- model.board.indices){
      for(j <- model.board(0).indices){
        if(colour == B){
          if(model.board(i)(j).colour == B){
            placeYours(i, j)
          }
          else if(model.board(i)(j).colour == R){

            placeTheirs(i, j)
          }
        }
        else{
          if(model.board(i)(j).colour == R){
            placeYours(i, j)
          }
          else if(model.board(i)(j).colour == B){
            placeTheirs(i, j)
          }
        }

      }
    }
  }

  override def clone(): HexGraph = {
    val newGraph = new HexGraph(size, colour)
    newGraph.nodes = Array.ofDim[Node](size*size+2)
    newGraph.s = new Node(0)
    newGraph.t = new Node(size * size + 1)
    /*
    for(elem <- t.getAdjacencies){
      newGraph.t.adjacencies += newGraph.nodes(elem.id)
    }
    for(elem <- s.getAdjacencies){
      newGraph.s.adjacencies += elem
    }
    */
    for(node <- nodes){
      val temp = new Node(node.id)
      newGraph.nodes(temp.id) = temp
    }
    for(node <- nodes){
      for(elem <- node.adjacencies)
      newGraph.nodes(node.id).addAdjacent(newGraph.nodes(elem.id))
    }

    newGraph

  }
  def generateAdjacencies(k : Int) : Set[Node] = {
    val x = ((k-1) % size) + 1
    val y = ((k-1) / size) + 1
    var set : Set[Node] = Set()
    for(i <- (x-1) to (x+1)){
      for(j <- (y-1) to (y+1)){
        if(isValid(i,j) && !(i == (x-1) && j == (y + 1)) && !(i == (x+1) && j == (y - 1)) && !(i == x && j == y)){
          set = set + nodes(((j-1)*size + i))
        }
      }
    }
    set
  }
  def isValid(x : Int, y : Int) : Boolean = (x >= 1 && x <= size && y >= 1 && y <= size) //not including s or t
  def getNodes : Array[Node] = nodes
  def placeYours(x : Int, y : Int) = {
    val k = x*size+y+1
    //handle the special case:
    if(s.getAdjacencies.contains(nodes(k))){
      for(node <- nodes(k).getAdjacencies){
        s.addAdjacent(node)
      }
    }
    for(node1 <- nodes(k).getAdjacencies){
      for(node2 <- nodes(k).getAdjacencies){
        if(node1.id != node2.id){
          if(node1.id != t.id){
            //avoid setting backwards edge from t
            node1.addAdjacent(node2)
          }

          if(node2.id != t.id){
            //avoid setting backwards edge from t
            node2.addAdjacent(node1)
          }
        }
      }
    }
    for(node <- nodes(k).getAdjacencies){
      node.removeAdjacent(nodes(k))
      nodes(k).removeAdjacent(node)
    }

  }
  def placeTheirs(x : Int, y : Int) = {

    val k = x*size+y+1

    //handle special case:
    if (s.getAdjacencies.contains(nodes(k))){
      s.removeAdjacent(nodes(k))
    }
    for(node <- nodes(k).getAdjacencies){
      node.removeAdjacent(nodes(k))
      nodes(k).removeAdjacent(node)
    }
  }
}
