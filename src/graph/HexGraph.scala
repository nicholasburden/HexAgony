package graph
import hexagony._
class HexGraph(size : Int, colour : Colour/*, pie : Boolean*/) extends Graph(size) with Const{
  val nodes : Array[Node] = Array.ofDim(size*size+2)
  val t : Node = new Node(0)
  val s : Node = new Node(size*size + 1)
  for(i <- 1 to size*size){
    nodes(i) = new Node(i)
  }
  nodes(0) = t
  nodes(size*size+1) = s
  for(i <- 1 to size*size){
    nodes(i).addAdjacencies(generateAdjacencies(i))
  }

  for(i <- 1 to size){
    t.addAdjacent(nodes(i))
  }
  for(i <- (size-1)*size+1 to size*size){
    nodes(i).addAdjacent(s)
  }
  val othercolour = colour match{
    case R => B
    case B => R
  }
  val col = true match{
    case true => /*other*/colour
    case false => colour
  }
  def generateAdjacencies(k : Int) : Set[Node] = {
    val x = ((k-1) % size) + 1
    val y = ((k-1) / size) + 1
    var set : Set[Node] = Set()
    for(i <- (x-1) to (x+1)){
      for(j <- (y-1) to (y+1)){
        if(col == B && isValid(i,j) && !(i == (x-1) && j == (y + 1)) && !(i == (x+1) && j == (y - 1)) && !(i == x && j == y)){
          set = set + nodes(((j-1)*size + i))
        }
        else if(col == R && isValid(i,j) && !(i == (x+1) && j == (y + 1)) && !(i == (x-1) && j == (y - 1)) && !(i == x && j == y)){
          set = set + nodes(((j-1)*size + i))
        }
      }
    }
    set
  }
  def isValid(x : Int, y : Int) : Boolean = (x >= 1 && x <= size && y >= 1 && y <= size) //not including s or t
  def getNodes : Array[Node] = nodes
  def placeYours(x : Int, y : Int) = {
    val k = ((x-1)*size+y)
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
    val k = ((x-1)*size+y)

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