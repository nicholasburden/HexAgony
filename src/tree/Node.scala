package tree
import scala.collection.mutable.ListBuffer
import montecarlo._

class Node(var state : State, var parent : Node, var childArray : ListBuffer[Node]) {
  def this() = this(new State(), null, new ListBuffer[Node]())
  def this(state : State) = this(state, null, new ListBuffer[Node]() )
  def this(t : (State, Node, ListBuffer[Node])) = this(t._1, t._2, t._3)
  def this(node : Node) = this(Node.init(node))
  def getState()= state
  def setState(newState : State) = state = newState
  def getParent() = parent
  def setParent(newParent : Node) = parent = newParent
  def getChildArray = childArray
  def setChildArray(newChildArray : ListBuffer[Node]) = childArray = newChildArray
  def getRandomChildNode() = {
    val noOfPossibleMoves = this.childArray.size
    val selectRandom = (Math.random() * noOfPossibleMoves).asInstanceOf[Int]
    this.childArray(selectRandom)

  }
  def getChildWithMaxScore() : Node = {
    var max = -1
    var c : Node = null
    for(child <- childArray){
      val count = child.getState.getVisitCount
      if(count > max){
        c = child; max = count
      }
    }
    c
  }
}
object Node{
  def init(node : Node): (State, Node, ListBuffer[Node]) ={
    var p : Node = null
    if(node.getParent() != null) p = node.getParent()
    val childArray : ListBuffer[Node] = node.getChildArray
    var newChildArray = new ListBuffer[Node]()
    for(child <- childArray){
      newChildArray += child
    }
    (new State(node.getState()), p, newChildArray)
  }
}