package tree
import scala.collection.mutable.ListBuffer
import montecarlo._

class Node(var state : State, var parent : Node, var childArray : ListBuffer[Node]) {
  def this(N : Int) = this(new State(N), null, new ListBuffer[Node]())
  def this(state : State) = this(state, null, new ListBuffer[Node]() )
  def this(t : (State, Node, ListBuffer[Node])) = this(t._1, t._2, t._3)
  def this(node : Node) = this(Node.init(node))


  def setParent(newParent : Node) = parent = newParent

  def getRandChild() = {
    val randomChild = (Math.random() * this.childArray.size).asInstanceOf[Int]
    this.childArray(randomChild)

  }
  def getMaxChild() : Node = {
    var max = -1
    var c : Node = null
    for(child <- childArray){
      val count = child.state.visit
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
    if(node.parent != null) p = node.parent
    val childArray : ListBuffer[Node] = node.childArray
    var newChildArray = new ListBuffer[Node]()
    for(child <- childArray){
      newChildArray += child
    }
    (new State(node.state), p, newChildArray)
  }
}