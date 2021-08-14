package montecarlo

import scala.collection.mutable.ListBuffer
import montecarlo._

class Node(var state: State, var parent: Node, var childArray: ListBuffer[Node]) {
  def this() = this(new State(), null, new ListBuffer[Node]())

  def this(state: State) = this(state, null, new ListBuffer[Node]())

  def this(t: (State, Node, ListBuffer[Node])) = this(t._1, t._2, t._3)

  def this(node: Node) = this(Node.init(node))

  def setState(newState: State) = state = newState

  def setParent(newParent: Node) = parent = newParent

  def setChildArray(newChildArray: ListBuffer[Node]) = childArray = newChildArray

  def getRandomChildNode() = {
    val noOfPossibleMoves = this.childArray.size
    val selectRandom = (Math.random() * noOfPossibleMoves).asInstanceOf[Int]
    this.childArray(selectRandom)

  }

  def getChildWithMaxScore(): Node = {
    //Returns the child with the highest number of visits
    var max = -1
    var c: Node = null
    for (child <- childArray) {
      val count = child.state.visits
      if (count > max) {
        c = child;
        max = count
      }
    }
    c
  }
}

object Node {
  def init(node: Node): (State, Node, ListBuffer[Node]) = {
    var p: Node = null
    if (node.parent != null) p = node.parent
    val childArray: ListBuffer[Node] = node.childArray
    var newChildArray = new ListBuffer[Node]()
    for (child <- childArray) {
      newChildArray += child
    }
    (new State(node.state), p, newChildArray)
  }
}