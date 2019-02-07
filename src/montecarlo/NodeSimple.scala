package montecarlo

import scala.collection.mutable.ListBuffer
import montecarlo._

class NodeSimple(var state: StateSimple, var parent: NodeSimple, var childArray: ListBuffer[NodeSimple]) {
  def this() = this(new StateSimple(), null, new ListBuffer[NodeSimple]())

  def this(state: StateSimple) = this(state, null, new ListBuffer[NodeSimple]())

  def this(t: (StateSimple, NodeSimple, ListBuffer[NodeSimple])) = this(t._1, t._2, t._3)

  def this(node: NodeSimple) = this(NodeSimple.init(node))

  def setState(newState: StateSimple) = state = newState

  def setParent(newParent: NodeSimple) = parent = newParent

  def setChildArray(newChildArray: ListBuffer[NodeSimple]) = childArray = newChildArray

  def getRandomChildNode() = {
    val noOfPossibleMoves = this.childArray.size
    val selectRandom = (Math.random() * noOfPossibleMoves).asInstanceOf[Int]
    this.childArray(selectRandom)

  }

  def getChildWithMaxScore(): NodeSimple = {
    //Returns the child with the highest number of visits
    var max = -1
    var c: NodeSimple = null
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

object NodeSimple {
  def init(node: NodeSimple): (StateSimple, NodeSimple, ListBuffer[NodeSimple]) = {
    var p: NodeSimple = null
    if (node.parent != null) p = node.parent
    val childArray: ListBuffer[NodeSimple] = node.childArray
    var newChildArray = new ListBuffer[NodeSimple]()
    for (child <- childArray) {
      newChildArray += child
    }
    (new StateSimple(node.state), p, newChildArray)
  }
}