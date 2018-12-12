package tree

class Tree(var root : Node) {
  def this(N : Int) = this(new Node(N))
  //def getRoot = root
  def setRoot (newRoot : Node) = root = newRoot
  def addChild(parent : Node, child : Node) = parent.childArray += child
}
