package tree

class Tree(var root : Node) {
  def this() = this(new Node())
  def getRoot = root
  def setRoot (newRoot : Node) = root = newRoot
  def addChild(parent : Node, child : Node) = parent.getChildArray += child
}
