package montecarlo

import tree.Node

object UCT {
  def uctValue(totalVisit : Int, nodeWinScore : Double, nodeVisit : Int) : Double = {
    if(nodeVisit == 0){
      return Integer.MAX_VALUE
    }
    (nodeWinScore / nodeVisit.asInstanceOf[Double]) + 1.41 * Math.sqrt(Math.log(totalVisit) / nodeVisit.asInstanceOf[Double])
  }
  def findBestNodeWithUCT(node : Node) = {
    val parentVisit = node.getState.getVisitCount
    var max : Double = -1
    var n : Node = null
    for(i <- 0 until node.getChildArray.size){
      val c = node.getChildArray(i)
      val v = uctValue(parentVisit, c.getState.getWinScore, c.getState.getVisitCount)
      if(v > max){
        max = v; n = c
      }
    }
    n
  }
}
