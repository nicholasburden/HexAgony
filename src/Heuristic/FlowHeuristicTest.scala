package Heuristic

import hexagony._
class FlowHeuristicTest extends Const {

  def foo = {
    val fh = new FlowHeuristic
    val model = new Model(7)
    println(fh.evaluate(model, R))
  }
}
