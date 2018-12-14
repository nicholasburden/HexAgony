package heuristic

import hexagony._
class ResistanceHeuristicTest extends Const {

  def foo = {
    val fh = new ResistanceHeuristic

    val model = new Model(3)

    model.playMove(new Cell(1,1), R)
    model.playMove(new Cell(2,2), R)
    model.playMove(new Cell(0,0), B)






  }
}
