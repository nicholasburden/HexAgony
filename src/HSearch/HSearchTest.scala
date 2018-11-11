package HSearch
import hexagony._
class HSearchTest extends Const{
  def foo = {
    val mod = new Model(5)
    mod.playMove(new Cell(2,2), R)
    mod.playMove(new Cell(3,3), B)
    mod.playMove(new Cell(4,4), R)
    mod.playMove(new Cell(4,3), B)
    mod.playMove(new Cell(2,3), R)
    mod.playMove(new Cell(2,2), B)

    val hsearch = new HSearch(mod, B)
    hsearch.initial
    hsearch.search
    val h = hsearch.makeMove(0,1,R)
    println(h.getStrongCarriers(h.model.board(2)(2), h.model.board(0)(1), true))
    println(h.getStrongCarriers(h.model.board(1)(2), HSearch.boundaryBlue1, true))





  }


}
