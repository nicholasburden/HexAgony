package HSearch
import hexagony._
class HSearchTest extends Const{
  def foo = {
    val mod = new Model(5)
    //mod.playMove(new Cell(0,0), R)
    //mod.playMove(new Cell(1,1), B)
    mod.playMove(new Cell(2,2), B)

    //mod.playMove(new Cell(1,2), R)
    //mod.playMove(new Cell(4,2), R)
    mod.playMove(new Cell(4,3), B)



    var hb = new HSearch(mod, B)
    var hr = new HSearch(mod, R)

    hb.initial
    hr.initial
    hr.search
    hb.search
    //hr = hr.makeMove(4,4, R)

    //h = h.makeMove(1,1,R)
    println(hb.getStrongCarriers(hb.model.board(4)(3), hb.model.board(3)(2), true))
    println(hb.getWeakCarriers(hb.model.board(1)(1), HSearch.boundaryBlue1, true))





  }


}
