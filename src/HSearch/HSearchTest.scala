package HSearch
import hexagony._
class HSearchTest extends Const{
  def foo = {
    val mod = new Model(3)
    mod.playMove(mod.board(1)(1), R)
    val hsearchRed = new HSearch(mod, R)
    hsearchRed.initial
    hsearchRed.search

    for(cell1 <- mod.myCells(R) ++ mod.myCells(O)){
      for(cell2 <- mod.myCells(R) ++ mod.myCells(O)){
        println(cell1 + " " + cell2 + hsearchRed.getStrongCarriers(cell1, cell2, true))
      }
    }
    val h = hsearchRed.makeMove(0,0, R)
    println(h.C((h.model.board(1)(1), h.model.board(0)(2))))




  }


}
