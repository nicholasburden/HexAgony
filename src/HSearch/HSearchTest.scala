package HSearch
import hexagony._
class HSearchTest extends Const{
  def foo = {
    val mod = new Model(3)

    val hsearchRed = new HSearch(mod, R)
    hsearchRed.initial
    hsearchRed.search
    println(hsearchRed.getStrongCarriers(mod.board(1)(1), HSearch.boundaryRed1, false))
    println(hsearchRed.getStrongCarriers(mod.board(1)(1), HSearch.boundaryRed2, false))
    //for(cell1 <- mod.myCells(R) ++ mod.myCells(O)){
      //for(cell2 <- mod.myCells(R) ++ mod.myCells(O)){
        //println(cell1 + " " + cell2 + hsearchRed.getStrongCarriers(cell1, cell2, true))
      //}
    //}

    val h = hsearchRed.makeMove(1,1, R)

    println(h.getWeakCarriers(h.model.board(1)(1), HSearch.boundaryRed2, false))
    println(h.getStrongCarriers(h.model.board(1)(1), HSearch.boundaryRed2, false))



  }


}
