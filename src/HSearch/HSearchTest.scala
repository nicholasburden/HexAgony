package HSearch
import hexagony._
class HSearchTest extends Const{
  def foo = {
    val mod = new Model(3)
    //mod.playMove(new Cell(1,0), R)
    //mod.playMove(new Cell(0,1), R)
    //mod.playMove(new Cell(0,2), R)
    //mod.playMove(new Cell(2,0), B)
    //mod.playMove(new Cell(2,1), B)
    //mod.playMove(new Cell(1,2), B)
    val hsearchRed = new HSearch(mod, R, 5)
    val hsearchBlue = new HSearch(mod, B, 5)

    hsearchRed.search
    hsearchBlue.search
    println("Red:")
    for(cell <- hsearchBlue.getStrongCarriers(mod.board(1)(1), hsearchBlue.boundaryBlue2)){

      print(cell)

      println()
    }

  }


}
