package HSearch
import hexagony._
class HSearchTest extends Const{
  def foo = {
    val mod = new Model(11)

    val hsearchRed = new HSearch(mod, R)
    val hsearchBlue = new HSearch(mod, B)

    hsearchRed.search
    hsearchBlue.search
    println("Red:")
    for(cell <- hsearchBlue.getStrongCarriers(hsearchBlue.boundaryBlue1, mod.board(0)(1))){

      print(cell)

      println()
    }

  }


}
