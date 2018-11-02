package moveordering

import hexagony._

class MoveOrderingTest extends Const {
   def test = {
     val mod : Model = new Model(4)
     val mod2 = mod.copy()
     //mod2.playMove(new Cell(1,1), R)
     mod2.playMove(new Cell(2,3), R)

   }
}
