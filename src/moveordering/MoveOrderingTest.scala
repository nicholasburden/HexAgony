package moveordering

import hexagony._

class MoveOrderingTest extends Const {
   def test = {
     val mod : Model = new Model(4)

     mod.playMove(new Cell(2,3), R)
     mod.playMove(new Cell(3,3), R)
     val mo = new MoveOrdering
     mo.initial(mod)
     println(mo.getOrdering(mod))

   }
}
