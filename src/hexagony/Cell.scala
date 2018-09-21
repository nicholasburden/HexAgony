package hexagony

class Cell(val i: Int, val j: Int) extends Const {
  
  var colour: Colour = O
  def equals(that: Cell): Boolean = (i == that.i && j == that.j)
  override def toString(): String = if (this == null) "null" else "(" + i + ", " + j + ")"
  
}