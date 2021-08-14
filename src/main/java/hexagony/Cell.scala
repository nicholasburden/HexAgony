package hexagony

class Cell(val i: Int, val j: Int) extends Const {
  
  var colour: Colour = O
  override def equals(that: Any): Boolean =
      that match{
        case that: Cell => i == that.i && j == that.j
        case _ => false
      }
  override def hashCode: Int = i + j
  override def toString(): String = if (this == null) "null" else "(" + i + ", " + j + ")"
  
}