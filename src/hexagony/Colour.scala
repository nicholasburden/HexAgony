package hexagony

class Colour(val n: Int) {
  
  val name: String = n match {
    case 0 => "Open"
    case 1 => "Red"
    case 2 => "Blue"
    case 3 => "Black"
    case 4 => "RedPath"
    case 5 => "BluePath"
    case _ => ""
  }
  
  import java.awt.Color
  val colour: Color = n match {
    case 0 => Color.WHITE
    case 1 => Color.RED
    case 2 => Color.BLUE
    case 3 => Color.BLACK
    case 4 => new Color(255, 100, 200)
    case 5 => new Color(0, 150, 255)
    case _ => null
  }
  
  val othercolour: String = n match {
    case 1 => "Blue"
    case 2 => "Red"
    case _ => null
  }
  
  override def equals(that: Any) = that match {
    case that: Colour => n == that.n
    case _ => false
  }
  
  override def hashCode(): Int = 31 + n
  
  override def toString(): String = name
  
}