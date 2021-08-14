package hexagony

import java.awt.Polygon

class Hexagon(val i: Int, val j: Int, xpoints: Array[Int], ypoints: Array[Int])
  extends Polygon(xpoints: Array[Int], ypoints: Array[Int], 6) with Const {
  
  var colour: Colour = O
  
  override def toString(): String = "(" + i + ", " + j + ")"
  
}