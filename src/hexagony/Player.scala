package hexagony

import scala.concurrent.duration._

abstract class Player(val colour: Colour) extends Const {
  
  val timelimit: Long

  var name: String

  def update(move: Move)
        
  def makeMove(): Cell
                
  def pieRule(firstmove: Cell): Boolean
  
  def playPieRule()
        
}
