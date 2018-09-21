package hexagony

import java.awt.event.MouseEvent

import javax.swing.JOptionPane
import javax.swing.event.MouseInputAdapter

class Human(frame: Frame, val timelimit: Long = 0, colour: Colour)
  extends Player(colour: Colour) {
  
  var move: Cell = null
  var done = true
  
  var name = colour.name
  
  val clickevent = new MouseInputAdapter() {
    override def mouseClicked(event: MouseEvent) { click(event) }
  }
  
  private def click(event: MouseEvent) = synchronized {
    
    val point = event.getPoint
    for (row <- frame.grid.grid; hex <- row) {
      if (hex.contains(point) && hex.colour == O) {
        move = new Cell(hex.i, hex.j)
        done = true
        notifyAll()
      }
    }
    
  }
  
  def makeMove(): Cell = synchronized { 
    
    move = null
    done = false
    frame.board.addMouseListener(clickevent)
    while (!done) wait()
    frame.board.removeMouseListener(clickevent)
    move
    
  }
                
  def pieRule(firstmove: Cell): Boolean = JOptionPane.showConfirmDialog(
      frame.panel2, "Blue, would you like to swap?", "Pie Rule", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION

  
  def update(move: Move) {}
  
  def playPieRule() {}
        
}