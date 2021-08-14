package hexagony

import javax.swing.JFileChooser
import java.io.File

class ScalaFileChooser extends JFileChooser {
  
  val filter = new ScalaFileFilter
  this.addChoosableFileFilter(filter)
  this.setAcceptAllFileFilterUsed(false)
  
  def name(file: File): String = filter.getSplitName(file)._1
  
}