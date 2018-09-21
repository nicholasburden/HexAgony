package hexagony

import java.io.File

import javax.swing.filechooser.FileFilter
 
class ScalaFileFilter extends FileFilter {
 
  def accept(file: File): Boolean = file.isDirectory || getSplitName(file)._2 == "scala"
  
  def getSplitName(file: File): (String, String) = {
    val name = file.getName
    val idx = name.lastIndexOf('.')
    if (idx > 0 && idx < name.length - 1) (name.substring(0, idx), name.substring(idx + 1).toLowerCase)
    else (name, "")
  }
 
  def getDescription: String = "Scala files"
  
}