package filewriting
import java.io._
class FileWrite(fileName : String) {
  val file = new File(fileName)
  val bw = new BufferedWriter(new FileWriter(file, true))


  def writeToFile(s : String): Unit ={
    bw.write(s)

  }
  def writeToFile(x : Int): Unit ={
    bw.write(Integer.toString(x))

  }
  def close = {
    bw.close()
  }
}
