package hexagony

import java.io.File
import java.io.FileReader
import java.io.FileWriter

import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError

import reflect.runtime.universe.runtimeMirror

class Compiler {

  def getRobot(file: File, model: Model, timelimit: Long, pierule: Boolean, colour: Colour): Either[String, Robot] = {
    
    try {      
      
      if (file == null) return Left("No robot selected")
      if (timelimit < 500) return Left("Time limit must be at least 0.5s")
      
      val name = file.getName.substring(0, file.getName.lastIndexOf('.'))
      var text = ("import hexagony._\n\n" +
                  "new RobotMaker {\n\n" +
                  "override def robot(model: Model, timelimit: Long, pierule: Boolean, colour: Colour): Robot = " +
                    "new %s(model, timelimit, pierule, colour)\n\n".format(name)).toCharArray()      
      
      new File("src/makers").createNewFile() // ensure the directory exists             
      val dest = new File("src/makers/%sMaker.hex".format(name))
      val reader = new FileReader(file)
      val writer = new FileWriter(dest)
      var ch = reader.read()
      while (ch >= 0) { text :+= ch.toChar; ch = reader.read() }
      reader.close()
      text :+= '\n'; text :+= '}'
      writer.write(text)
      writer.flush(); writer.close()
      
      val code = scala.io.Source.fromFile(dest).mkString("")
      val toolbox = runtimeMirror(getClass.getClassLoader).mkToolBox() 
      val tree = toolbox.parse(code)
      val robotmaker = toolbox.eval(tree).asInstanceOf[RobotMaker]
      val robot = robotmaker.robot(model, timelimit, pierule, colour)   
      robot.name = name   
      Right(robot)
      
    } catch {
      case err: ToolBoxError => println(err.getMessage + '\n'); Left("Error loading robot: see console")
      case ex: Exception => println(ex.getMessage + '\n'); Left("Error loading robot: see console")
    }    
    
  }
  
}