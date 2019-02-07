package experiments

import hexagony._

class RobotFactory(robotType : String) {
  def makeRobot(model: Model, timelimit: Long, pierule: Boolean, colour: Colour) : Robot = {
    if(robotType.equals("HSEARCH")) return new RobotAlphaBetaResistance(model, timelimit, pierule, colour)
    if(robotType.equals("FLOW")) return new RobotAlphaBetaFlow(model, timelimit, pierule, colour)
    if(robotType.equals("MONTECARLO")) return new RobotMonteCarlo(model, timelimit, pierule, colour)
    if(robotType.equals("MONTECARLOSIMPLE")) return new RobotMonteCarlo(model, timelimit, pierule, colour)
    if(robotType.equals("RANDOM")) return new RobotRandom(model, timelimit, pierule, colour)
    return null
  }
}
