package experiments
import hexagony._
class Game(robotFac1 : RobotFactory, robotFac2 : RobotFactory, timelimit : Long, pierule : Boolean, N : Int) extends Const {
  def playGame() : Int = {
    try {
      val model = new Model(N)
      var robot1 = robotFac1.makeRobot(model, timelimit, pierule, R)
      val firstMove = robot1.makeMove()
      model.playMove(firstMove, R)
      var robot2 = robotFac2.makeRobot(model, timelimit, pierule, B)
      val pie = robot2.pieRule(model.myCells(R)(0))

      var moveCount = 1
      if (pierule && pie) {
        model.playPieRule(firstMove)
      }
      else {
        model.playMove(robot2.makeMove(), B)
        moveCount += 1
      }


      while (moveCount < N || !model.solution()) {
        robot1 = robotFac1.makeRobot(model, timelimit, pierule, R)
        //println(model.myCells(O))
        val move = robot1.makeMove()
        //println("H")
        model.playMove(move, R)
        if(moveCount < N || !model.solution()) {
          robot2 = robotFac2.makeRobot(model, timelimit, pierule, B)
          model.playMove(robot2.makeMove(), B)
          moveCount += 1
        }
      }
      if (model.solution(R)) return 0
      if (model.solution(B)) return 1
      return -1 //should not happen
    }
    catch {
      case e: Exception => e.printStackTrace(); 1
    }
  }
}
