package experiments

object Main {
  def main(args : Array[String]) = {
    val robotFactory1 = new RobotFactory("HSEARCH")
    val robotFactory2 = new RobotFactory("FLOW")

    val game = new Game(robotFactory1, robotFactory2, 5000, false, 5)
    println(game.playGame())
  }
}
