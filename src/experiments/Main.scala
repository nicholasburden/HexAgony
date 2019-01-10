package experiments

import filewriting.FileWrite

object Main {
  def main(args : Array[String]) = {
    val robotFactory1 = new RobotFactory("RANDOM")
    val robotFactory2 = new RobotFactory("FLOW")
    val writer1 = new FileWrite("Experiments/RANDOM_FLOW_10_5.txt")
    val writer2 = new FileWrite("Experiments/FLOW_RANDOM_10_5.txt")
    for(i <- 1 to 500) {
      val game = new Game(robotFactory1, robotFactory2, 10000, false, 5)
      val res = game.playGame()
      writer1.writeToFile(res)
      println("Player " + res + " won that game.")
    }
    writer1.close
    for(i <- 1 to 500) {
      val game = new Game(robotFactory2, robotFactory1, 10000, false, 5)
      val res = game.playGame()
      writer2.writeToFile(res)
      println("Player " + res + " won that game.")
    }
    writer2.close
  }
}
