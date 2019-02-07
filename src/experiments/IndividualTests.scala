package experiments

import filewriting.FileWrite
import hexagony.{Colour, Model}

import scala.util.Random

class IndividualTests {
  def test() = {
    //File kept in the following format: BOT1_BOT2_TIME_SIZE

    var robotFactory1 = new RobotFactory("HSEARCH")
    var robotFactory2 = new RobotFactory("RANDOM")
    for (size <- 5 to 11) {
      val writer1 = new FileWrite("Experiments/HSEARCH_RANDOM_30_" + size + ".txt")
      //val writer2 = new FileWrite("Experiments/TEST2_10_5.txt")
      for (i <- 1 to 30) {
        val mod = new Model(size)
        val game1 = new Game(robotFactory1, robotFactory2, 30000, true)
        val game2 = new Game(robotFactory2, robotFactory1, 30000, true)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        writer1.writeToFile(res1)
        writer1.writeToFile(1-res2)
        println("Random game " + i + " played on board size " + size + " between HSEARCH and RANDOM.")
      }
      writer1.close
    }
    robotFactory1 = new RobotFactory("FLOW")
    robotFactory2 = new RobotFactory("RANDOM")
    for (size <- 5 to 11) {
      val writer1 = new FileWrite("Experiments/FLOW_RANDOM_30_" + size + ".txt")
      //val writer2 = new FileWrite("Experiments/TEST2_10_5.txt")
      for (i <- 1 to 30) {
        val mod = new Model(size)
        val game1 = new Game(robotFactory1, robotFactory2, 30000, true)
        val game2 = new Game(robotFactory2, robotFactory1, 30000, true)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        writer1.writeToFile(res1)
        writer1.writeToFile(1-res2)
        println("Random game " + i + " played on board size " + size + " between FLOW and RANDOM.")
      }
      writer1.close
    }
    robotFactory1 = new RobotFactory("MONTECARLO")
    robotFactory2 = new RobotFactory("RANDOM")
    for (size <- 5 to 11) {
      val writer1 = new FileWrite("Experiments/MONTECARLO_RANDOM_30_" + size + ".txt")
      //val writer2 = new FileWrite("Experiments/TEST2_10_5.txt")
      for (i <- 1 to 30) {
        val mod = new Model(size)
        val game1 = new Game(robotFactory1, robotFactory2, 30000, true)
        val game2 = new Game(robotFactory2, robotFactory1, 30000, true)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        writer1.writeToFile(res1)
        writer1.writeToFile(1-res2)
        println("Random game " + i + " played on board size " + size + " between MONTECARLO and RANDOM.")
      }
      writer1.close
    }
  }


}
