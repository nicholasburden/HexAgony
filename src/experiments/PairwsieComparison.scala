package experiments

import filewriting.FileWrite
import hexagony.{Colour, Model}

import scala.util.Random

class PairwsieComparison {
  def test() = {
    //File kept in the following format: BOT1_BOT2_TIME_SIZE

    var robotFactory1 = new RobotFactory("HSEARCH")
    var robotFactory2 = new RobotFactory("FLOW")
    for (size <- 5 to 11) {
      val writer1 = new FileWrite("Experiments/HSEARCH_FLOW_30_" + size + ".txt")
      //val writer2 = new FileWrite("Experiments/TEST2_10_5.txt")
      for (i <- 1 to 30) {
        val mod = getRandomBoard(2, size)
        val game1 = new Game(robotFactory1, robotFactory2, 30000, false)
        val game2 = new Game(robotFactory2, robotFactory1, 30000, false)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        writer1.writeToFile(res1)
        writer1.writeToFile(1-res2)
        println("HSEARCH V FLOW " + i + " played on board size " + size + ".")
      }
      writer1.close
    }
    robotFactory1 = new RobotFactory("FLOW")
    robotFactory2 = new RobotFactory("MONTECARLO")
    for (size <- 5 to 11) {
      val writer1 = new FileWrite("Experiments/FLOW_MONTECARLO_30_" + size + ".txt")
      //val writer2 = new FileWrite("Experiments/TEST2_10_5.txt")
      for (i <- 1 to 30) {
        val mod = getRandomBoard(2, size)
        val game1 = new Game(robotFactory1, robotFactory2, 30000, false)
        val game2 = new Game(robotFactory2, robotFactory1, 30000, false)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        writer1.writeToFile(res1)
        writer1.writeToFile(1-res2)
        println("FLOW V MONTECARLO " + i + " played on board size " + size + ".")
      }
      writer1.close
    }
    robotFactory1 = new RobotFactory("MONTECARLO")
    robotFactory2 = new RobotFactory("HSEARCH")
    for (size <- 5 to 11) {
      val writer1 = new FileWrite("Experiments/MONTECARLO_HSEARCH_30_" + size + ".txt")
      //val writer2 = new FileWrite("Experiments/TEST2_10_5.txt")
      for (i <- 1 to 30) {
        val mod = getRandomBoard(2, size)
        val game1 = new Game(robotFactory1, robotFactory2, 30000, false)
        val game2 = new Game(robotFactory2, robotFactory1, 30000, false)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        writer1.writeToFile(res1)
        writer1.writeToFile(1-res2)
        println("MONTECARLO V HSEARCH " + i + " played on board size " + size + ".")
      }
      writer1.close
    }
  }


  def getRandomBoard(numOfPiecesEach : Int, N : Int) : Model = {
    val mod = new Model(N)
    val rnd = new Random()
    for(i <- 1 to numOfPiecesEach){
      var x = rnd.nextInt(N)
      var y = rnd.nextInt(N)
      while(!mod.board(x)(y).colour.equals(new Colour(0))){
        x = rnd.nextInt(N)
        y = rnd.nextInt(N)
      }
      mod.playMove(mod.board(x)(y), new Colour(1))
      mod.playMove(mod.board(x)(y), new Colour(2))
    }
    mod
  }
}
