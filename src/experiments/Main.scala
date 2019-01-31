package experiments


import filewriting.FileWrite
import hexagony.{Colour, Model}

import scala.util.Random

object Main{
  def main(args : Array[String]) = {
    //File kept in the following format: BOT1_BOT2_TIME_SIZE

    val robotFactory1 = new RobotFactory("HSEARCH")
    val robotFactory2 = new RobotFactory("FLOW")
    for (size <- 4 to 11) {
      val writer1 = new FileWrite("Experiments/HSEARCH_FLOW_30_" + size + ".txt")
      //val writer2 = new FileWrite("Experiments/TEST2_10_5.txt")
      for (i <- 1 to 30) {
        val mod = getRandomBoard(2, 5)
        val game1 = new Game(robotFactory1, robotFactory2, 30000, false)
        val game2 = new Game(robotFactory2, robotFactory1, 30000, false)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        writer1.writeToFile(res1)
        writer1.writeToFile(res2)
        println("Player " + res1 + " won the first game, " + res2 + " won the second.")
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
