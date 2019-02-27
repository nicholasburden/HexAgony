package experiments

import filewriting.FileWrite
import hexagony._
import hsearch.HSearch

import scala.util.Random

//import scala.util.Random

class PerformanceTests extends Const {
  def test() = {
    //File kept in the following format: BOT1_BOT2_TIME_SIZE

    var robotFactory1 = new RobotFactory("HSEARCH")
    var robotFactory2 = new RobotFactory("FLOW")
    var robotFactory3 = new RobotFactory("MONTECARLO")
    var robotFactorySimple = new RobotFactory("MONTECARLOSIMPLE")
    var size = 4

    val writer1 = new FileWrite("Experiments/XHSEARCHDEPTH_PERFORMANCE_INF_" + size + ".txt")
    val writer2 = new FileWrite("Experiments/XHSEARCHTIME_PERFORMANCE_INF_" + size + ".txt")
    val writer3 = new FileWrite("Experiments/XHSEARCHM_PERFORMANCE_INF_" + size + ".txt")

    val writer4 = new FileWrite("Experiments/XHSEARCHK_PERFORMANCE_INF_" + size + ".txt")
    val writer5 = new FileWrite("Experiments/XFLOWDEPTH_PERFORMANCE_INF_" + size + ".txt")
    val writer6 = new FileWrite("Experiments/X2MCTIME_PERFORMANCE_INF_" + size + ".txt")
    val writer7 = new FileWrite("Experiments/XMCWIN_PERFORMANCE_INF_" + size + ".txt")
    val writer8 = new FileWrite("Experiments/XMCKNOWLEDGE_PERFORMANCE_INF_" + size + ".txt")
    val writer9 = new FileWrite("Experiments/XMCHSEARCHTIME_PERFORMANCE_INF_" + size + ".txt")


    //RobotAlphaBetaResistance - depth
    //val hsearch = robotFactory1.makeRobot(new Model(size), 30000, true, R)
    //val mcSimple = robotFactorySimple.makeRobot(new Model(size), 30000, true, R)
    for(i <- 1 to 3){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory1, robotFactorySimple, 20000, false)
        val game2 = new Game(robotFactorySimple, robotFactory1, 20000, false)
        RobotAlphaBetaResistance.DEPTH = i
        val mod = getRandomBoard(3, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 1")
        writer1.writeToFile(res1)
        writer1.writeToFile(1-res2)
      }
    }
    writer1.close
    RobotAlphaBetaResistance.DEPTH = 2





    //M
    for(i <- 0 to 20 by 10){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory1, robotFactorySimple, 20000, false)
        val game2 = new Game(robotFactorySimple, robotFactory1, 20000, false)
        HSearch.M = i
        val mod = getRandomBoard(3, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 3")
        writer3.writeToFile(res1)
        writer3.writeToFile(1-res2)
      }
    }
    writer3.close
    HSearch.M = 14


    //K
    for(i <- 0 to 6 by 2){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory1, robotFactorySimple, 20000, false)
        val game2 = new Game(robotFactorySimple, robotFactory1, 20000, false)
        HSearch._K = i
        val mod = getRandomBoard(3, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 4")
        writer4.writeToFile(res1)
        writer4.writeToFile(1-res2)
      }
    }
    writer4.close
    HSearch._K = 4


    size = 6
    //RobotAlphaBetaFlow - DEPTH
    for(i <- 1 to 3){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory2, robotFactorySimple, 20000, false)
        val game2 = new Game(robotFactorySimple, robotFactory2, 20000, false)
        RobotAlphaBetaFlow.DEPTH = i
        val mod = getRandomBoard(3, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 5")
        writer5.writeToFile(res1)
        writer5.writeToFile(1-res2)
      }
    }
    writer5.close
    RobotAlphaBetaFlow.DEPTH = 2




    RobotMonteCarlo.MCTS_TIME = 10000

    //WIN_SCORE
    /*
    for(i <- 0 to 40 by 10){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory3, robotFactorySimple, 20000, false)
        val game2 = new Game(robotFactorySimple, robotFactory3, 20000, false)
        RobotMonteCarlo.WIN_SCORE = i
        val mod = getRandomBoard(3, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 7")
        writer7.writeToFile(res1)
        writer7.writeToFile(1-res2)
      }
    }
    writer7.close
    RobotMonteCarlo.WIN_SCORE = 10
    */
    //MonteCarlo - KNOWLEDGE_THRESHOLD

    for(i <- 0 to 60 by 20){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory3, robotFactorySimple, 20000, false)
        val game2 = new Game(robotFactorySimple, robotFactory3, 20000, false)
        montecarlo.State.KNOWLEDGE_THRESHOLD = i
        val mod = getRandomBoard(3, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 8")
        writer8.writeToFile(res1)
        writer8.writeToFile(1-res2)
      }
    }
    writer8.close
    montecarlo.State.KNOWLEDGE_THRESHOLD = 15


    //MonteCarlo - HSEARCH_TIME

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
