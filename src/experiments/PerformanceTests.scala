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
    var size = 7
    val writer1 = new FileWrite("Experiments/HSEARCHDEPTH_PERFORMANCE_INF_" + size + ".txt")
    val writer2 = new FileWrite("Experiments/HSEARCHTIME_PERFORMANCE_INF_" + size + ".txt")
    val writer3 = new FileWrite("Experiments/HSEARCHM_PERFORMANCE_INF_" + size + ".txt")
    val writer4 = new FileWrite("Experiments/HSEARCHK_PERFORMANCE_INF_" + size + ".txt")
    val writer5 = new FileWrite("Experiments/FLOWDEPTH_PERFORMANCE_INF_" + size + ".txt")
    val writer6 = new FileWrite("Experiments/MCTIME_PERFORMANCE_INF_" + size + ".txt")
    val writer7 = new FileWrite("Experiments/MCWIN_PERFORMANCE_INF_" + size + ".txt")
    val writer8 = new FileWrite("Experiments/MCKNOWLEDGE_PERFORMANCE_INF_" + size + ".txt")
    val writer9 = new FileWrite("Experiments/MCHSEARCHTIME_PERFORMANCE_INF_" + size + ".txt")


    //RobotAlphaBetaResistance - depth
    //val hsearch = robotFactory1.makeRobot(new Model(size), 30000, true, R)
    //val mcSimple = robotFactorySimple.makeRobot(new Model(size), 30000, true, R)
    for(i <- 1 to 3){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory1, robotFactorySimple, 999999, false)
        val game2 = new Game(robotFactorySimple, robotFactory1, 999999, false)
        RobotAlphaBetaResistance.DEPTH = i
        val mod = getRandomBoard(2, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 1")
        writer1.writeToFile(res1)
        writer1.writeToFile(1-res2)
      }
    }
    RobotAlphaBetaResistance.DEPTH = 2


    //TIME
    for(i <- 0 to 30000 by 5000){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory1, robotFactorySimple, 999999, false)
        val game2 = new Game(robotFactorySimple, robotFactory1, 999999, false)
        //RobotAlphaBetaResistance.TIME = i
        val mod = getRandomBoard(2, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 2")
        writer2.writeToFile(res1)
        writer2.writeToFile(1-res2)
      }
    }
    //RobotAlphaBetaResistance.TIME = 5000


    //M
    for(i <- 0 to 30 by 5){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory1, robotFactorySimple, 999999, false)
        val game2 = new Game(robotFactorySimple, robotFactory1, 999999, false)
        HSearch.M = i
        val mod = getRandomBoard(2, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 3")
        writer3.writeToFile(res1)
        writer3.writeToFile(1-res2)
      }
    }
    HSearch.M = 20


    //K
    for(i <- 0 to 8 by 2){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory1, robotFactorySimple, 999999, false)
        val game2 = new Game(robotFactorySimple, robotFactory1, 999999, false)
        HSearch._K = i
        val mod = getRandomBoard(2, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 4")
        writer4.writeToFile(res1)
        writer4.writeToFile(1-res2)
      }
    }
    HSearch._K = 4



    //RobotAlphaBetaFlow - DEPTH
    for(i <- 1 to 3){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory2, robotFactorySimple, 999999, false)
        val game2 = new Game(robotFactorySimple, robotFactory2, 999999, false)
        RobotAlphaBetaFlow.DEPTH = i
        val mod = getRandomBoard(2, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 5")
        writer5.writeToFile(res1)
        writer5.writeToFile(1-res2)
      }
    }
    RobotAlphaBetaFlow.DEPTH = 2



    //MonteCarlo - MCTS_TIME

    for(i <- 1 to 3){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory3, robotFactorySimple, 999999, false)
        val game2 = new Game(robotFactorySimple, robotFactory3, 999999, false)
        RobotMonteCarlo.MCTS_TIME = i
        val mod = getRandomBoard(2, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 6")
        writer6.writeToFile(res1)
        writer6.writeToFile(1-res2)
      }
    }
    RobotMonteCarlo.MCTS_TIME = 10000

    //WIN_SCORE

    for(i <- 1 to 3){
      for(j <- 0 to 50 by 10) {
        val game1 = new Game(robotFactory3, robotFactorySimple, 999999, false)
        val game2 = new Game(robotFactorySimple, robotFactory3, 999999, false)
        RobotMonteCarlo.WIN_SCORE = i
        val mod = getRandomBoard(2, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 7")
        writer7.writeToFile(res1)
        writer7.writeToFile(1-res2)
      }
    }
    RobotMonteCarlo.WIN_SCORE = 10

    //MonteCarlo - KNOWLEDGE_THRESHOLD

    for(i <- 0 to 60 by 20){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory3, robotFactorySimple, 999999, false)
        val game2 = new Game(robotFactorySimple, robotFactory3, 999999, false)
        montecarlo.State.KNOWLEDGE_THRESHOLD = i
        val mod = getRandomBoard(2, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 8")
        writer8.writeToFile(res1)
        writer8.writeToFile(1-res2)
      }
    }
    montecarlo.State.KNOWLEDGE_THRESHOLD = 15


    //MonteCarlo - HSEARCH_TIME

    for(i <- 0 to 5000 by 1000){
      for(j <- 1 to 5) {
        val game1 = new Game(robotFactory3, robotFactorySimple, 999999, false)
        val game2 = new Game(robotFactorySimple, robotFactory3, 999999, false)
        montecarlo.State.HSEARCH_TIME_LIMIT = i
        val mod = getRandomBoard(2, size)
        val res1 = game1.playGame(mod)
        val res2 = game2.playGame(mod)
        println("PT " + j + " played testing parameter 9")
        writer9.writeToFile(res1)
        writer9.writeToFile(1-res2)
      }
    }
    montecarlo.State.HSEARCH_TIME_LIMIT = 2000
    writer1.close
    writer2.close
    writer3.close
    writer4.close
    writer5.close
    writer6.close
    writer7.close
    writer8.close
    writer9.close
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
