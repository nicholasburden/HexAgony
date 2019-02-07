package experiments

import filewriting.FileWrite
import hexagony._
import hsearch.HSearch

//import scala.util.Random

class TimeForParameter extends Const {
  def test() = {
    //File kept in the following format: BOT1_BOT2_TIME_SIZE

    var robotFactory1 = new RobotFactory("HSEARCH")
    var robotFactory2 = new RobotFactory("FLOW")
    var robotFactory3 = new RobotFactory("MONTECARLO")
    //var robotFactorySimple = new RobotFactory("MONTECARLO")
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
    val hsearch = robotFactory1.makeRobot(new Model(size), 30000, true, R)
    //val mcSimple = robotFactorySimple.makeRobot(new Model(size), 30000, true, R)
    for(i <- 1 to 3){
      RobotAlphaBetaResistance.DEPTH = i
      val start = System.currentTimeMillis()
      val move = hsearch.makeMove()
      val end = System.currentTimeMillis()
      writer1.writeToFile((end-start).toString() + "\n")
    }
    //-time
    RobotAlphaBetaResistance.DEPTH = 2
    val hsearch2 = robotFactory1.makeRobot(new Model(size), 999999, true, R).asInstanceOf[RobotAlphaBetaResistance]
    for(time <- 0 to 30000 by 5000){
      RobotAlphaBetaResistance.TIME = time
      val start = System.currentTimeMillis()
      val move = hsearch2.makeMove()
      val end = System.currentTimeMillis()
      writer2.writeToFile((end-start).toString() + "\n")
    }
    RobotAlphaBetaResistance.TIME = 5000
    //-M
    val hsearch3 = robotFactory1.makeRobot(new Model(size), 999999, true, R).asInstanceOf[RobotAlphaBetaResistance]
    for(m <- 0 to 30 by 5){
      HSearch.M = m
      val start = System.currentTimeMillis()
      val move = hsearch3.makeMove()
      val end = System.currentTimeMillis()
      writer3.writeToFile((end-start).toString() + "\n")
    }
    HSearch.M = 20
    //-K
    val hsearch4 = robotFactory1.makeRobot(new Model(size), 999999, true, R).asInstanceOf[RobotAlphaBetaResistance]
    for(k <- 0 to 8 by 2){
      HSearch._K = k
      val start = System.currentTimeMillis()
      val move = hsearch4.makeMove()
      val end = System.currentTimeMillis()
      writer4.writeToFile((end-start).toString() + "\n")
    }
    HSearch._K = 4
    //RobotAlphaBetaFlow - depth
    val flow = robotFactory2.makeRobot(new Model(size), 999999, true, R)
    for(i <- 1 to 3){
      RobotAlphaBetaFlow.DEPTH = i
      val start = System.currentTimeMillis()
      val move = flow.makeMove()
      val end = System.currentTimeMillis()
      writer5.writeToFile((end-start).toString() + "\n")
    }
    RobotAlphaBetaFlow.DEPTH = 2

    //RobotAlphaMonteCarlo - MCTS_TIME
    val mc1 = robotFactory3.makeRobot(new Model(size), 999999, true, R).asInstanceOf[RobotMonteCarlo]
    for(i <- 500 to 10000 by 500){
      RobotMonteCarlo.MCTS_TIME = i
      val start = System.currentTimeMillis()
      val move = mc1.makeMove()
      val end = System.currentTimeMillis()
      writer6.writeToFile((end-start).toString() + "\n")
    }
    RobotMonteCarlo.MCTS_TIME = 10000
    //RobotAlphaMonteCarlo - WIN_SCORE
    val mc2 = robotFactory3.makeRobot(new Model(size), 999999, true, R).asInstanceOf[RobotMonteCarlo]
    for(i <- 0 to 50 by 10){
      RobotMonteCarlo.WIN_SCORE = i
      val start = System.currentTimeMillis()
      val move = mc2.makeMove()
      val end = System.currentTimeMillis()
      writer7.writeToFile((end-start).toString() + "\n")
    }
    RobotMonteCarlo.WIN_SCORE = 10

    //RobotAlphaMonteCarlo - KNOWLEDGE_THRESHOLD
    val mc3 = robotFactory3.makeRobot(new Model(size), 999999, true, R).asInstanceOf[RobotMonteCarlo]
    for(i <- 0 to 60 by 20){
      montecarlo.State.KNOWLEDGE_THRESHOLD = i
      val start = System.currentTimeMillis()
      val move = mc2.makeMove()
      val end = System.currentTimeMillis()
      writer8.writeToFile((end-start).toString() + "\n")
    }
    montecarlo.State.KNOWLEDGE_THRESHOLD = Integer.MAX_VALUE
    val _start = System.currentTimeMillis()
    val move = mc3.makeMove()
    val _end = System.currentTimeMillis()
    writer8.writeToFile((_end-_start).toString() + "\n")

    montecarlo.State.KNOWLEDGE_THRESHOLD = 15

    //RobotAlphaMonteCarlo - KNOWLEDGE_THRESHOLD
    val mc4 = robotFactory3.makeRobot(new Model(size), 999999, true, R).asInstanceOf[RobotMonteCarlo]
    for(i <- 0 to 5000 by 1000){
      montecarlo.State.HSEARCH_TIME_LIMIT = i
      val start = System.currentTimeMillis()
      val move = mc4.makeMove()
      val end = System.currentTimeMillis()
      writer9.writeToFile((end-start).toString() + "\n")
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



}
