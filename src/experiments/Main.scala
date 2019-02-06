package experiments


import filewriting.FileWrite
import hexagony.{Colour, Model}

import scala.util.Random

object Main{
  def main(args : Array[String]) = {
    //File kept in the following format: BOT1_BOT2_TIME_SIZE
    val indTest = new IndividualTests
    indTest.test()
    val pairComp = new PairwsieComparison
    pairComp.test()
  }
}
