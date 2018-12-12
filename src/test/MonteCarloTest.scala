package test
import hexagony._
import montecarlo.MonteCarloTreeSearch
import org.junit.Assert._
import org.junit.Test
import org.junit.Before


class MonteCarloTest extends Const {
  @Test def firstTest = {
    val mod = new Model(5)
    val mcts = new MonteCarloTreeSearch()
    println(mcts.findNextMove(mod, 1).myCells(R))
  }
}
