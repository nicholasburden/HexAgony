package test

import hexagony._
import hsearch._
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class HSearchTest extends Const {
  var hRed = new HSearch(null, null)
  var hBlue = new HSearch(null, null)

  @Before def initialise() = {
    //Set up a blank model of size 5X5, and search from both perspectives
    val model = new Model(5)
    hRed = new HSearch(model, R)
    hRed.initial
    hRed.search
    hBlue = new HSearch(model, B)
    hBlue.initial
    hBlue.search

  }

  @Test def verifyStrongConnectionSimple() = {
    assertEquals(Set(new Cell(2, 1), new Cell(2, 2)), hRed.getStrongCarriers(new Cell(1, 1), new Cell(3, 2), true))
  }

  @Test def verifyStrongConnectionBoundary() = {
    assertEquals(Set(new Cell(0, 0), new Cell(1, 0)), hRed.getStrongCarriers(HSearch.boundaryRed1, new Cell(1, 1), true))
  }


  @Test def verifyStrongConnectionMakeMove() = {
    var h1 = hRed.makeMove(0, 0, R)
    h1 = h1.makeMove(1, 2, R)
    h1 = h1.makeMove(0, 1, B)
    assertEquals(Set(), h1.getStrongCarriers(h1.G.find(new Cell(0, 0)).get, h1.G.find(new Cell(1, 2)).get, true))
  }

  @Test def verifyWeakConnectionMakeMove() = {
    var h1 = hRed.makeMove(0, 0, R)
    h1 = h1.makeMove(1, 2, R)
    h1 = h1.makeMove(0, 1, B)
    assertEquals(Set(new Cell(1, 1)), h1.getWeakCarriers(h1.G.find(new Cell(1, 2)).get, h1.G.find(new Cell(0, 0)).get, true))
  }


  @Test def verifyFullStrongConnection(): Unit = {
    var h1 = hRed.makeMove(1, 1, R)
    h1 = h1.makeMove(2, 3, R)
    val t1 = h1.getStrongCarriers(HSearch.boundaryRed1, new Cell(1, 1), true).equals(Set(new Cell(0, 0), new Cell(1, 0)))
    val t2 = h1.getStrongCarriers(new Cell(1, 1), new Cell(2, 3), true).equals(Set(new Cell(1, 2), new Cell(2, 2)))
    val t3 = h1.getStrongCarriers(new Cell(2, 3), HSearch.boundaryRed2, true).equals(Set(new Cell(2, 4), new Cell(3, 4)))
    //val t4 = h1.getStrongCarriers(HSearch.boundaryRed1, HSearch.boundaryRed2, true).size == 0
    //val t5 = h1.getWeakCarriers(HSearch.boundaryRed1, HSearch.boundaryRed2, true).size == 0
    assertTrue(t1 && t2 && t3)
  }

  @Test def verifyFullWeakConnection(): Unit = {
    //var h1 = h.makeMove(1,0,R)
    //var h1 = h.makeMove(1,2,R)
    var h1 = hRed.makeMove(1, 3, R)


    val t1 = h1.getWeakCarriers(new Cell(1, 0), new Cell(1, 2), true).equals(Set(new Cell(1, 1)))

    val t2 = h1.getStrongCarriers(new Cell(1, 3), HSearch.boundaryRed2, true).equals(Set(new Cell(1, 4), new Cell(2, 4)))
    System.out.println(h1.getStrongCarriers(new Cell(1, 2), HSearch.boundaryRed2, true))
    assertTrue(t1 && t2)
  }

  @Test def verifyFullMixedConnection(): Unit = {
    val m2 = new Model(5)
    m2.playMove(m2.board(3)(1), R)
    m2.playMove(m2.board(4)(3), R)
    val h1 = new HSearch(m2, R)
    h1.initial
    h1.search

    val t1 = h1.getStrongCarriers(HSearch.boundaryRed1, new Cell(3, 1), true).equals(Set(new Cell(2, 0), new Cell(3, 0)))
    val t2 = h1.getStrongCarriers(new Cell(3, 1), new Cell(4, 3), true).equals(Set(new Cell(3, 2), new Cell(4, 2)))
    System.out.println(h1.getStrongCarriers(new Cell(4, 3), HSearch.boundaryRed2, true))
    val t3 = h1.getWeakCarriers(new Cell(4, 3), HSearch.boundaryRed2, true).equals(Set(new Cell(4, 4)))
    val t4 = h1.getStrongCarriers(HSearch.boundaryRed1, HSearch.boundaryRed2, true).size == 0
    val t5 = h1.getWeakCarriers(HSearch.boundaryRed1, HSearch.boundaryRed2, true).size == 0
    assertTrue(t1 && t2 && t3 && t4 && t5)
  }

  @Test def verfifyStrongExample1(): Unit = {
    var h1 = hRed.makeMove(2, 2, R)
    h1 = h1.makeMove(2, 3, B)
    h1 = h1.makeMove(3, 3, R)
    assertEquals(Set(new Cell(4, 4), new Cell(3, 4)), h1.getStrongCarriers(new Cell(3, 3), HSearch.boundaryRed2, true))
  }

  @Test def verfifyWeakExample1(): Unit = {
    var h1 = hRed.makeMove(2, 2, R)
    h1 = h1.makeMove(2, 3, R)
    h1 = h1.makeMove(2, 4, B)
    assertEquals(Set(new Cell(3, 4)), h1.getWeakCarriers(new Cell(2, 3), HSearch.boundaryRed2, true))
  }

  @Test def verfifyStrongExample2(): Unit = {
    var h1 = hRed.makeMove(2, 0, R)
    h1 = h1.makeMove(3, 4, R)
    h1 = h1.makeMove(2, 1, R)
    h1 = h1.makeMove(3, 3, R)
    assertEquals(Set(new Cell(2, 2), new Cell(1, 2)), h1.getStrongCarriers(new Cell(1, 1), new Cell(2, 3), true))
  }




}
