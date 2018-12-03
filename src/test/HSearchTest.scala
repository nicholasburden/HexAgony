package test
import hexagony._
import HSearch._
import org.hamcrest.Matchers
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class HSearchTest extends Const{
  var h = new HSearch(null, null)
  @Before def initialise() = {
    val model = new Model(5)
    h = new HSearch(model, R)
    h.initial
    h.search

  }
  @Test def verifyStrongConnectionSimple() = {
    assertEquals(Set(new Cell(2,1), new Cell(2, 2)), h.getStrongCarriers(new Cell(1,1), new Cell(3, 2), true))
  }
  @Test def verifyStrongConnectionBoundary() = {
    assertEquals(Set(new Cell(0,0), new Cell(1, 0)), h.getStrongCarriers(HSearch.boundaryRed1, new Cell(1, 1), true))
  }
  @Test def verifyStrongConnectionBoundaryLarge() = {
    assertEquals(Set(new Cell(2, 3), new Cell(1, 4), new Cell(3, 3), new Cell(2, 4), new Cell(3, 4), new Cell(1, 2), new Cell(4, 4), new Cell(0,2), new Cell(1, 3)), h.getStrongCarriers(h.G.find(HSearch.boundaryRed2).get, h.G.find(new Cell(2, 2)).get, true))
  }
  @Test def verifyWeakConnectionLarge() = {

    var h1 = h.makeMove(2,1,B)
    h1 = h1.makeMove(0,2,B)

    h1.initial
    h1.search
    assertEquals(Set(new Cell(2, 3), new Cell(0, 1), new Cell(1, 1), new Cell(1, 2), new Cell(1, 3)), h1.getWeakCarriers(h1.G.find(new Cell(0,0)).get, h1.G.find(new Cell(2, 4)).get, true))
  }
  @Test def verifyStrongConnectionMakeMove() = {
    var h1 = h.makeMove(0,0,R)
    h1 = h1.makeMove(1,2,R)
    h1 = h1.makeMove(0,1,B)
    assertEquals(Set(), h1.getStrongCarriers(h1.G.find(new Cell(0,0)).get, h1.G.find(new Cell(1, 2)).get, true))
  }
  @Test def verifyWeakConnectionMakeMove() = {
    var h1 = h.makeMove(0,0,R)
    h1 = h1.makeMove(1,2,R)
    h1 = h1.makeMove(0,1,B)
    assertEquals(Set(new Cell(1,1)), h1.getWeakCarriers(h1.G.find(new Cell(1,2)).get, h1.G.find(new Cell(0, 0)).get, true))
  }
  @Test def verifyWeakConnectionSimple() = {
    assertEquals(Set(new Cell(2,1)), h.getWeakCarriers(new Cell(1,1), new Cell(3, 1), true))
  }
  @Test def verifyWeakConnectionBoundary() = {
    assertThat(h.getWeakCarriers(HSearch.boundaryRed1, new Cell(1, 1), true), Matchers.either(Matchers.is(Set(new Cell(0,0)))).or(Matchers.is(Set(new Cell(1,0)))))
  }
  @Test def verifyFullStrongConnection():Unit = {
    var h1 = h.makeMove(1,1,R)
    h1 = h1.makeMove(2,3,R)
    val t1 = h1.getStrongCarriers(HSearch.boundaryRed1, new Cell(1,1), true).equals(Set(new Cell(0,0), new Cell(1,0)))
    val t2 = h1.getStrongCarriers(new Cell(1,1), new Cell(2,3), true).equals(Set(new Cell(1,2), new Cell(2,2)))
    val t3 = h1.getStrongCarriers(new Cell(2,3), HSearch.boundaryRed2, true).equals(Set(new Cell(2,4), new Cell(3,4)))
    //val t4 = h1.getStrongCarriers(HSearch.boundaryRed1, HSearch.boundaryRed2, true).size == 0
    //val t5 = h1.getWeakCarriers(HSearch.boundaryRed1, HSearch.boundaryRed2, true).size == 0
    assertTrue(t1&&t2&&t3)
  }
  @Test def verifyFullWeakConnection():Unit = {
    //var h1 = h.makeMove(1,0,R)
    //var h1 = h.makeMove(1,2,R)
    var h1 = h.makeMove(1,3,R)


    val t1 = h1.getWeakCarriers(new Cell(1,0), new Cell(1,2), true).equals(Set(new Cell(1,1)))

    val t2 = h1.getStrongCarriers(new Cell(1,3), HSearch.boundaryRed2, true).equals(Set(new Cell(1,4), new Cell(2,4)))
    System.out.println(h1.getStrongCarriers(new Cell(1,2), HSearch.boundaryRed2, true))
    assertTrue(t1&&t2)
  }
  @Test def verifyFullMixedConnection():Unit = {
    val m2 = new Model(5)
    m2.playMove(m2.board(3)(1), R)
    m2.playMove(m2.board(4)(3), R)
    val h1 = new HSearch(m2, R)
    h1.initial
    h1.search
    //var h1 = h.makeMove(3,1,R)
    //h1 = h.makeMove(4,3,R)
    val t1 = h1.getStrongCarriers(HSearch.boundaryRed1, new Cell(3,1), true).equals(Set(new Cell(2,0), new Cell(3,0)))
    val t2 = h1.getStrongCarriers(new Cell(3,1), new Cell(4,3), true).equals(Set(new Cell(3,2), new Cell(4,2)))

    val t3 = h1.getWeakCarriers(new Cell(4,3), HSearch.boundaryRed2, true).equals(Set(new Cell(4,4)))
    val t4 = h1.getStrongCarriers(HSearch.boundaryRed1, HSearch.boundaryRed2, true).size == 0
    val t5 = h1.getWeakCarriers(HSearch.boundaryRed1, HSearch.boundaryRed2, true).size == 0
    assertTrue(t1&&t2&&t3&&t4&&t5)
  }

}
