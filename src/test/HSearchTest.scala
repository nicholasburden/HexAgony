package test
import hexagony._
import HSearch._
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
    assertEquals(Set(new Cell(2, 3), new Cell(1, 4), new Cell(3, 3), new Cell(2, 4), new Cell(3, 4), new Cell(1, 2), new Cell(4, 4), new Cell(1, 3)), h.getStrongCarriers(h.G.find(HSearch.boundaryRed2).get, h.G.find(new Cell(2, 2)).get, true))
  }
  @Test def verifyStrongConnectionMakeMove() = {
    h = h.makeMove(0,0,R)
    h = h.makeMove(1,2,R)
    h = h.makeMove(0,1,B)
    assertEquals(Set(), h.getStrongCarriers(h.G.find(new Cell(0,0)).get, h.G.find(new Cell(1, 2)).get, true))
  }
  @Test def verifyWeakgConnectionMakeMove() = {
    h = h.makeMove(0,0,R)
    h = h.makeMove(1,2,R)
    h = h.makeMove(0,1,B)
    assertEquals(Set(new Cell(1,1)), h.getWeakCarriers(h.G.find(new Cell(1,2)).get, h.G.find(new Cell(0, 0)).get, true))
  }
}
