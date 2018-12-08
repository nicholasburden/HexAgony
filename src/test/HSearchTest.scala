package test
import hexagony._
import HSearch._
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
/*
class HSearchTest extends Const{
  var h = new HSearch(null, null)
  var hBlue = new HSearch(null, null)
  @Before def initialise() = {
    val model = new Model(5)
    h = new HSearch(model, R)
    h.initial
    h.search
    hBlue = new HSearch(model, B)
    hBlue.initial
    hBlue.search

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
    System.out.println(h1.getStrongCarriers(new Cell(4,3), HSearch.boundaryRed2, true))
    val t3 = h1.getWeakCarriers(new Cell(4,3), HSearch.boundaryRed2, true).equals(Set(new Cell(4,4)))
    val t4 = h1.getStrongCarriers(HSearch.boundaryRed1, HSearch.boundaryRed2, true).size == 0
    val t5 = h1.getWeakCarriers(HSearch.boundaryRed1, HSearch.boundaryRed2, true).size == 0
    assertTrue(t1&&t2&&t3&&t4&&t5)
  }

  @Test def verfifyStrongExample1() : Unit = {
    var h1 = h.makeMove(2,2,R)
    h1 = h1.makeMove(2,3,B)
    h1 = h1.makeMove(3,3,R)
    assertEquals(Set(new Cell(4,4), new Cell(3,4)), h1.getStrongCarriers(new Cell(3,3), HSearch.boundaryRed2, true))
  }
  @Test def verfifyWeakExample1() : Unit = {
    var h1 = h.makeMove(2,2,R)
    h1 = h1.makeMove(2,3,R)
    h1 = h1.makeMove(2,4,B)
    assertEquals(Set(new Cell(3,4)), h1.getWeakCarriers(new Cell(2,3), HSearch.boundaryRed2, true))
  }
  @Test def verfifyStrongExample2() : Unit = {
    var h1 = h.makeMove(2,0,R)
    h1 = h1.makeMove(3,4,R)
    h1 = h1.makeMove(2,1,R)
    h1 = h1.makeMove(3,3,R)
    assertEquals(Set(new Cell(2,2), new Cell(1,2)), h1.getStrongCarriers(new Cell(1,1), new Cell(2,3), true))
  }
  @Test def verifyWeakExample2() : Unit = {
    val model2 = new Model(4)
    h = new HSearch(model2, R)
    h.initial
    h.search
    hBlue = new HSearch(model2, B)
    hBlue.initial
    hBlue.search
    var h1 = h.makeMove(2,2,R)
    var hBlue1 = hBlue.makeMove(2,2,R)
    h1 = h1.makeMove(1,1,B)
    hBlue1 = hBlue1.makeMove(1,1,B)
    h1 = h1.makeMove(2,1,R)
    hBlue1 = hBlue1.makeMove(2,1,B)
    var res = true
    for(cell1 <- h1.model.myCells(R) ++ Set(HSearch.boundaryRed1, HSearch.boundaryRed2)){
      for(cell2 <- h1.model.myCells(R) ++ Set(HSearch.boundaryRed1, HSearch.boundaryRed2)){
        if(h1.getWeakCarriers(cell1, cell2,false).nonEmpty) res = false

      }
    }
    for(cell1 <- hBlue1.model.myCells(B) ++ Set(HSearch.boundaryBlue1, HSearch.boundaryBlue2)){
      for(cell2 <- hBlue1.model.myCells(B) ++ Set(HSearch.boundaryBlue1, HSearch.boundaryBlue2)){
        //if(h1.getWeakCarriers(cell1, cell2,false).nonEmpty) res = false
        if(hBlue1.getWeakCarriers(cell1, cell2,false).nonEmpty) res = false
      }
    }
    assertTrue(res)
  }
  @Test def verfifyStrongExample3() : Unit = {
    var h1 = h.makeMove(2,2,R)
    var hBlue1 = hBlue.makeMove(2,2,R)
    h1 = h1.makeMove(1,1,B)
    hBlue1 = hBlue1.makeMove(1,1,B)
    //h1 = h1.makeMove(3,0,R)
    //hBlue1 = hBlue1.makeMove(3,0,R)
    h1 = h1.makeMove(2,1,R)
    hBlue1 = hBlue1.makeMove(2,1,B)
    val t1 = Set(new Cell(1,0), new Cell(2,0)).equals(h1.getStrongCarriers(new Cell(2,1), HSearch.boundaryRed1, true))
    val t2 = Set(new Cell(3,1), new Cell(3,2)).equals(hBlue1.getStrongCarriers(new Cell(2,1), HSearch.boundaryBlue2, true))

    //assertTrue(t1&&t2)
  }

}
*/