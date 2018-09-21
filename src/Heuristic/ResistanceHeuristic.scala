package Heuristic
import hexagony._
import Circuits._
import HSearch._
class ResistanceHeuristic extends Const{
  def evaluate(model: Model, colour : Colour) : Float = {
    val DEPTH = 5
    val blueCircuit : HexCircuit = new HexCircuit(model.N, B)
    val redCircuit : HexCircuit = new HexCircuit(model.N, R)
    val hSearchBlue = new HSearch(model, B, DEPTH)
    val hSearchRed = new HSearch(model, B, DEPTH)
    hSearchBlue.search
    hSearchRed.search
    val cellResistancesBlue : Array[Float] = Array.ofDim(model.N * model.N + 2)
    val cellResistancesRed : Array[Float] = Array.ofDim(model.N * model.N + 2)
    cellResistancesBlue(0) = 1
    cellResistancesBlue(model.N * model.N + 1) = 1
    cellResistancesRed(0) = 1
    cellResistancesRed(model.N * model.N + 1) = 1
    for(cell <- model.myCells(B)){
      val i = cell.i * model.N + cell.j + 1
      cellResistancesBlue(i) = 0
      cellResistancesRed(i) = Float.PositiveInfinity
    }
    for(cell <- model.myCells(R)){
      val i = cell.i * model.N + cell.j + 1
      cellResistancesRed(i) = 0
      cellResistancesBlue(i) = Float.PositiveInfinity
    }
    for(cell <- model.myCells(O)){
      val i = cell.i * model.N + cell.j + 1
      cellResistancesBlue(i) = 1
      cellResistancesRed(i) = 1
    }
    for(node1 <- blueCircuit.getNodes){
      for(node2 <- blueCircuit.getNodes){
        blueCircuit.setResistance(node1, node2, cellResistancesBlue(node1.id) + cellResistancesBlue(node2.id))
      }
    }
    for(node1 <- redCircuit.getNodes){
      for(node2 <- redCircuit.getNodes){
        redCircuit.setResistance(node1, node2, cellResistancesRed(node1.id) + cellResistancesRed(node2.id))
      }
    }

    for(node1 <- blueCircuit.getNodes){
      for(node2 <- blueCircuit.getNodes){
        val cell1 = model.board(node1.id / model.N)((node1.id-1) % model.N)
        val cell2 = model.board(node2.id / model.N)((node2.id-1) % model.N)
        val strongCarriers = hSearchBlue.getStrongCarriers(cell1, cell2)
        if(strongCarriers.nonEmpty){

        }
      }
    }
  }

  def areNearestNeighbours(g1 : Cell, g2: Cell): Boolean ={
    val x1 = g1.i
    val x2 = g2.i
    val y1 = g1.j
    val y2 = g2.j

    val h = (x1 == x2 && y1 == y2+1) || (x1 == x2 && y1 == y2-1) || (y1 == y2 && x1 == x2+1) || (y1 == y2 && x1 == x2-1) || (x1 == x2+1 && y1 == y2+1) || (x1 == x2-1 && y1 == y2-1)
    //println(h)
    return h
  }
}
