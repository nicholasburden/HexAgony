package Heuristic

import hexagony._
import Circuits._
import HSearch._

class ResistanceHeuristic extends Const {
  def evaluate(model: Model, colour: Colour, hme: HSearch, hthem: HSearch): Float = {
    if (model.solution(colour)) {
      return Float.PositiveInfinity
    }
    else if (model.solution()) {
      return Float.NegativeInfinity
    }
    val blueCircuit: HexCircuit = new HexCircuit(model.N, B)
    val redCircuit: HexCircuit = new HexCircuit(model.N, R)
    var hSearchBlue = hme
    var hSearchRed = hthem
    if (colour.equals(R)) {
      hSearchRed = hme
      hSearchBlue = hthem
    }
    val cellResistancesBlue: Array[Float] = Array.ofDim(model.N * model.N + 2)
    val cellResistancesRed: Array[Float] = Array.ofDim(model.N * model.N + 2)
    cellResistancesBlue(0) = ResistanceHeuristic.epsilon
    cellResistancesBlue(model.N * model.N + 1) = ResistanceHeuristic.epsilon
    cellResistancesRed(0) = ResistanceHeuristic.epsilon
    cellResistancesRed(model.N * model.N + 1) = ResistanceHeuristic.epsilon
    for (cell <- model.myCells(B)) {
      val i = cell.i * model.N + cell.j + 1
      cellResistancesBlue(i) = ResistanceHeuristic.epsilon
      cellResistancesRed(i) = Float.PositiveInfinity
    }
    for (cell <- model.myCells(R)) {
      val i = cell.i * model.N + cell.j + 1
      cellResistancesRed(i) = ResistanceHeuristic.epsilon
      cellResistancesBlue(i) = Float.PositiveInfinity
    }

    for (cell <- model.myCells(O)) {
      val i = cell.i * model.N + cell.j + 1
      cellResistancesBlue(i) = 1f
      cellResistancesRed(i) = 1f
    }

    for (node1 <- blueCircuit.getNodes) {
      for (node2 <- blueCircuit.getNodes) {
        val cell1 = getCell(node1.id, B, hSearchBlue); val cell2 = getCell(node2.id, B, hSearchBlue)
        if (hSearchBlue.areNearestNeighbours(cell1, cell2)) {
          blueCircuit.setResistance(node1, node2, cellResistancesBlue(node1.id) + cellResistancesBlue(node2.id))
        }
      }
    }


    for (node1 <- redCircuit.getNodes) {
      for (node2 <- redCircuit.getNodes) {
        val cell1 = getCell(node1.id, R, hSearchRed); val cell2 = getCell(node2.id, R, hSearchRed)
        if (hSearchRed.areNearestNeighbours(cell1, cell2)) {
          redCircuit.setResistance(node1, node2, cellResistancesRed(node1.id) + cellResistancesRed(node2.id))
        }
      }
    }

    for (node1 <- blueCircuit.getNodes) {
      for (node2 <- blueCircuit.getNodes) {
        val cell1 = getCell(node1.id, B, hSearchBlue); val cell2 = getCell(node2.id, B, hSearchBlue)

        val strongCarriers = hSearchBlue.getStrongCarriers(cell1, cell2, false)
        if (strongCarriers.nonEmpty) {
          blueCircuit.addLink(node1.id, node2.id)
          blueCircuit.setResistance(node1, node2, (1f - (1f / strongCarriers.size)) / 7)
        }
      }
    }

    for (node1 <- redCircuit.getNodes) {
      for (node2 <- redCircuit.getNodes) {
        val cell1 = getCell(node1.id, R, hSearchRed); val cell2 = getCell(node2.id, R, hSearchRed)
        val strongCarriers = hSearchRed.getStrongCarriers(cell1, cell2, false)
        if (strongCarriers.nonEmpty) {
          redCircuit.addLink(node1.id, node2.id)
          redCircuit.setResistance(node1, node2, (1f - (1f / strongCarriers.size)) / 7)
        }
      }
    }
    val redsGo = model.myCells(R).size <= (model.myCells(B) ++ model.myCells(R)).size.toFloat / 2.0
    val myGo = (colour.equals(B) && !redsGo) || (colour.equals(R) && redsGo)
    if (myGo) {
      if (colour.equals(B)) {
        for (node1 <- blueCircuit.getNodes) {
          for (node2 <- blueCircuit.getNodes) {
            val cell1 = getCell(node1.id, B, hSearchBlue); val cell2 = getCell(node2.id, B, hSearchBlue)
            val weakCarriers = hSearchBlue.getWeakCarriers(cell1, cell2, false)
            if (weakCarriers.nonEmpty) {
              blueCircuit.addLink(node1.id, node2.id)
              blueCircuit.setResistance(node1, node2, (1f - (1f / (weakCarriers.size))) / 3)
            }
          }
        }
      }
      else {
        for (node1 <- redCircuit.getNodes) {
          for (node2 <- redCircuit.getNodes) {
            val cell1 = getCell(node1.id, R, hSearchRed); val cell2 = getCell(node2.id, R, hSearchRed)
            val weakCarriers = hSearchRed.getWeakCarriers(cell1, cell2, false)
            if (weakCarriers.nonEmpty) {
              redCircuit.addLink(node1.id, node2.id)
              redCircuit.setResistance(node1, node2, (1f - (1f / (weakCarriers.size))) / 3)
            }
          }
        }
      }
    }


    val circuitSolver = new CircuitSolver

    val redResistance = circuitSolver.getResistance(redCircuit)
    val blueResistance = circuitSolver.getResistance(blueCircuit)


    //println("RED: " + redResistance)
    //println("BLUE: " + blueResistance)
    var result: Float = 0
    if (colour.equals(B)) {

      result = Math.log(redResistance / blueResistance).toFloat
    }
    else {
      result = Math.log(blueResistance / redResistance).toFloat
    }



    return result
  }


  def getCell(nodeId: Int, c: Colour, hsearch: HSearch): Cell = {
    var cell = new Cell(-1, -1)
    if (c.equals(R)) {
      if (nodeId == 0) {
        cell = HSearch.boundaryRed1
      }
      else if (nodeId == hsearch.model.N * hsearch.model.N + 1) {
        cell = HSearch.boundaryRed2
      }
      else {
        cell = hsearch.model.board((nodeId - 1) / hsearch.model.N)((nodeId - 1) % hsearch.model.N)
      }
    }
    else {
      if (nodeId == 0) {
        cell = HSearch.boundaryBlue1
      }
      else if (nodeId == hsearch.model.N * hsearch.model.N + 1) {
        cell = HSearch.boundaryBlue2
      }
      else {
        cell = hsearch.model.board((nodeId - 1) / hsearch.model.N)((nodeId - 1) % hsearch.model.N)
      }
    }

    return cell
  }




}

object ResistanceHeuristic{
  val epsilon = 0.0000001f
  def maxNotInfinity(boardSize : Int) : Float = (1f/epsilon) + 1
}