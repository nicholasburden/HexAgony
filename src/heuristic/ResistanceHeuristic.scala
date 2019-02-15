package heuristic

import hexagony._
import circuits._
import hsearch._

class ResistanceHeuristic extends Const {

  //Evaluate a given board for a particular colour
  def evaluate(model: Model, colour: Colour, hme: HSearch, hthem: HSearch): Double = {

    if (model.solution(colour)) {
      //Player has won
      return Double.PositiveInfinity
    }
    else if (model.solution()) {
      //Other player has won
      return Double.NegativeInfinity
    }

    //Configure orientation of board depending on if pie rule played or not
    var blueInitial = B
    var redInitial = R

    if (model.pie) {
      //Change orientation
      blueInitial = R; redInitial = B
    }

      //Circuits for each direction
    val blueCircuit: HexCircuit = new HexCircuit(model.N, blueInitial)
    val redCircuit: HexCircuit = new HexCircuit(model.N, redInitial)

    blueCircuit.initial
    redCircuit.initial



    var hSearchBlue = hme
    var hSearchRed = hthem
    if (colour.equals(R)) {
      hSearchRed = hme
      hSearchBlue = hthem
    }

    //Array to store resistances of each cell
    val cellResistancesBlue: Array[Double] = Array.ofDim(model.N * model.N + 2)
    val cellResistancesRed: Array[Double] = Array.ofDim(model.N * model.N + 2)

    //Boundaries have negligible resistance
    cellResistancesBlue(0) = ResistanceHeuristic.epsilon
    cellResistancesBlue(model.N * model.N + 1) = ResistanceHeuristic.epsilon
    cellResistancesRed(0) = ResistanceHeuristic.epsilon
    cellResistancesRed(model.N * model.N + 1) = ResistanceHeuristic.epsilon

    /*
    Cells of player's colour have negligible resistance
    Empty cells have resistance of 1 unit
     */
    for (cell <- model.myCells(B)) {
      val i = cell.i * model.N + cell.j + 1
      cellResistancesBlue(i) = ResistanceHeuristic.epsilon
      cellResistancesRed(i) = Double.PositiveInfinity
    }
    for (cell <- model.myCells(R)) {
      val i = cell.i * model.N + cell.j + 1
      cellResistancesRed(i) = ResistanceHeuristic.epsilon
      cellResistancesBlue(i) = Double.PositiveInfinity
    }

    for (cell <- model.myCells(O)) {
      val i = cell.i * model.N + cell.j + 1
      cellResistancesBlue(i) = 1d
      cellResistancesRed(i) = 1d
    }

    //Set resistances for wires between neighbouring cells by adding resistances of the end cells
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


    //Set resistance of wires between non neighbouring cells based on results of H-Search
    for (node1 <- blueCircuit.getNodes) {
      for (node2 <- blueCircuit.getNodes) {

        val cell1 = getCell(node1.id, B, hSearchBlue); val cell2 = getCell(node2.id, B, hSearchBlue)
        if(!(cell1.colour.equals(R) || cell2.colour.equals(R))) {

          val repId1 = getNodeId(hSearchBlue.G.find(cell1).get, hSearchBlue.model.N)
          val repId2 = getNodeId(hSearchBlue.G.find(cell2).get, hSearchBlue.model.N)

          val strongCarriers = hSearchBlue.getStrongCarriers(cell1, cell2, false)
          if (strongCarriers.nonEmpty) {
            blueCircuit.addLink(repId1, repId2)
            //Add wire with negligible resistance between two strongly connected cells
            blueCircuit.setResistance(blueCircuit.nodes(repId1), blueCircuit.nodes(repId2), ResistanceHeuristic.epsilon)
          }
        }
      }
    }

    for (node1 <- redCircuit.getNodes) {
      for (node2 <- redCircuit.getNodes) {
        val cell1 = getCell(node1.id, R, hSearchRed); val cell2 = getCell(node2.id, R, hSearchRed)
        if(!(cell1.colour.equals(B) || cell2.colour.equals(B))) {
          val repId1 = getNodeId(hSearchRed.G.find(cell1).get, hSearchRed.model.N)
          val repId2 = getNodeId(hSearchRed.G.find(cell2).get, hSearchRed.model.N)
          val strongCarriers = hSearchRed.getStrongCarriers(cell1, cell2, false)
          if (strongCarriers.nonEmpty) {
            redCircuit.addLink(repId1, repId2)
            //Add wire with negligible resistance between two strongly connected cells
            redCircuit.setResistance(redCircuit.nodes(repId1), redCircuit.nodes(repId2), ResistanceHeuristic.epsilon)
          }
        }
      }
    }



    //We will only consider weak connections for colour c if it is c's turn next. There is no point accounting for a weak connection of a player who has just gone since they will not be able to make the connection
    val redsGo = (model.pie && model.count % 2 == 1) || (!model.pie && model.count % 2 == 0)
    val blueWeakRes = ResistanceHeuristic.epsilon
    val redWeakRes = ResistanceHeuristic.epsilon
    if (!redsGo) {
      //Blues go next
      for (node1 <- blueCircuit.getNodes) {
        for (node2 <- blueCircuit.getNodes) {
          val cell1 = getCell(node1.id, B, hSearchBlue); val cell2 = getCell(node2.id, B, hSearchBlue)
          if(!(cell1.colour.equals(R) || cell2.colour.equals(R))) {
            val repId1 = getNodeId(hSearchBlue.G.find(cell1).get, hSearchBlue.model.N)
            val repId2 = getNodeId(hSearchBlue.G.find(cell2).get, hSearchBlue.model.N)
            val weakCarriers = hSearchBlue.getWeakCarriers(cell1, cell2, false)
            if (weakCarriers.nonEmpty) {
              blueCircuit.addLink(repId1, repId2)
              //Add wire with negligible resistance between two weakly connected cells
              blueCircuit.setResistance(blueCircuit.nodes(repId1), blueCircuit.nodes(repId2), blueWeakRes)
            }
          }
        }
      }
    }
    else {
      //Reds go next
      for (node1 <- redCircuit.getNodes) {
        for (node2 <- redCircuit.getNodes) {
          val cell1 = getCell(node1.id, R, hSearchRed); val cell2 = getCell(node2.id, R, hSearchRed)
          if(!(cell1.colour.equals(B) || cell2.colour.equals(B))) {
            val repId1 = getNodeId(hSearchRed.G.find(cell1).get, hSearchRed.model.N)
            val repId2 = getNodeId(hSearchRed.G.find(cell2).get, hSearchRed.model.N)
            val weakCarriers = hSearchRed.getWeakCarriers(cell1, cell2, false)
            if (weakCarriers.nonEmpty) {
              redCircuit.addLink(repId1, repId2)
              //Add wire with negligible resistance between two weakly connected cells
              redCircuit.setResistance(redCircuit.nodes(repId1), redCircuit.nodes(repId2), redWeakRes)
            }
          }
        }
      }
    }




    //Solve circuits
    val circuitSolver = new CircuitSolver
    val redResistance = circuitSolver.getResistance(redCircuit)
    val blueResistance = circuitSolver.getResistance(blueCircuit)



    var result: Double = 0

    //Heuristic is the log of the ratio
    if (colour.equals(B)) {
      result = Math.log(redResistance / blueResistance)
    }
    else {
      result = Math.log(blueResistance / redResistance)
    }
    return result
  }


  def getCell(nodeId: Int, c: Colour, hsearch: HSearch): Cell = {
    //nodeID -> Cell
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
  def getNodeId(cell : Cell, N : Int) : Int = {
    //Cell -> nodeID
    if (cell.equals(new Cell(-1, 0)) || cell.equals(new Cell(0, -1))){
      return 0
    }
    else if (cell.equals(new Cell(-3, 0)) || cell.equals(new Cell(0, -3))){
      return N*N
    }
    else{
      return (cell.i * N) + cell.j + 1
    }

  }

}

object ResistanceHeuristic {
  val epsilon = 0.00001d
}