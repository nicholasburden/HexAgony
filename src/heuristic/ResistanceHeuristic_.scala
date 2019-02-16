package heuristic

import circuits._
import hexagony._
import hsearch._

class ResistanceHeuristic_(var mod : Model, colour : Colour) extends Const {


  //Configure orientation of board depending on if pie rule played or not
  var blueInitial = B
  var redInitial = R

  if (mod.pie) {
    //Change orientation
    blueInitial = R; redInitial = B
  }

  //Circuits for each direction
  val blueCircuit_ : HexCircuit = new HexCircuit(mod.N, blueInitial)
  val redCircuit_ : HexCircuit = new HexCircuit(mod.N, redInitial)
  blueCircuit_.initial
  redCircuit_.initial

  //Array to store resistances of each cell
  val cellResistancesBlue: Array[Double] = Array.ofDim(mod.N * mod.N + 2)
  val cellResistancesRed: Array[Double] = Array.ofDim(mod.N * mod.N + 2)

  //Boundaries have negligible resistance
  cellResistancesBlue(0) = ResistanceHeuristic.epsilon
  cellResistancesBlue(mod.N * mod.N + 1) = ResistanceHeuristic.epsilon
  cellResistancesRed(0) = ResistanceHeuristic.epsilon
  cellResistancesRed(mod.N * mod.N + 1) = ResistanceHeuristic.epsilon

  /*
  Cells of player's colour have negligible resistance
  Empty cells have resistance of 1 unit
   */
  println("BLUE: " + mod.myCells(B))
  println("RED: " + mod.myCells(R))
  for (cell <- mod.myCells(B)) {
    val i = cell.i * mod.N + cell.j + 1
    cellResistancesBlue(i) = ResistanceHeuristic.epsilon
    cellResistancesRed(i) = Double.PositiveInfinity
  }
  for (cell <- mod.myCells(R)) {
    val i = cell.i * mod.N + cell.j + 1
    cellResistancesRed(i) = ResistanceHeuristic.epsilon
    cellResistancesBlue(i) = Double.PositiveInfinity
  }

  for (cell <- mod.myCells(O)) {
    val i = cell.i * mod.N + cell.j + 1
    cellResistancesBlue(i) = 1d
    cellResistancesRed(i) = 1d
  }

  //Set resistances for wires between neighbouring cells by adding resistances of the end cells
  for (node1 <- blueCircuit_.getNodes) {
    for (node2 <- blueCircuit_.getNodes) {
      val cell1 = getCell(node1.id, B, mod); val cell2 = getCell(node2.id, B, mod)
      if (areNearestNeighbours(cell1, cell2, mod)) {
        blueCircuit_.setResistance(node1, node2, cellResistancesBlue(node1.id) + cellResistancesBlue(node2.id))
      }
    }
  }


  for (node1 <- redCircuit_.getNodes) {
    for (node2 <- redCircuit_.getNodes) {
      val cell1 = getCell(node1.id, R, mod); val cell2 = getCell(node2.id, R, mod)
      if (areNearestNeighbours(cell1, cell2, mod)) {
        redCircuit_.setResistance(node1, node2, cellResistancesRed(node1.id) + cellResistancesRed(node2.id))
      }
    }
  }
  //Evaluate a given board for a particular colour
  def evaluate(model : Model, colour: Colour, hme: HSearch_, hthem: HSearch_): Double = {
    

    //mod = mod.copy()
    val blueCircuit = blueCircuit_.clone()
    val redCircuit = redCircuit_.clone()
    /*println("NEW")
    for(n1 <- redCircuit.nodes;n2 <- redCircuit.nodes){
      if(redCircuit.resistance isDefinedAt((n1, n2))) println(n1.id + ", " + n2.id + ": " + redCircuit.resistance((n1,n2)))

    }
    */
    var hSearchBlue = hme
    var hSearchRed = hthem
    if (colour.equals(R)) {
      hSearchRed = hme
      hSearchBlue = hthem
    }
    if (model.solution(colour)) {
      //Player has won
      return Double.PositiveInfinity
    }
    else if (model.solution()) {
      //Other player has won
      return Double.NegativeInfinity
    }
    //Set resistance of wires between non neighbouring cells based on results of H-Search
    for (node1 <- blueCircuit.getNodes) {
      //val cell1 =
      //for (node2 <- blueCircuit.getNodes) {
      val cell1 = getCell(node1.id, B, model)
      if(!(cell1.colour.equals(R))) {
        for(cell2_ <- hSearchBlue.strongDual(hSearchBlue.G.find(cell1).get)){
          var cell2 = cell2_
          if(cell2.i >= 0 && cell2.j >= 0) cell2 = hSearchBlue.model.board(cell2_)

        //val cell1 = getCell(node1.id, B, model); val cell2 = getCell(node2.id, B, model)
          if(!cell2.colour.equals(R)){
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
    }
    /*
    //Set resistance of wires between non neighbouring cells based on results of H-Search
    for (node1 <- blueCircuit.getNodes) {
      for (node2 <- blueCircuit.getNodes) {

        val cell1 = getCell(node1.id, B, model); val cell2 = getCell(node2.id, B, model)
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
        val cell1 = getCell(node1.id, R, model); val cell2 = getCell(node2.id, R, model)
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
    */

    for (node1 <- redCircuit.getNodes) {
      //val cell1 =
      //for (node2 <- blueCircuit.getNodes) {
      val cell1 = getCell(node1.id, R, model)
      if(!cell1.colour.equals(B)){
        for(cell2_ <- hSearchRed.strongDual(hSearchRed.G.find(cell1).get)){
          var cell2 = cell2_
          if(cell2.i >= 0 && cell2.j >= 0) cell2 = hSearchBlue.model.board(cell2_)
          //val cell1 = getCell(node1.id, B, model); val cell2 = getCell(node2.id, B, model)
          if(!cell2.colour.equals(B)) {

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
    }


    //We will only consider weak connections for colour c if it is c's turn next. There is no point accounting for a weak connection of a player who has just gone since they will not be able to make the connection
    val redsGo = (model.pie && model.count % 2 == 1) || (!model.pie && model.count % 2 == 0)
    val blueWeakRes = ResistanceHeuristic.epsilon
    val redWeakRes = ResistanceHeuristic.epsilon
    if (!redsGo) {
      //Blues go next
      for (node1 <- blueCircuit.getNodes) {
        for (node2 <- blueCircuit.getNodes) {
          val cell1 = getCell(node1.id, B, model); val cell2 = getCell(node2.id, B, model)
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
          val cell1 = getCell(node1.id, R, model); val cell2 = getCell(node2.id, R, model)
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


  def getCell(nodeId: Int, c: Colour, model : Model): Cell = {
    //nodeID -> Cell
    var cell = new Cell(-1, -1)
    if (c.equals(R)) {
      if (nodeId == 0) {
        cell = HSearch.boundaryRed1
      }
      else if (nodeId == model.N * model.N + 1) {
        cell = HSearch.boundaryRed2
      }
      else {
        cell = model.board((nodeId - 1) / model.N)((nodeId - 1) % model.N)
      }
    }
    else {
      if (nodeId == 0) {
        cell = HSearch.boundaryBlue1
      }
      else if (nodeId == model.N * model.N + 1) {
        cell = HSearch.boundaryBlue2
      }
      else {
        cell = model.board((nodeId - 1) / model.N)((nodeId - 1) % model.N)
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


  def areNearestNeighbours(g1: Cell, g2: Cell, model : Model): Boolean = {
    val x1 = g1.i
    val x2 = g2.i
    val y1 = g1.j
    val y2 = g2.j

    var h = (x1 == x2 && y1 == y2 + 1) || (x1 == x2 && y1 == y2 - 1) || (y1 == y2 && x1 == x2 + 1) || (y1 == y2 && x1 == x2 - 1) || (x1 == x2 + 1 && y1 == y2 + 1) || (x1 == x2 - 1 && y1 == y2 - 1)
    h = h || (x1 == 0 && x2 == -1) || (x1 == model.N - 1 && x2 == -3) || (y1 == 0 && y2 == -1) || (y1 == model.N - 1 && y2 == -3)
    h = h || (x2 == 0 && x1 == -1) || (x2 == model.N - 1 && x1 == -3) || (y2 == 0 && y1 == -1) || (y2 == model.N - 1 && y1 == -3)
    return h
  }

}

