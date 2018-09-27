package Heuristic
import hexagony._
import Circuits._
import HSearch._
class ResistanceHeuristic extends Const{
  def evaluate(model: Model, colour : Colour) : Float = {

    if(model.solution(colour)){

      return Float.PositiveInfinity

    }
    else if(model.solution()){
      return Float.NegativeInfinity
    }
    var hSearchRed = new HSearch(model, colour)
    var hSearchBlue = new HSearch(model, colour)



    val blueCircuit : HexCircuit = new HexCircuit(model.N, B)
    val redCircuit : HexCircuit = new HexCircuit(model.N, R)

    //val hSearchBlue = new HSearch(model, B)
    //val hSearchRed = new HSearch(model, R)

    //hSearchBlue.search
    //hSearchRed.search


    val cellResistancesBlue : Array[Float] = Array.ofDim(model.N * model.N + 2)
    val cellResistancesRed : Array[Float] = Array.ofDim(model.N * model.N + 2)

    cellResistancesBlue(0) = 0.0000001f
    cellResistancesBlue(model.N * model.N + 1) = 0.0000001f
    cellResistancesRed(0) = 0.0000001f
    cellResistancesRed(model.N * model.N + 1) = 0.0000001f

    for(cell <- model.myCells(B)){
      val i = cell.i * model.N + cell.j + 1
      cellResistancesBlue(i) = 0.0000001f
      cellResistancesRed(i) = Float.PositiveInfinity
    }
    for(cell <- model.myCells(R)){
      val i = cell.i * model.N + cell.j + 1
      cellResistancesRed(i) = 0.0000001f
      cellResistancesBlue(i) = Float.PositiveInfinity
    }

    for(cell <- model.myCells(O)){
      val i = cell.i * model.N + cell.j + 1
      cellResistancesBlue(i) = 1
      cellResistancesRed(i) = 1
    }

    for(node1 <- blueCircuit.getNodes){
      for(node2 <- blueCircuit.getNodes){

        var cell1, cell2 : Cell = new Cell(-1,-1)

        if(node1.id == 0){
          cell1 = hSearchBlue.boundaryBlue1
        }
        else if(node1.id == model.N * model.N + 1){
          cell1 = hSearchBlue.boundaryBlue2
        }
        else{
          cell1 = model.board((node1.id-1) / model.N)((node1.id-1) % model.N)
        }
        if(node2.id == 0){
          cell2 = hSearchBlue.boundaryBlue1
        }
        else if(node2.id == model.N * model.N + 1){
          cell2 = hSearchBlue.boundaryBlue2
        }
        else{
          cell2 = model.board((node2.id-1) / model.N)((node2.id-1) % model.N)
        }

        if(hSearchBlue.areNearestNeighbours(cell1, cell2)) {
          blueCircuit.setResistance(node1, node2, cellResistancesBlue(node1.id) + cellResistancesBlue(node2.id))
        }
      }
    }


    for(node1 <- redCircuit.getNodes){
      for(node2 <- redCircuit.getNodes){
        var cell1, cell2 : Cell = new Cell(-1,-1)

        if(node1.id == 0){
          cell1 = hSearchRed.boundaryRed1
        }
        else if(node1.id == model.N * model.N + 1){
          cell1 = hSearchRed.boundaryRed2
        }
        else{
          cell1 = model.board((node1.id-1) / model.N)((node1.id-1) % model.N)
        }
        if(node2.id == 0){
          cell2 = hSearchRed.boundaryRed1
        }
        else if(node2.id == model.N * model.N + 1){
          cell2 = hSearchRed.boundaryRed2
        }
        else{
          cell2 = model.board((node2.id-1) / model.N)((node2.id-1) % model.N)
        }
        if(hSearchRed.areNearestNeighbours(cell1, cell2)) {
          redCircuit.setResistance(node1, node2, cellResistancesRed(node1.id) + cellResistancesRed(node2.id))

        }
      }
    }

    for(node1 <- blueCircuit.getNodes){
      for(node2 <- blueCircuit.getNodes){
        var cell1, cell2 : Cell = new Cell(0,0)

        if(node1.id == 0){
          cell1 = hSearchBlue.boundaryBlue1
        }
        else if(node1.id == model.N * model.N + 1){
          cell1 = hSearchBlue.boundaryBlue2
        }
        else{
          cell1 = model.board((node1.id-1) / model.N)((node1.id-1) % model.N)
        }
        if(node2.id == 0){
          cell2 = hSearchBlue.boundaryBlue1
        }
        else if(node2.id == model.N * model.N + 1){
          cell2 = hSearchBlue.boundaryBlue2
        }
        else{
          cell2 = model.board((node2.id-1) / model.N)((node2.id-1) % model.N)
        }


        val strongCarriers = hSearchBlue.getStrongCarriers(cell1, cell2)

        if(strongCarriers.nonEmpty){
          blueCircuit.addLink(node1.id, node2.id)

          blueCircuit.setResistance(node1, node2, (1f-(1f/strongCarriers.size))/3)
        }
      }
    }

    for(node1 <- redCircuit.getNodes){
      for(node2 <- redCircuit.getNodes){
        var cell1, cell2 : Cell = new Cell(-1,-1)

        if(node1.id == 0){
          cell1 = hSearchRed.boundaryRed1
        }
        else if(node1.id == model.N * model.N + 1){
          cell1 = hSearchRed.boundaryRed2
        }
        else{
          cell1 = model.board((node1.id-1) / model.N)((node1.id-1) % model.N)
        }
        if(node2.id == 0){
          cell2 = hSearchRed.boundaryRed1
        }
        else if(node2.id == model.N * model.N + 1){
          cell2 = hSearchRed.boundaryRed2
        }
        else{
          cell2 = model.board((node2.id-1) / model.N)((node2.id-1) % model.N)
        }

        val strongCarriers = hSearchRed.getStrongCarriers(cell1, cell2)
        if(strongCarriers.nonEmpty){
          redCircuit.addLink(node1.id, node2.id)
          redCircuit.setResistance(node1, node2, (1f-(1f/strongCarriers.size))/3)
        }
      }
    }
    val redsGo = model.myCells(R).size <= (model.myCells(B) ++ model.myCells(R)).size.toFloat / 2.0
    val myGo = (colour.equals(B) && !redsGo) || (colour.equals(R) && redsGo)
    if(myGo){
      for(node1 <- blueCircuit.getNodes){
        for(node2 <- blueCircuit.getNodes){
          var cell1, cell2 : Cell = new Cell(0,0)

          if(node1.id == 0){
            cell1 = hSearchBlue.boundaryBlue1
          }
          else if(node1.id == model.N * model.N + 1){
            cell1 = hSearchBlue.boundaryBlue2
          }
          else{
            cell1 = model.board((node1.id-1) / model.N)((node1.id-1) % model.N)
          }
          if(node2.id == 0){
            cell2 = hSearchBlue.boundaryBlue1
          }
          else if(node2.id == model.N * model.N + 1){
            cell2 = hSearchBlue.boundaryBlue2
          }
          else{
            cell2 = model.board((node2.id-1) / model.N)((node2.id-1) % model.N)
          }


          val weakCarriers = hSearchBlue.getWeakCarriers(cell1, cell2)

          if(weakCarriers.nonEmpty){
            blueCircuit.addLink(node1.id, node2.id)

            blueCircuit.setResistance(node1, node2, (1f-(1f/(weakCarriers.size+1)))/3)
          }
        }
      }

      for(node1 <- redCircuit.getNodes){
        for(node2 <- redCircuit.getNodes){
          var cell1, cell2 : Cell = new Cell(-1,-1)

          if(node1.id == 0){
            cell1 = hSearchRed.boundaryRed1
          }
          else if(node1.id == model.N * model.N + 1){
            cell1 = hSearchRed.boundaryRed2
          }
          else{
            cell1 = model.board((node1.id-1) / model.N)((node1.id-1) % model.N)
          }
          if(node2.id == 0){
            cell2 = hSearchRed.boundaryRed1
          }
          else if(node2.id == model.N * model.N + 1){
            cell2 = hSearchRed.boundaryRed2
          }
          else{
            cell2 = model.board((node2.id-1) / model.N)((node2.id-1) % model.N)
          }

          val weakCarriers = hSearchRed.getWeakCarriers(cell1, cell2)
          if(weakCarriers.nonEmpty){
            redCircuit.addLink(node1.id, node2.id)
            redCircuit.setResistance(node1, node2, (1f-(1f/(weakCarriers.size+1)))/3)
          }
        }
      }
    }


    val circuitSolver = new CircuitSolver

    val redResistance = circuitSolver.getResistance(redCircuit)
    val blueResistance = circuitSolver.getResistance(blueCircuit)
    //println("RED: " + redResistance)
    //println("BLUE: " + blueResistance)
    var result : Float = 0
    if (colour.equals(B)){

      result = Math.log(redResistance/blueResistance).toFloat
    }
    else{
      result = Math.log(blueResistance/redResistance).toFloat
    }




    return result
  }


}
