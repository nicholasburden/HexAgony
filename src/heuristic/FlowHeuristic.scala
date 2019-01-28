package heuristic

import graph._
import hexagony._
class FlowHeuristic extends Const{

  def evaluate(model : Model, colour : Colour, graphRed : HexGraph, graphBlue : HexGraph) : Double = {
    val pie = model.pie
    val othercolour = colour match{
      case R => B
      case B => R
      }

    /*
    val graph = new HexGraph(model.N, colour)//, model.pie)
    val board = model.board

    //Place pieces on circuit to alter pipes
    for(i <- board.indices){
      for(j <- board(0).indices){
        if(colour == B){
          if(board(i)(j).colour == B){
            graph.placeYours(i+1, j+1)
          }
          else if(board(i)(j).colour == R){

            graph.placeTheirs(i+1, j+1)
          }
        }
        else{
          if(board(i)(j).colour == R){
            graph.placeYours(model.N-j, i + 1)
          }
          else if(board(i)(j).colour == B){
            graph.placeTheirs(model.N-j, i + 1)
          }
        }

      }
    }
    */
    //Calculate network flow from one boundary to another
    val networkFlowRed = new NetworkFlow(graphRed); val networkFlowBlue = new NetworkFlow(graphBlue)
    val x : Double = networkFlowRed.maxFlow
    val y : Double= networkFlowBlue.maxFlow

    if(colour.equals(R)) return Math.log((x+1)/(y+1))
    else return Math.log((y+1)/(x+1))


  }
}
