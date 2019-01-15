package heuristic

import graph._
import hexagony._
class FlowHeuristic extends Heuristic with Const{

  def evaluate(model : Model, colour : Colour) : Int = {
    val pie = model.pie
    val othercolour = colour match{
      case R => B
      case B => R
      }

    val c = pie match{
      case true => othercolour
      case false => colour
    }
    val graph = new HexGraph(model.N, colour, model.pie)
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

    //Calculate network flow from one boundary to another
    val networkFlow = new NetworkFlow(graph)
    return networkFlow.maxFlow

  }
}
