package heuristic

import graph._
import hexagony._
class FlowHeuristic extends Heuristic with Const{

  def evaluate(model : Model, colour : Colour, pie : Boolean) : Int = {
    val othercolour = colour match{
      case R => B
      case B => R
      }

    val c = pie match{
      case true => othercolour
      case false => colour
    }
    val graph = new HexGraph(model.N, c)
    val board = model.board
    for(i <- 0 until board.length){
      for(j <- 0 until board(0).length){
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


    val networkFlow = new NetworkFlow(graph)
    return networkFlow.maxFlow

  }
}
