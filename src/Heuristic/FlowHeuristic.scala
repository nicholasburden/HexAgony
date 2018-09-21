package Heuristic

import Graph._
import hexagony._
class FlowHeuristic extends Heuristic with Const{

  def evaluate(model : Model, colour : Colour) : Int = {

    val graph = new HexGraph(model.N, colour)
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
