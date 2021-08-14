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

    //Calculate network flow from one boundary to another
    val networkFlowRed = new NetworkFlow(graphRed); val networkFlowBlue = new NetworkFlow(graphBlue)
    val x : Double = networkFlowRed.maxFlow
    val y : Double= networkFlowBlue.maxFlow

    if(colour.equals(R)) return Math.log((x+1)/(y+1))
    else return Math.log((y+1)/(x+1))


  }
}
