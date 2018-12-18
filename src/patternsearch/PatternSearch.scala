package patternsearch

import heuristic.FlowHeuristic
import hexagony._

class PatternSearch extends Const{
  def patternSearchAlphaBeta(mod : Model, colour : Colour, alpha : Float, beta : Float, depth : Int) : Pattern = {
    val p = new Pattern(mod.N)
    val best = Float.NegativeInfinity
    val othercolour = colour match{
      case R => B
      case B => R
    }
    if(mod.solution(colour)){
      p.value = Pattern.WIN
      return p
    }
    if(depth == 0){
      val heuristic = new FlowHeuristic
      p.value = heuristic.evaluate(mod, colour)
      return p
    }
    var moveList : List[Cell] = mod.myCells(O)
    p.value = Pattern.LOSS
    for(move <- moveList){
      val mod2 = mod.copy()
      mod2.playMove(move, colour)
      val pTemp = patternSearchAlphaBeta(mod2, othercolour, -beta, -Math.max(alpha, best).asInstanceOf[Float], depth-1)
      if(pTemp.value == Pattern.LOSS){
        p.value = Pattern.WIN
        p.pattern = pTemp.pattern + move
        return p
      }
      else if(pTemp.value == Pattern.WIN) {
        p.pattern = p.pattern.union(pTemp.pattern)
        moveList = moveList.toSet.intersect(pTemp.pattern).toList
      }
      p.value = Math.max(p.value, -pTemp.value)
      if(p.value >= beta){
        return p
      }

    }

    p

  }


}
