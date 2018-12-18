package patternsearch

import hexagony.Cell

class Pattern(N : Int) {
  var value : Float = -1 //initialise
  var pattern : scala.collection.immutable.Set[Cell] = Set[Cell]()

}
object Pattern{
  final val WIN : Float = Float.PositiveInfinity
  final val LOSS : Float = Float.NegativeInfinity

}
