package heuristic

import hexagony.{Colour, Model}

abstract class Heuristic {
  def evaluate(model : Model, colour : Colour, pie : Boolean) : Int
}
