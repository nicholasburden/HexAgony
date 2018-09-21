package Heuristic

import hexagony.{Colour, Model}

abstract class Heuristic {
  def evaluate(model : Model, colour : Colour) : Int
}
