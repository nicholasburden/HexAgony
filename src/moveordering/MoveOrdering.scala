package moveordering

import hexagony._

class MoveOrdering extends Const {

  //Maintains a set of moves already played
  var currentMoves: Set[Cell] = Set()

  //Maintains a set of promising moves (ie forms a 2 bridge with another piece of smae colour)
  var goodMoves: Set[Cell] = Set[Cell]()

  //Initialise the move ordering
  def initial(model: Model) = {
    for (move <- model.myCells(R) ++ model.myCells(B)) {
      //Initialise all current moves
      currentMoves = currentMoves + move

      //Considers all moves that forms a bridge or is a neighbour
      for (goodMove <- getBridges(move, model) ++ model.neighbours(move)) {
        if (goodMove.colour == O) goodMoves = goodMoves + goodMove

      }
    }
  }

  def getOrdering(model: Model): List[Cell] = {
    var ordering: List[Cell] = List()

    //If first move, then order moves from inside to outside of board
    if (model.count == 0) {
      for (layer <- 0 until model.N / 2) {
        for (i <- layer until (model.N - layer)) {
          ordering = model.board(i)(model.N - layer - 1) :: model.board(i)(layer) :: ordering

        }
        for (j <- layer + 1 until (model.N - layer - 1)) {
          ordering = model.board(model.N - layer - 1)(j) :: model.board(layer)(j) :: ordering
        }
      }
      if (model.N % 2 != 0) {
        ordering = model.board(model.N / 2)(model.N / 2) :: ordering
      }
    }
    else {
      //Not the first go
      for (cell <- model.myCells(O)) {
        //Place moves not in goodMoves set at the back of the list
        if (!goodMoves.contains(cell) && !currentMoves.contains(cell)) {
          ordering = cell :: ordering
        }
      }
      //Add goodMoves set to start of list (higher priority)
      return goodMoves.toList ++ ordering
    }

    ordering
  }

  def addGoodMove(cell: Cell) = goodMoves = goodMoves + cell

  def addMovesFor(start: Cell, model: Model): MoveOrdering = {
    //Clones the object for a new board, adding good moves for a specific move

    //Initialise cloned object
    val result = new MoveOrdering
    result.goodMoves = goodMoves
    result.currentMoves = currentMoves + start

    //Add moves which form a bridge with the new move
    getBridges(start, model).foreach(x => result.addGoodMove(model.board(x.i)(x.j)))

    //Remove any moves already made
    result.goodMoves = result.goodMoves -- result.currentMoves
    result
  }

  def isValid(i: Int, j: Int, model: Model): Boolean = {
    return i >= 0 && i < model.N && j >= 0 && j < model.N && model.board(i)(j).colour == O
  }

  def getBridges(start: Cell, model: Model): List[Cell] = {
    //Returns a list of all moves that form a 2-bridge with the start cell
    var list: List[Cell] = List()
    if (isValid(start.i - 1, start.j - 2, model)) {
      list = model.board(start.i - 1)(start.j - 2) :: list
    }
    if (isValid(start.i - 2, start.j - 1, model)) {
      list = model.board(start.i - 2)(start.j - 1) :: list
    }
    if (isValid(start.i - 1, start.j + 1, model)) {
      list = model.board(start.i - 1)(start.j + 1) :: list
    }
    if (isValid(start.i + 1, start.j + 2, model)) {
      list = model.board(start.i + 1)(start.j + 2) :: list
    }
    if (isValid(start.i + 2, start.j + 1, model)) {
      list = model.board(start.i + 2)(start.j + 1) :: list
    }
    if (isValid(start.i + 1, start.j - 1, model)) {
      list = model.board(start.i + 1)(start.j - 1) :: list
    }
    list
  }


}
