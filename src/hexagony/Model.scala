package hexagony

class Model(val N: Int) extends Const {
  
  val board = Array.ofDim[Cell](N, N)
  var count = 0
  var pie = false
  
  reset()
  
  def reset() { for (i <- 0 until N; j <- 0 until N) { board(i)(j) = new Cell(i, j); board(i)(j).colour = O } }
  
  def playMove(move: Move) {
    
    val (cell, colour) = move
    board(cell).colour = colour
    count += 1
    
  }
  
  def playPieRule() { count -= 1; pie = true }
  
  def playPieRule(firstmove: Cell) {
    
    count = 1
    pie = true
    board(firstmove).colour = B
    
  }
  
  def copy(): Model = {
    
    val model = new Model(N)
    model.pie = pie
    for (cell <- myCells(R)) model.playMove(cell, R)
    for (cell <- myCells(B)) model.playMove(cell, B)
    model
    
    
  }
  
  def checkSol(colour: Colour): MaybePath = {
    
    var init: Set[Cell] = Set.empty
    var goal: Set[Cell] = Set.empty
    
    if ((colour == R && !pie) || (colour == B && pie)) { 
      // Check for solution from left to right 
      for (i <- 0 until N) { init += board(i)(0); goal += board(i)(N - 1) }
    } else {   
      // Check for solution from top to bottom
      for (j <- 0 until N) { init += board(0)(j); goal += board(N - 1)(j) }
    }
    search(init, goal, colour)
    
  }
  
  def solution(colour: Colour): Boolean = checkSol(colour) != None
  
  def solution(): Boolean = checkSol(R) != None || checkSol(B) != None
  
  def valid(cell: Cell): Boolean = !(cell == null || cell.i < 0 || cell.i >= N || cell.j < 0 || cell.j >= N) 
  
  def emptyCell(cell: Cell): Boolean = board(cell).colour == O
  
  def legal(cell: Cell): Boolean = valid(cell) && emptyCell(cell)
  
  def board(cell: Cell): Cell = board(cell.i)(cell.j)
  
  def colour(cell: Cell): Colour = cell.colour
  
  def myEdges(colour: Colour): List[Cell] = {
    
    var list = List[Cell]()
    if ((colour == R && !pie) || (colour == B && pie)) {
      for (i <- N - 1 to 0 by -1) { list ::= board(i)(0); list ::= board(i)(N - 1) }
    } else {
      for (j <- N - 1 to 0 by -1) { list ::= board(0)(j); list ::= board(N - 1)(j) }
    }
    list
    
  }
  
  def myCells(colour: Colour): List[Cell] = {
    
    var list = List[Cell]()
    for (i <- N - 1 to 0 by -1; j <- N - 1 to 0 by -1) {
      val cell = board(i)(j)
      if (cell.colour == colour) list ::= cell
    }
    list
    
  }
  
  def neighbours(cell: Cell): List[Cell] = valid(cell) match {
    
    case false => List[Cell]()
    case true =>
      val (i, j) = (cell.i, cell.j)
      var list = List[Cell]()
      if (i > 0) list ::= board(i - 1)(j)
      if (j > 0) list ::= board(i)(j - 1)
      if (i < N - 1) list ::= board(i + 1)(j)
      if (j < N - 1) list ::= board(i)(j + 1)
      if (i > 0 && j > 0) list ::= board(i - 1)(j - 1)
      if (i < N - 1 && j < N - 1) list ::= board(i + 1)(j + 1)
      list
    
  }
  
  private def search(init: Set[Cell], goal: Set[Cell], colour: Colour): MaybePath = {
    
    var queue = collection.mutable.Queue[Cell]()
    var visited = Set[Cell]()
    var parent = collection.mutable.Map[Cell, Cell]()
    for (cell <- init) if (cell.colour == colour) queue += cell
    
    // Standard breadth-first graph search
    while (!queue.isEmpty) {
      val cell = queue.head
      queue = queue.drop(1)
      visited += cell
      if (goal.contains(cell)) {
        // Solution has been found
        var path = List[Cell]()
        var cur = cell
        while (!init.contains(cur)) { path ::= cur; cur = parent(cur) }
        path ::= cur
        return Some(path)
      } else {
        // Solution has not been found
        for (c <- neighbours(cell)) {
          if (c.colour == colour && !queue.contains(c) && !visited.contains(c)) {
            queue += c; parent(c) = cell
          }
        }
      }
    }
    // No solution exists
    None
    
  }



  def checkIfFinished() : Int = {
    if(solution(R)) return 0
    if(solution(B)) return 1
    return -1
  }
  
}

