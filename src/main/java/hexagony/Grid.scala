package hexagony

class Grid(N: Int) extends Const {
  
  val grid = Array.ofDim[Hexagon](N, N)
  
  def grid(cell: Cell): Hexagon = grid(cell.i)(cell.j)
  
}