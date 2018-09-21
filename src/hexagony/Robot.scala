package hexagony

abstract class Robot(model: Model, val timelimit: Long, pierule: Boolean, colour: Colour)
  extends Player(colour: Colour) {   
  
  val board = model.board
  val N = model.N
  var count = model.count
  val othercolour = if (colour == R) B else R
  
  var name = "Robot" + colour.name
  
  var lastCell: Cell = null
  var piePlayed = false
  
  def update(move: Move) = { model.playMove(move); lastCell = move._1 }
  
  def playPieRule() { model.playPieRule(); piePlayed = true }
  
}