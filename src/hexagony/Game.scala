package hexagony

import java.io.BufferedWriter
import java.io.FileWriter
import java.text.DecimalFormat
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.TimeoutException
import java.io.File

class Game(Red: Player, Blue: Player, frame: Frame, pierule: Boolean) extends Const {
  
  val N = frame.N
  val model = new Model(N)
  var player: Player = Red
  var start: Long = 0
  
  // Start the game; called by the frame. This process runs for the entire game
                            
  def startGame() {
    
    var fine = true
    var done = false
    var lastmove: Cell = null
    var count = 0
    
    start = System.currentTimeMillis()
    
    log(new SimpleDateFormat("yyyy.MM.dd HH:mm:ss").format(new Date()))
    val str1 = Red.name + " (" + (if (Red.timelimit > 0) (Red.timelimit + "ms") else "Human") + ")"
    val str2 = Blue.name + " (" + (if (Blue.timelimit > 0) (Blue.timelimit + "ms") else "Human") + ")"
    log(N + " x " + N + " game: " + str1 + " vs " + str2)
    log("Pie rule is " + (if (pierule) "" else "not ") + "available")
    
    while (fine && !done) {
      
      // Request move from player
      val move = requestMove()
      val time = System.currentTimeMillis()
      move match {
        case None =>
          // Player failed to select a move
          log(time - start, player.name + " failed to select a move on move " + (count + 1))
          endGameBad()
          fine = false
        case Some(m) =>
          lastmove = m
          if (!model.legal(m)) {
            // Player selected an invalid move
            log(time - start, player.name + " attempted the illegal move " + m.toString() + " on move " + (count + 1))
            endGameBad()
            fine = false
          }  
      }
      
      // Proceed if a valid move has been selected
      if (fine) {
        
        log(time - start, player.name + " plays move " + (count + 1) + ": " + lastmove.toString())
        playMove(lastmove)
        count += 1
        
        // Check for a solution if enough moves have been made
        if (count >= 2 * N - 1) {
          val sol = model.checkSol(player.colour)
          sol match {
            case Some(path) =>
              // Solution exists
              done = true
              endGame(player, sol)
            case None => 
          }
        }
        
        // No solution; switch player
        if (!done) switchPlayer()
        
        // Request pie from Blue after first move if the pie rule is available
        if (count == 1 && pierule) {
          val pie = requestPie(lastmove)
          val time = System.currentTimeMillis()
          pie match {
            case None =>
              log(time - start, player.name + " timed out on pie rule")
              endGameBad()
              fine = false
            case Some(true) =>
              log(time - start, player.name + " plays pie rule")
              playPie(lastmove, true)
            case Some(false) =>
              log(time - start, player.name + " does not play pie rule")
              playPie(lastmove, false)
          }
        }
        
      }
      
    }
    
  }  
  
  
  // Request move from the active player
  
  private def requestMove(): Option[Cell] = {
    
    try {
      frame.time(player.colour, true)
      val move = timedRun[Cell](player.timelimit)(player.makeMove())
      if (move != null) Some(move) else None
    } catch {
      case ex: Exception => log(System.currentTimeMillis() - start, ex.toString()); None
    } finally {
      frame.time(player.colour, false)
    }
    
  }  
  
  
  // Request pie from Blue
  
  private def requestPie(firstmove: Cell): Option[Boolean] = {
    
    try {
      frame.time(player.colour, true)
      Some(timedRun[Boolean](player.timelimit)(player.pieRule(firstmove)))
    } catch {
      case ex: Exception => log(System.currentTimeMillis() - start, ex.toString()); None
    } finally {
      frame.time(player.colour, false)
    }
    
    
  }
  
  
  // Update the model, frame and players after a move
  
  private def playMove(cell: Cell) { playMove(cell, false) }
                                        
  private def playMove(cell: Cell, boo: Boolean) {
    
    val move = (cell, player.colour)
    model.playMove(move)
    frame.playMove(move, boo)
    Red.update(move)
    Blue.update(move)
    
  }  
  
  
  // Update the model, frame and players after the pie rule
  
  private def playPie(firstmove: Cell, boo: Boolean) = boo match {
    
    case true =>
      frame.playPieRule(true)
      playMove(firstmove, true)
      model.playPieRule()
      Red.playPieRule()
      Blue.playPieRule()
      switchPlayer()
    case false =>
      frame.playPieRule(false)
    
  } 
  
  
  // Switch the active player
              
  private def switchPlayer() { player = if (player == Red) Blue else Red }  
  
  
  // End the game
    
  private def endGameBad() { switchPlayer(); endGame(player, None) }
              
  private def endGame(winner: Player, sol: MaybePath) {

    val time = System.currentTimeMillis()
    log(time - start, "End game - winner is " + winner.name + "\n")
    frame.endGame(winner, sol)
    logger.close()
    
  }
  
  
  // Logger used to log the game to the file hexlog.txt
  new File("hexlog.txt").createNewFile() // ensure the file exists
  val logger = new BufferedWriter(new FileWriter("hexlog.txt", true))
  val df = new DecimalFormat("0.000")
  
  private def log(time: Long, str: String) { log(df.format(time.toFloat / 1000) + ": (" + player.colour.name + ") " + str) }
  
  private def log(str: String) { println(str); logger.write(str + "\n") }
                
}