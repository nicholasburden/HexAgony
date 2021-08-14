package hexagony
import java.awt.BasicStroke
import java.awt.Dimension
import java.awt.Font
import java.awt.Graphics2D
import java.awt.Polygon
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import javax.swing.{BoxLayout, JButton, JCheckBox, JFrame, JLabel, JPanel, JSlider, JSplitPane, WindowConstants}
import javax.swing.event.ChangeEvent
import javax.swing.event.ChangeListener

class Frame extends JFrame with Const {
  
  val frame = this
  
  var N = 7
  var grid = new Grid(N)
  
  val panelwidth = 250
  val introheight = 50
  val boardwidth = 690
  val boardheight = 380
  val infoheight = 50
  val hexheight = boardwidth / 7.5
  val framewidth = 2 * panelwidth + boardwidth
  val frameheight = introheight + boardheight + infoheight
  val splitheight = introheight + boardheight
  
  frame.setSize(framewidth, frameheight + 40)
  frame.setTitle("HexAgony")
  frame.setResizable(false)
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setLocationRelativeTo(null)
  
    val split = new JSplitPane()
    split.setMinimumSize(new Dimension(framewidth, frameheight))
    split.setMaximumSize(new Dimension(framewidth, frameheight))
    split.setOrientation(JSplitPane.VERTICAL_SPLIT)
    split.setDividerSize(0)
    frame.add(split)
 
      val top = new JPanel()
      top.setLayout(new BoxLayout(top, BoxLayout.X_AXIS))
      top.setMinimumSize(new Dimension(framewidth, splitheight))
      top.setMaximumSize(new Dimension(framewidth, splitheight))
      split.setTopComponent(top)

        val human1 = new Human(frame, 0, R)
        
        val panel1 = new PlayerPanel(human1, panelwidth, splitheight)
        top.add(panel1)
 
        val body = new JPanel()
        body.setMaximumSize(new Dimension(boardwidth, splitheight))
        body.setMinimumSize(new Dimension(boardwidth, splitheight))
        body.setLayout(new BoxLayout(body, BoxLayout.Y_AXIS))
        top.add(body)
 
          val intro = new JPanel()
          intro.setMaximumSize(new Dimension(boardwidth, introheight))
          intro.setMaximumSize(new Dimension(boardwidth, introheight))
          body.add(intro)
          
            val lblSelect = new JLabel("Select board size: ")
            intro.add(lblSelect)
       
            val sldBoard = new JSlider(3, 15, N)
            sldBoard.setSnapToTicks(true)
            sldBoard.setMajorTickSpacing(2)
            sldBoard.setMinorTickSpacing(1)
            sldBoard.setPaintLabels(true)
            intro.add(sldBoard)
            
            val chkPie = new JCheckBox("Pie rule")
            chkPie.setSelected(true)
            intro.add(chkPie)
       
            val btnPlay = new JButton("Play!")
            intro.add(btnPlay)
            
          val board = new JPanel
          board.setMinimumSize(new Dimension(boardwidth, boardheight))
          board.setMaximumSize(new Dimension(boardwidth, boardheight))
          body.add(board)
          
        val human2 = new Human(frame, 0, B)
          
        val panel2 = new PlayerPanel(human2, panelwidth, splitheight)
        top.add(panel2)
       
      val bottom = new JPanel()
      bottom.setMinimumSize(new Dimension(framewidth, infoheight))
      bottom.setMaximumSize(new Dimension(framewidth, infoheight))
      split.setBottomComponent(bottom) 

        val lblInfo = new JLabel()
        lblInfo.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 30))
        lblInfo.setText("Welcome to Hex!")
        lblInfo.setOpaque(false)
        bottom.add(lblInfo)
        
        
  val slideevent = new ChangeListener() {
    def stateChanged(event: ChangeEvent) = {
      N = event.getSource.asInstanceOf[JSlider].getValue
      grid = new Grid(N)
      drawBoard(grid)
    }
  }    
  sldBoard.addChangeListener(slideevent)
  
  val clickResetGame = new ActionListener() { 
    def actionPerformed(event: ActionEvent): Unit = { resetGame() }
  }
  val clickStartGame = new ActionListener() { 
    def actionPerformed(event: ActionEvent): Unit = { startGame() }
  }
  btnPlay.addActionListener(clickStartGame)

         
  frame.setVisible(true)
  
  val g1 = board.getGraphics.asInstanceOf[Graphics2D]
          
  Thread.sleep(200); drawBoard(grid)  
  
  
// -----------------------------------------------------------------------------------  
// Methods used by the controller
  
  
  // Paint cell and set text according to given move
  //   boo is true if and only if the pie rule was played
  
  var lastMove: Move = null
  
  def playMove(move: Move, boo: Boolean) = {
    
    val (cell, colour) = move
    val hex = grid.grid(cell)
    if (!boo) {
      if (lastMove != null) fillHex(lastMove)
      setText(colour.name + " played " + cell.toString() + ". " + colour.othercolour + "'s turn", colour)
    }
    hex.colour = if (colour == R) RP else BP
    fillHex(hex, hex.colour)
    lastMove = move
    
  }
  
  
  // Respond to pie rule being played
  
  def playPieRule(boo: Boolean) = boo match {
    
    case true =>
      drawBoard(grid, true)
      setText("Blue played the pie rule. Red's turn", B)
    case false =>
      setText("Blue did not play the pie rule. Blue's turn", B)
    
  }
  
  
  // End game with given winner and winning path
  
  def endGame(winner: Player, sol: MaybePath) = {
    
    setText(winner.name + " wins! Play again?", winner.colour)
    toggleButtonPlay(false)
    drawPath(sol)
    
  }
  
  
  // Start/stop clock on player panel
  
  def time(colour: Colour, boo: Boolean) = colour match {
    
    case R => panel1.time(boo)
    case B => panel2.time(boo)
    case _ => 
      
  }
  
  
// -----------------------------------------------------------------------------------
// Private methods used by the GUI
  
  
  // Start the game (upon clicking Play)
  
  private def startGame(): Unit = {
    
    val pierule = chkPie.isSelected()
    panel1.getPlayer(N, pierule) match { 
      case Left(str) => setText(str, R) // Error with Red; do not start game
      case Right(red) =>
        panel2.getPlayer(N, pierule) match {
          case Left(str) => setText(str, B) // Error with Blue; do not start game
          case Right(blue) => // Both players valid; start game
            drawBoard(grid)
            toggleComponents(false)
            setText("Red to play first", R)
            val game = new Game(red, blue, frame, pierule)
            val thread = new Thread { override def run() = { game.startGame() } }
            thread.start()
        }
    }
    
  }
  
  
  // Paint board
  
  private def drawBoard(grid: Grid) { drawBoard(grid, false) }
  
  private def drawBoard(grid: Grid, pie: Boolean) = {
    
    // Constants used to define the board
    
    val side = (hexheight / N).toInt
    val sqs = (Math.sqrt(3) * side).toInt
    val mid = boardwidth / 2
    val centre = mid - (3 * N - 1) * side
    val border = 35 - N
    val gap = 30 - 5*N/4
    
    // Method for defining polygon
    
    def makePolygon(coord: Array[(Int, Int)]): Polygon =
      new Polygon(coord.map(_._1), coord.map(_._2), coord.length)
    
    // Method for defining geometry of a hexagon
    
    def makeCell(i: Int, j: Int, x: Int, y: Int): Hexagon = {
      val xpoints = Array[Int](side + x, 3 * side + x, 4 * side + x, 3 * side + x, side + x, x)
      val ypoints = Array[Int](y, y, sqs + y, 2 * sqs + y, 2 * sqs + y, sqs + y)
      new Hexagon(i, j, xpoints, ypoints)
    }
  
    // Populate grid with hexagons
    
    for (i <- 0 until N; j <- 0 until N) {
      grid.grid(i)(j) = makeCell(i, j, 
        3 * (N - i + j - 1) * side + centre,
        sqs * (i + j) + border)
    }
    
    // Clear board before repainting
    
    val rect = board.getBounds()
    g1.clearRect(0, 0, rect.getWidth.toInt, rect.getHeight.toInt)
    
    // Paint coloured background underneath board
    
    val north = (mid, border - gap)
    val west = (centre - 3 * gap, N * sqs + border)
    val south = (mid, 2 * N * sqs + border + gap)
    val east = ((6 * N - 2) * side + centre + 3 * gap, N * sqs + border)
    val middle = (mid, N * sqs + border)
    
    val topleft = makePolygon(Array(north, middle, west))
    val topright = makePolygon(Array(north, middle, east))
    val bottomleft = makePolygon(Array(south, middle, west))
    val bottomright = makePolygon(Array(south, middle, east))
    val diamond = makePolygon(Array(north, west, south, east))
    
    setColour(if (!pie) R else B)
    g1.fillPolygon(topleft)
    g1.fillPolygon(bottomright)
    setColour(if (!pie) B else R)
    g1.fillPolygon(topright)
    g1.fillPolygon(bottomleft)
    g1.setStroke(new BasicStroke(4))    
    setColour(K)
    g1.drawPolygon(diamond)
    
    // Paint hexagons
    
    for (row <- grid.grid; hex <- row) fillHex(hex, hex.colour)
    
  }
  
  
  // Set text in info box
  
  private def setText(str: String, colour: Colour) = {
    
    lblInfo.setForeground(colour.colour)
    lblInfo.setText(str) 
    
  }
  
  
  // Set paint colour of the graphics
  
  private def setColour(colour: Colour) = { g1.setColor(colour.colour) }
  
  
  // Paint hexagon with given colour
  
  private def fillHex(hex: Hexagon, colour: Colour) = {
    
    setColour(colour)
    g1.fillPolygon(hex)
    setColour(K)
    g1.drawPolygon(hex)
    
  }
  
  private def fillHex(move: Move) { fillHex(grid.grid(move._1), move._2) }
  
  
  // Draw winning path on board
  
  private def drawPath(sol: MaybePath) = sol match {
    
      case Some(path) =>
        val colour = if (path.head.colour == R) RP else BP
        for (cell <- path) fillHex(grid.grid(cell), colour)
      case None =>
    
  }
  
  
  // Toggle various components to be enabled/disabled
  
  private def toggleComponents(boo: Boolean) = {
    
    btnPlay.setEnabled(boo)
    sldBoard.setEnabled(boo)
    chkPie.setEnabled(boo)
    panel1.toggleComponents(boo)
    panel2.toggleComponents(boo)
    
  }
  
  
  // Toggle button between Play and Reset
  
  private def toggleButtonPlay(boo: Boolean) = boo match {
    
    case true =>
      btnPlay.removeActionListener(clickResetGame)
      btnPlay.addActionListener(clickStartGame)
      btnPlay.setText("Play!")
      btnPlay.setEnabled(true)
    case false =>
      btnPlay.removeActionListener(clickStartGame)
      btnPlay.addActionListener(clickResetGame)
      btnPlay.setText("Reset")
      btnPlay.setEnabled(true)
    
  }
  
  
  // Reset game
  
  private def resetGame(): Unit = {
    
    setText("Welcome to Hex!", K)
    lastMove = null
    toggleButtonPlay(true)
    toggleComponents(true)
    grid = new Grid(N)
    drawBoard(grid)
    
  }
  
}