package hexagony

import java.awt.Dimension
import java.awt.Font
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.io.File

import javax.swing.ButtonGroup
import javax.swing.JButton
import javax.swing.JFileChooser
import javax.swing.JLabel
import javax.swing.JPanel
import javax.swing.JRadioButton
import javax.swing.JTextField
import javax.swing.Timer

class PlayerPanel(human: Player, width: Int, height: Int) extends JPanel with Const {
  
  val panel = this
  val colour = human.colour
  var timelimit: Long = 0
  var file: File = null
  
  val compiler = new Compiler
  
  panel.setMinimumSize(new Dimension(width, height))
  panel.setMaximumSize(new Dimension(width, height))
  
  val lblName = new JLabel()
  lblName.setFont(new Font(Font.SANS_SERIF, Font.BOLD, width / 3))
  lblName.setForeground(colour.colour)
  lblName.setText(colour.name)
  panel.add(lblName)
  
  val lblChoose = new JLabel()
  lblChoose.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 20))
  lblChoose.setText("Choose your player:")
  panel.add(lblChoose)

  val radHuman = new JRadioButton()
  radHuman.setText("Human")
  radHuman.addActionListener(new ActionListener() {
    def actionPerformed(event: ActionEvent) = { toggleRobotComponents(false) }
  })
  panel.add(radHuman)
  
  val radRobot = new JRadioButton()
  radRobot.setText("Robot")
  radRobot.addActionListener(new ActionListener() {
    def actionPerformed(event: ActionEvent) = { toggleRobotComponents(true) }
  })
  panel.add(radRobot)
  
  val grpRadio = new ButtonGroup()
  grpRadio.add(radHuman)
  grpRadio.add(radRobot)
  
  val txtRobot = new JTextField(10)
  txtRobot.setEditable(false)
  panel.add(txtRobot)
  
  val fchRobot = new ScalaFileChooser()  
  val btnChoose = new JButton()
  btnChoose.setText("Browse...")
  btnChoose.addActionListener(new ActionListener() {
    def actionPerformed(event: ActionEvent) = {
      if (fchRobot.showOpenDialog(panel) == JFileChooser.APPROVE_OPTION) {
        file = fchRobot.getSelectedFile()
        txtRobot.setText(fchRobot.name(file))
      }
    }
  })
  panel.add(btnChoose)
  
  val lblTime = new JLabel()
  lblTime.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 20))
  lblTime.setText("Time limit (secs):")
  panel.add(lblTime)
  
  val txtTime = new JTextField(4)
  txtTime.setEditable(true)
  txtTime.setText("0.5")
  panel.add(txtTime)
  
  val lblClock = new JLabel()
  lblClock.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, width / 2))
  lblClock.setForeground(colour.colour)
  panel.add(lblClock)
  
  var countdown = 0
  val timerTask = new ActionListener() {
    def actionPerformed(e: ActionEvent) = {
      if (countdown > 0) { countdown -= 1; setClock(countdown) }
    }
  }
  val timer = new Timer(1000, timerTask)
  
  radHuman.setSelected(true)
  toggleRobotComponents(false)
  
  
// -------------------------------------------------------------------------------------------
// Methods called by the main frame
  
  
  def getPlayer(N: Int, pierule: Boolean): Either[String, Player] = radHuman.isSelected() match {
    
    case true => Right(human)
    case false =>
      try {
        timelimit = (txtTime.getText.toDouble * 1000).toLong
        compiler.getRobot(file, new Model(N), timelimit, pierule, colour)
      } catch {
        case ex: NumberFormatException => Left("Time limit must be a number")
        case ex: Exception => println(ex.toString()); Left("Invalid robot")
      }
    
  }
  
  
  def time(boo: Boolean) = boo match {
    
    case true =>
      countdown = timelimit.toInt / 1000
      setClock(countdown)
      timer.start()
    case false =>
      timer.stop()
      lblClock.setText("")
    
  }
  
  
  def toggleComponents(boo: Boolean) = {
    
    radHuman.setEnabled(boo)
    radRobot.setEnabled(boo)
    btnChoose.setEnabled(boo)
    txtTime.setEnabled(boo)
    
  }
  
  
// -----------------------------------------------------------------------------------------
// Private methods
  
  
  private def setClock(n: Int) = { 
    
    lblClock.setFont(new Font(Font.SANS_SERIF, Font.BOLD, (width / ((Math.log10(n)).toInt + 2))))
    lblClock.setText(n.toString())
    
  }
  
  
  private def toggleRobotComponents(boo: Boolean) = {
    
    txtRobot.setVisible(boo)
    btnChoose.setVisible(boo)
    lblTime.setVisible(boo)
    txtTime.setVisible(boo)
    lblClock.setVisible(boo)
    
  }
  
}