package pierule

class PieRule(size : Int) {
  //Winning first move for initial red: table(i)(j)
  //Winning move for initial blue: table(j)(i)


  //These are tables for deciding on whether to play the pie rule
  val table : Array[Array[Boolean]] = size match{

    case 3 => Array(Array(true, true, false),
                    Array(false, true, false),
                    Array(false, true, true))

    case 4 => Array(Array(true, false, false, false),
                    Array(false, true, false, false),
                    Array(false, false, true, false),
                    Array(false, false, false, true))

    case 5 => Array(Array(true, true, false, false, false),
                    Array(false, true, true, true, false),
                    Array(false, true, true, true, false),
                    Array(false, true, true, true, false),
                    Array(false, false, false, true, true))

    case 6 => Array(Array(false, false, false, false, false, false),
                    Array(false, true, true, true, true, false),
                    Array(false, true, true, true, true, false),
                    Array(false, true, true, true, true, false),
                    Array(false, true, true, true, true, false),
                    Array(false, false, false, false, false, false))

    case 7 => Array(Array(false, false, false, false, false, false, false),
                    Array(false, false, false, false, false, false, false),
                    Array(false, false, true, true, true, false, false),
                    Array(false, false, true, true, true, false, false),
                    Array(false, false, true, true, true, false, false),
                    Array(false, false, false, false, false, false, false),
                    Array(false, false, false, false, false, false, false))

    case 8 => Array(Array(false, false, false, false, false, false, false, false),
                    Array(false, false, false, false, false, false, false, false),
                    Array(false, false, true, true, true, true, false, false),
                    Array(false, false, true, true, true, true, false, false),
                    Array(false, false, true, true, true, true, false, false),
                    Array(false, false, true, true, true, true, false, false),
                    Array(false, false, false, false, false, false, false, false),
                    Array(false, false, false, false, false, false, false, false))

    case 9 => Array(Array(false, false, false, false, false, false, false, false, false),
                    Array(false, false, false, false, false, false, false, false, false),
                    Array(false, false, true, true, true, true, true, false, false),
                    Array(false, false, true, true, true, true, true, false, false),
                    Array(false, false, true, true, true, true, true, false, false),
                    Array(false, false, true, true, true, true, true, false, false),
                    Array(false, false, true, true, true, true, true, false, false),
                    Array(false, false, false, false, false, false, false, false, false),
                    Array(false, false, false, false, false, false, false, false, false))

    case 10 =>Array(Array(false, false, false, false, false, false, false, false, false, false),
                    Array(false, false, false, false, false, false, false, false, false, false),
                    Array(false, false, false, false, false, false, false, false, false, false),
                    Array(false, false, false, true, true, true, true, false, false, false),
                    Array(false, false, false, true, true, true, true, false, false, false),
                    Array(false, false, false, true, true, true, true, false, false, false),
                    Array(false, false, false, true, true, true, true, false, false, false),
                    Array(false, false, false, false, false, false, false, false, false, false),
                    Array(false, false, false, true, false, false, false, false, false, false),
                    Array(false, false, false, false, false, false, false, false, false, false))

    case 11 =>Array(Array(false, false, false, false, false, false, false, false, false, false, false),
                    Array(false, false, false, false, false, false, false, false, false, false, false),
                    Array(false, false, false, false, false, false, false, false, false, false, false),
                    Array(false, false, false, true, true, true, true, true, false, false, false),
                    Array(false, false, false, true, true, true, true, true, false, false, false),
                    Array(false, false, false, true, true, true, true, true, false, false, false),
                    Array(false, false, false, true, true, true, true, true, false, false, false),
                    Array(false, false, false, true, true, true, true, true, false, false, false),
                    Array(false, false, false, false, false, false, false, false, false, false, false),
                    Array(false, false, false, false, false, false, false, false, false, false, false),
                    Array(false, false, false, false, false, false, false, false, false, false, false))
    case _ => Array()
  }
  def getTable : Array[Array[Boolean]] = table
}
