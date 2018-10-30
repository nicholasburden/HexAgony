package pierule

class PieRule(size : Int) {
  //Winning first move for initial red: table(i)(j)
  //Winning move for initial blue: table(j)(i)
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
    case _ => Array()
  }
  def getTable : Array[Array[Boolean]] = table
}
