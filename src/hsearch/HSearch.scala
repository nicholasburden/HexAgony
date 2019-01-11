package hsearch

import hexagony._

class HSearch(var model: Model, var colour: Colour) extends Const {

  //a set to store all cells that belong to a strong connection's carrier
  var strong: Set[Cell] = Set()
  var boundarySet: Set[Cell] = colour match {
    case R => Set(HSearch.boundaryRed1, HSearch.boundaryRed2)
    case B => Set(HSearch.boundaryBlue1, HSearch.boundaryBlue2)
    case _ => Set()
  }


  //initialise set to store all cells (apart from opponents colour)
  var Gtemp: Set[Cell] = Set()

  //G is forest of disjoint sets (to allow connected cells to be treated as one)
  var G: DisjointSets[Cell] = new DisjointSets[Cell]()

  //C stores strong carriers (maps pairs of cells to a set of carriers)
  var C: collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]] = collection.mutable.Map()
  //SC stores weak carriers (maps pairs of cells to a set of carriers)
  var SC: collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]] = collection.mutable.Map()
  //oldC stores last rounds C
  var oldC: collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]] = collection.mutable.Map()


  def initial = {
    //Initialise G through Gtemp
    //Add all cells apart from opponent's tiles
    for (cell <- model.myCells(colour)) {
      Gtemp = Gtemp + cell
    }
    for (cell <- model.myCells(O)) {
      Gtemp = Gtemp + cell
    }
    if (colour.equals(B)) {
      Gtemp = Gtemp + HSearch.boundaryBlue1
      Gtemp = Gtemp + HSearch.boundaryBlue2
    }
    else {
      Gtemp = Gtemp + HSearch.boundaryRed1
      Gtemp = Gtemp + HSearch.boundaryRed2
    }
    for (g <- Gtemp) {
      G.add(g)
    }

    for (g1 <- Gtemp) {

      for (g2 <- Gtemp) {
        //Union cells that are neighbours of same colour, so they can be treated as a single cell
        if (g1.colour.equals(colour) && g2.colour.equals(colour) && areNearestNeighbours(g1, g2)) {

          G.union(g1, g2)
        }
      }
    }
    //Initilaise oldC
    for (g1 <- G.getReps) {
      for (g2 <- G.getReps) {
        oldC((g1, g2)) = Set()
        oldC((g2, g1)) = Set()
      }
    }

    //Initialise C to:
    /*    if c1 and c2 are neighbours, then C(c1, c2) is the set containing the empty set
          otherwise, C(c1, c2) is the empty set
    */
    //Initialise SC to:
    /*    each C(c1, c2) is the empty set
    */
    for (g1 <- Gtemp) {
      for (g2 <- Gtemp) {
        val rep1 = G.find(g1).get
        val rep2 = G.find(g2).get
        if (!areNearestNeighbours(g1, g2)) {
          C((rep1, rep2)) = Set()
          C((rep2, rep1)) = Set()
        }
        else {
          C((rep1, rep2)) = Set(Set())
          C((rep2, rep1)) = Set(Set())
        }
        SC((rep1, rep2)) = Set()
        SC((rep2, rep1)) = Set()



      }

    }

  }


  def makeMove(i: Int, j: Int, c: Colour): HSearch = {
    //When a move is made, we consider what happens to the strong/weak connections
    //Make a clone of the HSearch object to allow for easy recursion in minimax
    val cell = model.board(i)(j)
    //Make move
    val mod2 = result(model, cell, c)
    //Initialise new H-Search object
    val hsearch = new HSearch(mod2, colour)
    //Clone disjoint sets (to maintain structure of neighbouring cells)
    val newG = G.clone()
    hsearch.G = newG.asInstanceOf[DisjointSets[Cell]]

    //Update colour
    if (colour.equals(c)) hsearch.G.add(mod2.board(cell.i)(cell.j))
    else hsearch.G.remove(mod2.board(cell.i)(cell.j))
    val C_clone = C.clone()
    val SC_clone = SC.clone()


    //Clone existing strong/weak connections
    hsearch.C = C_clone
    hsearch.SC = SC_clone


    //Initialise oldC for new object
    for (g1 <- hsearch.G.getReps) {
      for (g2 <- hsearch.G.getReps) {
        hsearch.oldC((g1, g2)) = Set()
      }
    }


    //done maps pairs of cells to a boolean value (if they have been considered yet, to avoid double consideration)
    val done = collection.mutable.Map[(Cell, Cell), Boolean]().withDefaultValue(false)
    for (cell1 <- mod2.myCells(colour) ++ mod2.myCells(O) ++ boundarySet) {
      for (cell2 <- mod2.myCells(colour) ++ mod2.myCells(O) ++ boundarySet) {

        if (!done((cell1, cell2))) {
          val strongCarriers = getStrongCarriers(cell1, cell2, true)
          val weakCarriers = getWeakCarriers(cell1, cell2, true)

          if (!c.equals(colour) && weakCarriers.contains(cell)) {
            //Case: opponent moves into a weak carrier of the player
            //In this case we can no longer trust that the weak connection is still a weak connection, so we remove the connection
            hsearch.SC((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set()
            //other way
            hsearch.SC((hsearch.G.find(cell2).get, hsearch.G.find(cell1).get)) = Set()

          }
          else if (c.equals(colour) && weakCarriers.contains(cell)) {
            //Case: player plays in one of its own weak connections
            //We make the (not necessarily correct) assumption that the moved was played in correct cell to preserve its connection
            hsearch.C((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set(weakCarriers - cell)
            hsearch.C((hsearch.G.find(cell2).get, hsearch.G.find(cell1).get)) = Set(weakCarriers - cell)


            //Remove weak connection
            hsearch.SC((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set()
            hsearch.SC((hsearch.G.find(cell2).get, hsearch.G.find(cell1).get)) = Set()


          }
          else if (!c.equals(colour) && strongCarriers.contains(cell)) {
            //Case: opponent plays in a carrier of one of the player's strong carriers
            //This connection now becomes weak, so we remove it from C, and add the connection to SC
            hsearch.SC((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set(strongCarriers - cell)
            hsearch.C((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set()

            //other way
            hsearch.SC((hsearch.G.find(cell2).get, hsearch.G.find(cell1).get)) = Set(strongCarriers - cell)
            hsearch.C((hsearch.G.find(cell2).get, hsearch.G.find(cell1).get)) = Set()
          }
          else if (c.equals(colour) && strongCarriers.contains(cell)) {
            //Case: the player plays in a carrier of one its own strong connections
            //This does not change the strength of the connection, so we just remove the cell from the carrier
            hsearch.C((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set(strongCarriers - cell)
            //other way
            hsearch.C((hsearch.G.find(cell2).get, hsearch.G.find(cell1).get)) = Set(strongCarriers - cell)

            //We may be left with a full connection, however, so we remove the strong connection if the carrier is of size 1 (implying the cells are now neighbours)
            if (hsearch.getStrongCarriers(cell1, cell2, true).size == 1) {
              hsearch.C((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set()
              //other way
              hsearch.C((hsearch.G.find(cell2).get, hsearch.G.find(cell1).get)) = Set()
            }
          }
          done((cell1, cell2)) = true;
          done((cell2, cell1)) = true;
        }
      }
    }

    //We must now union all connections concerning the new cell with its neighbours (and vice versa)
    for (cellTemp: Cell <- mod2.neighbours(mod2.board(cell.i)(cell.j)).toSet ++ boundarySet) {
      //ensure the neighbour is indeed a neighbour of the same colour (and is the colour of the player)
      if (cellTemp.colour.equals(c) && colour.equals(c) && areNearestNeighbours(cellTemp, cell)) {

        for (cell_ <- mod2.myCells(colour) ++ mod2.myCells(O) ++ boundarySet) {
          //for each pair of cells (cellTemp, cell_)
          //find union of strong connections of (cell, cell_) and (cellTemp, cell_)
          val unionS = hsearch.C((hsearch.G.find(cell).get, hsearch.G.find(cell_).get)).union(hsearch.C(hsearch.G.find(cellTemp).get, hsearch.G.find(cell_).get))

          //Add this union to connections of the respective pairs
          hsearch.C((hsearch.G.find(cell).get, hsearch.G.find(cell_).get)) = unionS
          hsearch.C((hsearch.G.find(cell_).get, hsearch.G.find(cell).get)) = unionS
          hsearch.C((hsearch.G.find(cellTemp).get, hsearch.G.find(cell_).get)) = unionS
          hsearch.C((hsearch.G.find(cell_).get, hsearch.G.find(cellTemp).get)) = unionS

          //same for weak connections
          val unionW = hsearch.SC((hsearch.G.find(cell).get, hsearch.G.find(cell_).get)).union(hsearch.SC((hsearch.G.find(cellTemp).get, hsearch.G.find(cell_).get)))
          hsearch.SC((hsearch.G.find(cell).get, hsearch.G.find(cell_).get)) = unionW
          hsearch.SC((hsearch.G.find(cell_).get, hsearch.G.find(cell).get)) = unionW
          hsearch.SC((hsearch.G.find(cellTemp).get, hsearch.G.find(cell_).get)) = unionW
          hsearch.SC((hsearch.G.find(cell_).get, hsearch.G.find(cellTemp).get)) = unionW

        }
        hsearch.G.union(cellTemp, cell)

      }
    }
    //makeMove may result in some connections that are not consistent (ie a weak and strong connection for a pair of cells)
    hsearch.makeConnectionsConsistent()

    //return new H-Search object
    hsearch
  }

  /*
  //takes a pair of cells and returns 2
  def getStrongBridge(cell1: Cell, cell2: Cell): Option[(Cell, Cell)] = {
    var boundary1 = HSearch.boundaryBlue1
    var boundary2 = HSearch.boundaryBlue2

    if (colour.equals(R)) {
      boundary1 = HSearch.boundaryRed1; boundary2 = HSearch.boundaryRed2
    }


    def getBoundaryCarrier(a: Cell, b: Cell): Option[(Cell, Cell)] = {
      //a is boundary, b is cell on the board
      var index = b.i

      if ((model.pie && colour.equals(B)) || (!model.pie && colour.equals(R))) {
        index = b.j
      }
      if (a.equals(boundary1) && index == 1) {
        if ((model.pie && colour.equals(R)) || (!model.pie && colour.equals(B))) {
          if (b.i >= 1 && b.j >= 1 && model.board(b.i - 1)(b.j).colour.equals(O) && model.board(b.i - 1)(b.j - 1).colour.equals(O)) {
            return Some((model.board(b.i - 1)(b.j), model.board(b.i - 1)(b.j - 1)))
          }
        }
        else {
          if (b.i >= 1 && b.j >= 1 && model.board(b.i)(b.j - 1).colour.equals(O) && model.board(b.i - 1)(b.j - 1).colour.equals(O)) {
            return Some((model.board(b.i)(b.j - 1), model.board(b.i - 1)(b.j - 1)))
          }
        }
      }
      if (a.equals(boundary2) && index == model.N - 2) {
        if ((model.pie && colour.equals(B)) || (!model.pie && colour.equals(R))) {
          if (b.i < model.N - 1 && b.j < model.N - 1 && model.board(b.i)(b.j + 1).colour.equals(O) && model.board(b.i + 1)(b.j + 1).colour.equals(O)) {
            return Some((model.board(b.i)(b.j + 1), model.board(b.i + 1)(b.j + 1)))
          }
        }
        else {
          if (b.i < model.N - 1 && b.j < model.N - 1 && model.board(b.i + 1)(b.j).colour.equals(O) && model.board(b.i + 1)(b.j + 1).colour.equals(O)) {
            return Some((model.board(b.i + 1)(b.j), model.board(b.i + 1)(b.j + 1)))
          }
        }
      }
      return None

    }

    val possibleCarrier1 = getBoundaryCarrier(cell1, cell2)
    val possibleCarrier2 = getBoundaryCarrier(cell2, cell1)

    if (possibleCarrier1.isDefined) return possibleCarrier1
    if (possibleCarrier2.isDefined) return possibleCarrier2
    var a = new Cell(-1, -1)
    var b = new Cell(-1, -1)
    var found = false
    if (!cell1.equals(boundary1) && !cell1.equals(boundary2) && !cell2.equals(boundary1) && !cell2.equals(boundary2)) {

      if (cell1.i == cell2.i - 1 && cell1.j == cell2.j - 2) {
        a = model.board(cell1.i)(cell1.j + 1)
        b = model.board(cell2.i)(cell2.j - 1)
        found = true
      }
      if (cell1.i == cell2.i - 2 && cell1.j == cell2.j - 1) {
        a = model.board(cell1.i + 1)(cell1.j)
        b = model.board(cell2.i - 1)(cell2.j)
        found = true
      }
      if (cell1.i == cell2.i - 1 && cell1.j == cell2.j + 1) {
        a = model.board(cell1.i)(cell2.j)
        b = model.board(cell2.i)(cell1.j)
        found = true


      }
      if (cell1.i == cell2.i + 1 && cell1.j == cell2.j + 2) {
        a = model.board(cell1.i)(cell1.j - 1)
        b = model.board(cell2.i)(cell2.j + 1)
        found = true
      }
      if (cell1.i == cell2.i + 2 && cell1.j == cell2.j + 1) {
        a = model.board(cell1.i - 1)(cell1.j)
        b = model.board(cell2.i + 1)(cell2.j)
        found = true
      }
      if (cell1.i == cell2.i + 1 && cell1.j == cell2.j - 1) {
        a = model.board(cell1.i)(cell2.j)
        b = model.board(cell2.i)(cell1.j)
        found = true
      }
      if (found && a.colour.equals(O) && b.colour.equals(O)) {

        return Some((a, b))
      }
    }
    return None
  }


  def getWeakBridge(cell1: Cell, cell2: Cell): Set[Set[Cell]] = {
    var boundary1 = HSearch.boundaryBlue1
    var boundary2 = HSearch.boundaryBlue2

    if (colour.equals(R)) {
      boundary1 = HSearch.boundaryRed1; boundary2 = HSearch.boundaryRed2
    }

    def getBoundaryCarrier(a: Cell, b: Cell): Set[Set[Cell]] = {
      var index = b.i
      if ((model.pie && colour.equals(B)) || (!model.pie && colour.equals(R))) {
        index = b.j
      }
      if (a.equals(boundary1) && index == 1) {
        if ((model.pie && colour.equals(R)) || (!model.pie && colour.equals(B))) {
          var set: Set[Set[Cell]] = Set()
          if (b.i == 1 && model.board(b.i - 1)(b.j).colour.equals(O)) {
            set = set + Set(model.board(b.i - 1)(b.j))
          }
          if (b.i == 1 && b.j >= 1 && model.board(b.i - 1)(b.j - 1).colour.equals(O)) {
            set = set + Set(model.board(b.i - 1)(b.j - 1))
          }
          return set
        }
        else {
          var set: Set[Set[Cell]] = Set()
          if (b.j == 1 && model.board(b.i)(b.j - 1).colour.equals(O)) {
            set = set + Set(model.board(b.i)(b.j - 1))
          }
          if (b.j == 1 && b.i >= 1 && model.board(b.i - 1)(b.j - 1).colour.equals(O)) {
            set = set + Set(model.board(b.i - 1)(b.j - 1))
          }
          return set
        }
      }
      if (a.equals(boundary2) && index == model.N - 2) {
        if ((model.pie && colour.equals(B)) || (!model.pie && colour.equals(R))) {
          var set: Set[Set[Cell]] = Set()
          if (b.j == model.N - 2 && model.board(b.i)(b.j + 1).colour.equals(O)) {
            set = set + Set(model.board(b.i)(b.j + 1))
          }
          if (b.j == model.N - 2 && b.i < model.N - 1 && model.board(b.i + 1)(b.j + 1).colour.equals(O)) {
            set = set + Set(model.board(b.i + 1)(b.j + 1))
          }
          return set
        }
        else {

          var set: Set[Set[Cell]] = Set()
          if (b.i == model.N - 2 && model.board(b.i + 1)(b.j).colour.equals(O)) {
            set = set + Set(model.board(b.i + 1)(b.j))
          }
          if (b.i == model.N - 2 && b.j < model.N - 1 && model.board(b.i + 1)(b.j + 1).colour.equals(O)) {
            set = set + Set(model.board(b.i + 1)(b.j + 1))
          }
          return set
        }
      }
      return Set()

    }

    if (cell1.equals(boundary1) || cell2.equals(boundary1) || cell1.equals(boundary2) || cell2.equals(boundary2)) {
      val possibleCarrier1 = getBoundaryCarrier(cell1, cell2)
      val possibleCarrier2 = getBoundaryCarrier(cell2, cell1)
      if (!possibleCarrier1.isEmpty) return possibleCarrier1
      return possibleCarrier2
    }

    var a = new Cell(-1, -1)
    var found = false
    if (cell1.i == cell2.i + 2 && cell1.j == cell2.j + 2) {
      a = model.board(cell1.i - 1)(cell1.j - 1)
      found = true
    }

    if (cell1.i == cell2.i + 2 && cell1.j == cell2.j) {
      a = model.board(cell1.i - 1)(cell1.j)
      found = true
    }
    if (cell1.i == cell2.i && cell1.j == cell2.j + 2) {
      a = model.board(cell1.i)(cell2.j + 1)
      found = true
    }
    if (cell1.i == cell2.i - 2 && cell1.j == cell2.j - 2) {
      a = model.board(cell1.i + 1)(cell1.j + 1)
      found = true
    }

    if (cell1.i == cell2.i - 2 && cell1.j == cell2.j) {
      a = model.board(cell1.i + 1)(cell1.j)
      found = true
    }
    if (cell1.i == cell2.i && cell1.j == cell2.j - 2) {
      a = model.board(cell1.i)(cell2.j - 1)
      found = true
    }

    if (found && a.colour.equals(O)) {
      return Set(Set(a))
    }
    return Set()
  }
  */

  //Ensures that simple rules are enforced between connections
  def makeConnectionsConsistent(): Unit = {
    /*
    for (cell1 <- model.myCells(colour) ++ model.myCells(O) ++ boundarySet; cell2 <- model.myCells(colour) ++ model.myCells(O) ++ boundarySet) {

      val strong_ = getStrongBridge(cell1, cell2)


      val weak = getWeakBridge(cell1, cell2)
      if (strong_.isDefined) {

        C((G.find(cell1).get, G.find(cell2).get)) = Set(Set(strong_.get._1, strong_.get._2))
        C((G.find(cell2).get, G.find(cell1).get)) = Set(Set(strong_.get._1, strong_.get._2))
      }
      if (weak.nonEmpty && getStrongCarriers(cell1, cell2, true).isEmpty) {
        SC((G.find(cell1).get, G.find(cell2).get)) = weak
        SC((G.find(cell2).get, G.find(cell1).get)) = weak
      }

    }
    */
    for (cell1 <- model.myCells(colour) ++ model.myCells(O) ++ boundarySet; cell2 <- model.myCells(colour) ++ model.myCells(O) ++ boundarySet) {
      //actually tends to work better without these:
      val carrier = getStrongCarriers(cell1, cell2, false)
      strong = strong.union(carrier)

      if (getStrongCarriers(cell1, cell2, true).nonEmpty) {
        //if a pair of cells form a strong connection, they cannot also have a weak connection
        SC((G.find(cell1).get, G.find(cell2).get)) = Set()
      }
      if (areNearestNeighbours(cell1, cell2)) {
        //neighbouring cells cannot have a weak connection between them
        SC((G.find(cell1).get, G.find(cell2).get)) = Set()
      }
    }


  }
  //main search method
  def search: Unit = {
    var currentNewVC = false
    var previousNewVC = true
    while (currentNewVC || previousNewVC) {

      val tempC = C
      previousNewVC = currentNewVC
      currentNewVC = false
      for (g <- G.getReps) {
        for (g1 <- G.getReps; g2 <- G.getReps) {
          if (g1 != g2 && (newCarrier(oldC, g1, g) || newCarrier(oldC, g2, g)) && (!(g.colour == colour) || (g1.colour == O && g2.colour == O))) {
            for (c1 <- C((g1, g)); c2 <- C((g2, g))) {
              if ((!oldC((g1, g)).contains(c1) || !oldC((g2, g)).contains(c2)) && (c1 & c2).isEmpty && !c2.contains(g1) && !c1.contains(g2)) {
                currentNewVC = true
                if (g.colour == colour && (c1.size + c2.size) <= HSearch.M) {
                  C((g1, g2)) = C((g1, g2)) + (c1 ++ c2)
                }
                else {
                  val sc = c1 ++ Set(g) ++ c2
                  SC((g1, g2)) = SC((g1, g2)) + sc
                  C((g1, g2)) = apply(C((g1, g2)), (SC((g1, g2)) - sc).take(HSearch.X), sc, sc)
                }
              }
            }
          }
        }
      }
      oldC = tempC
    }
    //ensure connections are consistent after search algorithm
    makeConnectionsConsistent()
  }


  //returns the strong carrier between two cells
  //getAll specifies if we include open cells too
  def getStrongCarriers(cell1: Cell, cell2: Cell, getAll: Boolean): Set[Cell] = {

    var minSet: Set[Cell] = Set()
    var minSize = Int.MaxValue
    if (cell1.equals(cell2)) return Set()
    val othercolour = colour match{
      case R => B
      case B => R
    }

    //only considers cells that are not the opponents colour
    if (cell1.colour.equals(othercolour) || cell2.colour.equals(othercolour)) {
      return Set()
    }
    if (getAll) {
      //find minimal carrier
      for (set <- C((G.find(cell1).get, G.find(cell2).get))) {
        if (minSize > set.size) {
          minSet = set
          minSize = set.size
        }
      }
      return minSet
    }
    else {
      if (cell1.colour.equals(colour) && cell2.colour.equals(colour)) {
        //find minimal carrier
        for (set <- C((G.find(cell1).get, G.find(cell2).get))) {
          if (minSize > set.size) {
            minSet = set
            minSize = set.size
          }
        }
      }
      return minSet
    }

  }
  //returns the strong carrier between two cells
  //getAll specifies if we include open cells too
  def getWeakCarriers(cell1: Cell, cell2: Cell, getAll: Boolean): Set[Cell] = {

    var minSet: Set[Cell] = Set()
    var minSize = Int.MaxValue
    if (cell1.equals(cell2)) return Set()
    val othercolour = colour match{
      case R => B
      case B => R
    }
    //do not consider opponent's cells
    if (cell1.colour.equals(othercolour) || cell2.colour.equals(othercolour)) {

      return Set()
    }
    if (getAll) {
      for (set <- SC((G.find(cell1).get, G.find(cell2).get))) {
        //find minimal carrier
        if (minSize > set.size) {
          minSet = set
          minSize = set.size
        }
      }
      return minSet
    }
    else {
      if (cell1.colour.equals(colour) && cell2.colour.equals(colour)) {
        for (set <- SC((G.find(cell1).get, G.find(cell2).get))) {
          //find minimal carrier
          if (minSize > set.size) {
            minSet = set
            minSize = set.size
          }
        }
      }
      return minSet
    }

  }



  //Determines whether a there is a new carrier between cells x and y (since the last round)
  def newCarrier(oldC: collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]], x: Cell, y: Cell): Boolean = {

    val a = try {
      oldC((x, y)).size
    }
    catch {
      case _: Exception => 0
    }
    val b = try {
      C((x, y)).size
    }
    catch {
      case _: Exception => 0
    }

    return a != b


  }

  //recusrive method used in main search method
  //applies deduction rules between connections
  def apply(C_set: Set[Set[Cell]], SC_set: Set[Set[Cell]], union: Set[Cell], intersection: Set[Cell]): Set[Set[Cell]] = {
    var C_clone = C_set

    for (scl <- SC_set) {

      val ul = scl ++ union
      val il = scl & intersection
      if (il.isEmpty && ul.size <= HSearch.M) {
        C_clone += ul
      }
      else {

        C_clone = apply(C_clone, SC_set - scl, ul, il)
      }
    }
    return C_clone
  }

  //method to decide if two cells g1, g2 are neighbours (also used for boundary neighbour checks)
  def areNearestNeighbours(g1: Cell, g2: Cell): Boolean = {
    val x1 = g1.i
    val x2 = g2.i
    val y1 = g1.j
    val y2 = g2.j

    var h = (x1 == x2 && y1 == y2 + 1) || (x1 == x2 && y1 == y2 - 1) || (y1 == y2 && x1 == x2 + 1) || (y1 == y2 && x1 == x2 - 1) || (x1 == x2 + 1 && y1 == y2 + 1) || (x1 == x2 - 1 && y1 == y2 - 1)
    h = h || (x1 == 0 && x2 == -1) || (x1 == model.N - 1 && x2 == -3) || (y1 == 0 && y2 == -1) || (y1 == model.N - 1 && y2 == -3)
    h = h || (x2 == 0 && x1 == -1) || (x2 == model.N - 1 && x1 == -3) || (y2 == 0 && y1 == -1) || (y2 == model.N - 1 && y1 == -3)
    return h
  }

  private def result(mod: Model, cell: Cell, col: Colour): Model = {
    val mod2 = mod.copy()
    mod2.playMove(cell, col)
    return mod2

  }

  //Get all cells that are in a carrier of the two boundaries
  def getUnionOfWeakConnections: Set[Cell] = {
    val setList = boundarySet.toList
    val boundary1 = setList(0)
    val boundary2 = setList(1)
    var finalSet = model.myCells(O).toSet
    for (s <- SC((G.find(boundary1).get, G.find(boundary2).get))) {
      finalSet = finalSet.intersect(s)
    }
    finalSet
  }


}

object HSearch extends Const {

  var boundaryRed1: Cell = new Cell(0, -1)
  var boundaryRed2: Cell = new Cell(0, -3)
  var boundaryBlue1: Cell = new Cell(-1, 0)
  var boundaryBlue2: Cell = new Cell(-3, 0)
  boundaryRed1.colour = R
  boundaryRed2.colour = R
  boundaryBlue1.colour = B
  boundaryBlue2.colour = B

  def pie = {

    var temp = boundaryRed1
    boundaryRed1 = boundaryBlue1
    boundaryBlue1 = temp
    temp = boundaryRed2
    boundaryRed2 = boundaryBlue2
    boundaryBlue2 = temp
    boundaryRed1.colour = R
    boundaryRed2.colour = R
    boundaryBlue1.colour = B
    boundaryBlue2.colour = B
  }


  //Hyper-Parameters:
  /* M: maximum number of carriers for a pair of cells
     X: maximumn number of virtual connections as an input to the dedection rule
  */
  val M = 12
  val X = 4
}