package hsearch

import hexagony._

class HSearch(var model: Model, var colour: Colour) extends Const {

  //a set to store all cells that belong to a strong connection's carrier
  var strong: Set[Cell] = Set()

  val strongDual : collection.mutable.Map[Cell, Set[Cell]] = new collection.mutable.HashMap[Cell, Set[Cell]]().withDefaultValue(Set())
  val weakDual : collection.mutable.Map[Cell, Set[Cell]] = new collection.mutable.HashMap[Cell, Set[Cell]]().withDefaultValue(Set())

  val strongParents : collection.mutable.Map[Cell, Set[(Cell, Cell)]] = new collection.mutable.HashMap[Cell, Set[(Cell, Cell)]]().withDefaultValue(Set())
  val weakParents : collection.mutable.Map[Cell, Set[(Cell, Cell)]] = new collection.mutable.HashMap[Cell, Set[(Cell, Cell)]]().withDefaultValue(Set())
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
        //Union cells od same colour that are neighbours, so they can be treated as a single cell
        if (g1.colour.equals(colour) && g2.colour.equals(colour) && areNearestNeighbours(g1, g2)) {

          G.union(g1, g2)
        }
      }
    }
    //Initialise oldC
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

    val defined = scala.collection.mutable.Map[(Cell, Cell), Boolean]().withDefaultValue(false)

    for (g1 <- Gtemp) {
      for (g2 <- Gtemp) {
        val rep1 = G.find(g1).get
        val rep2 = G.find(g2).get
        if (!areNearestNeighbours(g1, g2) && !defined((rep1, rep2))) {
          C((rep1, rep2)) = Set()
          C((rep2, rep1)) = Set()
        }
        else if(areNearestNeighbours(g1, g2)){
          C((rep1, rep2)) = Set(Set())
          C((rep2, rep1)) = Set(Set())
          defined((rep1, rep2)) = true
          defined((rep2, rep1)) = true
        }
        SC((rep1, rep2)) = Set()
        SC((rep2, rep1)) = Set()
      }
    }
  }


  def makeMove(moves : List[(Cell, Colour)]): HSearch = {
    //println("MAKE MOVE START")
    //println("MAKE MOVE START")
    //When a move is made, we consider what happens to the strong/weak connections
    //Make a clone of the HSearch object to allow for easy recursion in minimax

    //Make move
    var mod2 = model.copy()
    for((cell, c) <- moves) mod2 = result(mod2, cell, c)
    //Initialise new H-Search object
    val hsearch = new HSearch(mod2, colour)
    //Clone disjoint sets (to maintain structure of neighbouring cells)
    val newG = G.clone()
    hsearch.G = newG.asInstanceOf[DisjointSets[Cell]]

    //Update colour
    for((cell,c) <- moves) {
      if (colour.equals(c)) hsearch.G.add(mod2.board(cell.i)(cell.j))
      else hsearch.G.remove(mod2.board(cell.i)(cell.j))
    }
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
    for ((cell, c) <- moves; (cell1,cell2) <- strongParents(G.find(cell).get)) {
      //for (cell2 <- mod2.myCells(colour) ++ mod2.myCells(O) ++ boundarySet) {
      if(boundarySet.contains(cell1) || (cell1.i >= 0 && cell1.j < mod2.N && cell1.i >= 0 && cell1.j < mod2.N && (mod2.board(cell1).colour.equals(O) || mod2.board(cell1).colour.equals(colour)))){
        if(boundarySet.contains(cell2) || (cell2.i >= 0 && cell2.j < mod2.N && cell2.i >= 0 && cell2.j < mod2.N && (mod2.board(cell2).colour.equals(O) || mod2.board(cell2).colour.equals(colour))) && !done((cell1, cell2))){
          //for((cell,c) <- moves){
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
          }
          done((cell1, cell2)) = true
          done((cell2, cell1)) = true
      }
    }
    hsearch
  }

  private def containsNonEmpty(setOfSets : Set[Set[Cell]]) : Boolean = {
    for(set <- setOfSets){
      if(set.size > 0) return true
    }
    return false
  }

  //main search method
  def search(timelimit : Long): Unit = {
    var currentNewVC = false
    var previousNewVC = true
    val start = System.currentTimeMillis()
    val end : Long = start + timelimit
    while ((currentNewVC || previousNewVC) && System.currentTimeMillis() < end) {

      val tempC = C
      previousNewVC = currentNewVC
      currentNewVC = false
      for (g <- Gtemp) {
        if(System.currentTimeMillis() < end) {
          for (g1 <- Gtemp; g2 <- Gtemp) {
            if(System.currentTimeMillis() < end) {
              if (g1 != g2 && (newCarrier(oldC, g1, g) || newCarrier(oldC, g2, g)) && (!(g.colour == colour) || (g1.colour == O && g2.colour == O))) {
                for (c1 <- C((G.find(g1).get, G.find(g).get)); c2 <- C((G.find(g2).get, G.find(g).get))) {
                  if (System.currentTimeMillis() < end && (!oldC((G.find(g1).get, G.find(g).get)).contains(c1) || !oldC((G.find(g2).get, G.find(g).get)).contains(c2)) && (c1 & c2).isEmpty && !c2.contains(g1) && !c1.contains(g2)) {
                    currentNewVC = true
                    if (g.colour == colour && (c1.size + c2.size) <= HSearch.M) {
                      val cTemp = c1 ++ c2
                      /**/
                      if(C((G.find(g1).get, G.find(g2).get)).isEmpty || C((G.find(g1).get, G.find(g2).get)).head.size > cTemp.size) C((G.find(g1).get, G.find(g2).get)) = Set(cTemp)
                      //C((G.find(g1).get, G.find(g2).get)) = C((G.find(g1).get, G.find(g2).get)) + cTemp
                      if(cTemp.size > 0) {
                        strongDual(G.find(g1).get) = strongDual(G.find(g1).get) + G.find(g2).get
                        strongDual(G.find(g2).get) = strongDual(G.find(g2).get) + G.find(g1).get
                        for(cell_ <- cTemp){
                          val tuple = (G.find(g1).get, G.find(g2).get)
                          strongParents(G.find(cell_).get) = strongParents(G.find(cell_).get) + tuple
                        }
                      }
                      if (g1.colour == colour && g2.colour == colour) {
                        strong = strong.union(cTemp)
                      }
                    }
                    else {
                      val sc = c1 ++ Set(g) ++ c2

                      /**/
                      if(SC((G.find(g1).get, G.find(g2).get)).isEmpty || SC((G.find(g1).get, G.find(g2).get)).head.size > sc.size) SC((G.find(g1).get, G.find(g2).get)) = Set(sc)
                      //SC((G.find(g1).get, G.find(g2).get)) = SC((G.find(g1).get, G.find(g2).get)) + sc
                      if (sc.size > 0) {
                        for(cell_ <- sc){
                          val tuple = (G.find(g1).get, G.find(g2).get)
                          weakParents(G.find(cell_).get) = weakParents(G.find(cell_).get) + tuple
                        }
                        weakDual(G.find(g1).get) = weakDual(G.find(g1).get) + G.find(g2).get
                        weakDual(G.find(g2).get) = weakDual(G.find(g2).get) + G.find(g1).get
                      }
                      val temp = apply(C((G.find(g1).get, G.find(g2).get)), (SC((G.find(g1).get, G.find(g2).get)) - sc).take(HSearch._K), sc, sc, end)
                      C((G.find(g1).get, G.find(g2).get)) = temp
                      if(containsNonEmpty(temp)){
                        for(set <- temp){
                          for(cell_ <- set){
                            val tuple = (G.find(g1).get, G.find(g2).get)
                            strongParents(G.find(cell_).get) = strongParents(G.find(cell_).get) + tuple
                          }
                        }
                        strongDual(G.find(g1).get) = strongDual(G.find(g1).get) + G.find(g2).get
                        strongDual(G.find(g2).get) = strongDual(G.find(g2).get) + G.find(g1).get
                      }

                    }
                  }
                }
              }
            }
          }
        }
      }
      oldC = tempC
    }
    println("HSEARCH FINISHED")
  }


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
    if (cell1.colour.equals(othercolour) || cell2.colour.equals(othercolour) ) {

      return Set()
    }

    //No weak connection if already a strong one
    if (getStrongCarriers(cell1, cell2, getAll).nonEmpty) {
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
      C((G.find(x).get, G.find(y).get)).size
    }
    catch {
      case _: Exception => 0
    }

    return a != b
  }

  //recusrive method used in main search method
  //applies deduction rules between connections
  def apply(C_set: Set[Set[Cell]], SC_set: Set[Set[Cell]], union: Set[Cell], intersection: Set[Cell], end : Long): Set[Set[Cell]] = {
    var C_clone = C_set

    for (scl <- SC_set) {
      if(System.currentTimeMillis() < end) {
        val ul = scl ++ union
        val il = scl & intersection
        if (il.isEmpty && C_clone.size <= HSearch.M) {
          C_clone += ul

        }
        else {
          if (System.currentTimeMillis() < end) C_clone = apply(C_clone, SC_set - scl, ul, il, end)
        }
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
  var p = false
  var boundaryRed1: Cell = new Cell(0, -1)
  var boundaryRed2: Cell = new Cell(0, -3)
  var boundaryBlue1: Cell = new Cell(-1, 0)
  var boundaryBlue2: Cell = new Cell(-3, 0)
  boundaryRed1.colour = R
  boundaryRed2.colour = R
  boundaryBlue1.colour = B
  boundaryBlue2.colour = B

  def pie = {
    p = !p
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
  var M = 14
  var _K : Int = 3
}