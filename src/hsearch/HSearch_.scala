package hsearch

import experiments.RobotAlphaBetaResistance
import hexagony._

class HSearch_(var model: Model, var colour: Colour, startingC : collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]], startingSC : collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]]) extends Const {
  def this(mod: Model, col: Colour) = this(mod, col, collection.mutable.HashMap[(Cell, Cell), Set[Set[Cell]]](), collection.mutable.HashMap[(Cell, Cell), Set[Set[Cell]]]())
  //a set to store all cells that belong to a strong connection's carrier
  var strong: Set[Cell] = Set()

  var strongDual : collection.mutable.Map[Cell, Set[Cell]] = new collection.mutable.HashMap[Cell, Set[Cell]]().withDefaultValue(Set())
  var weakDual : collection.mutable.Map[Cell, Set[Cell]] = new collection.mutable.HashMap[Cell, Set[Cell]]().withDefaultValue(Set())


  var strongParents : collection.mutable.Map[Cell, Set[(Cell, Cell)]] = new collection.mutable.HashMap[Cell, Set[(Cell, Cell)]]().withDefaultValue(Set())
  var weakParents : collection.mutable.Map[Cell, Set[(Cell, Cell)]] = new collection.mutable.HashMap[Cell, Set[(Cell, Cell)]]().withDefaultValue(Set())
  var boundarySet: Set[Cell] = colour match {
    case R => Set(HSearch.boundaryRed1, HSearch.boundaryRed2)
    case B => Set(HSearch.boundaryBlue1, HSearch.boundaryBlue2)
    case _ => Set()
  }
  var minC : collection.mutable.Map[(Cell, Cell), Int] = collection.mutable.Map[(Cell, Cell), Int]().withDefaultValue(Int.MaxValue)
  var minSC : collection.mutable.Map[(Cell, Cell), Int] = collection.mutable.Map[(Cell, Cell), Int]().withDefaultValue(Int.MaxValue)
  var numC : collection.mutable.Map[(Cell, Cell), Int] = collection.mutable.Map[(Cell, Cell), Int]().withDefaultValue(0)
  var numSC : collection.mutable.Map[(Cell, Cell), Int] = collection.mutable.Map[(Cell, Cell), Int]().withDefaultValue(0)

  //initialise set to store all cells (apart from opponents colour)
  var Gtemp: Set[Cell] = Set()

  //G is forest of disjoint sets (to allow connected cells to be treated as one)
  var G: DisjointSets[Cell] = new DisjointSets[Cell]()

  //C stores strong carriers (maps pairs of cells to a set of carriers)
  var C: collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]] = startingC
  //SC stores weak carriers (maps pairs of cells to a set of carriers)
  var SC: collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]] = startingSC
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
          minC((rep1, rep2)) = 0
          minC((rep2, rep1)) = 0
          numC((rep1, rep2)) = 1
          numC((rep2, rep1)) = 1
          defined((rep1, rep2)) = true
          defined((rep2, rep1)) = true
        }
        SC((rep1, rep2)) = Set()
        SC((rep2, rep1)) = Set()



      }

    }

  }
  def cloneWithMove(moves : List[(Cell, Colour)] ) : HSearch_ = {
    val othercolour = colour match{
      case R => B
      case B => R
    }
    var mod2 = model.copy()
    for((cell, c) <- moves) mod2 = result(mod2, cell, c)
    //Initialise new H-Search object
    val hsearch = new HSearch_(mod2, colour, C.clone(), SC.clone())//, C.clone(), SC.clone())

    hsearch.minC = collection.mutable.Map[(Cell, Cell), Int]() ++= minC
    hsearch.minSC = collection.mutable.Map[(Cell, Cell), Int]() ++= minSC
    hsearch.numC = collection.mutable.Map[(Cell, Cell), Int]() ++= numC
    hsearch.numSC = collection.mutable.Map[(Cell, Cell), Int]() ++= numSC
    hsearch.strongDual = collection.mutable.Map[Cell, Set[Cell]]() ++= strongDual
    hsearch.weakDual = collection.mutable.Map[Cell, Set[Cell]]() ++= weakDual

    hsearch.strongParents= collection.mutable.Map[Cell, Set[(Cell, Cell)]]()
    hsearch.weakParents= collection.mutable.Map[Cell, Set[(Cell, Cell)]]()


    //Clone disjoint sets (to maintain structure of neighbouring cells)
    val newG = G.clone()
    hsearch.G = newG.asInstanceOf[DisjointSets[Cell]]

    //Update colour
    for((cell_,c) <- moves) {
      val cell = hsearch.model.board(cell_)
      if (colour.equals(c)) {
        hsearch.G.add(mod2.board(cell.i)(cell.j))
        for (neighbour <- mod2.neighbours(mod2.board(cell)) ++ boundarySet) {
          if (neighbour.colour.equals(colour) && areNearestNeighbours(cell, neighbour)) hsearch.G.union(neighbour, cell)
        }
      }
      else hsearch.G.remove(mod2.board(cell.i)(cell.j))
    }
    for(c1 <- strongParents.keys){
      if(hsearch.G.find(c1).isDefined) hsearch.strongParents(hsearch.G.find(c1).get) = strongParents(c1)

    }
    for(c1 <- weakParents.keys){
      if(hsearch.G.find(c1).isDefined) hsearch.weakParents(hsearch.G.find(c1).get) = weakParents(c1)

    }
    for(c1 <- strongDual.keys){
      if(hsearch.G.find(c1).isDefined) hsearch.strongDual(hsearch.G.find(c1).get) = strongDual(c1)

    }
    for(c1 <- weakDual.keys){
      if(hsearch.G.find(c1).isDefined) hsearch.weakDual(hsearch.G.find(c1).get) = weakDual(c1)

    }



    for((cell_,c) <- moves){

      val cell = hsearch.model.board(cell_)
      if(c.equals(colour)){
        hsearch.strongParents(hsearch.G.find(cell).get) = Set()////////////////////////////////
        hsearch.weakParents(hsearch.G.find(cell).get) = Set()
      }

      for((c1_, c2_) <- strongParents(G.find(cell).get)){
        var c1 = c1_
        var c2 = c2_
        if(c1_.i >= 0 && c1_.j >= 0) c1 = hsearch.model.board(c1)
        if(c2_.i >= 0 && c2_.j >= 0) c2 = hsearch.model.board(c2)
        if(!c1.colour.equals(othercolour) && !c2.colour.equals(othercolour)) {
          hsearch.strongDual(hsearch.G.find(c1).get) = hsearch.strongDual(hsearch.G.find(c1).get) - hsearch.G.find(c2).get
          hsearch.strongDual(hsearch.G.find(c2).get) = hsearch.strongDual(hsearch.G.find(c2).get) - hsearch.G.find(c1).get



          hsearch.C((c1, c2)) = Set()
          hsearch.C((c2, c1)) = Set()
        }
      }
      for((c1_, c2_) <- weakParents(G.find(cell).get)){
        var c1 = c1_
        var c2 = c2_
        if(c1_.i >= 0 && c1_.j >= 0) c1 = hsearch.model.board(c1)
        if(c2_.i >= 0 && c2_.j >= 0) c2 = hsearch.model.board(c2)
        if(!c1.colour.equals(othercolour) && !c2.colour.equals(othercolour)) {
          hsearch.weakDual(hsearch.G.find(c1).get) = hsearch.weakDual(hsearch.G.find(c1).get) - hsearch.G.find(c2).get
          hsearch.weakDual(hsearch.G.find(c2).get) = hsearch.weakDual(hsearch.G.find(c2).get) - hsearch.G.find(c1).get
          hsearch.SC((c1, c2)) = Set()
          hsearch.SC((c2, c1)) = Set()

        }
      }

      }

    hsearch.search(RobotAlphaBetaResistance.LEAFTIME)
    hsearch

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
          done((cell1, cell2)) = true;
          done((cell2, cell1)) = true;
        //}
      }
    }
    /*
    //We must now union all connections concerning the new cell with its neighbours (and vice versa)
    for ((cell,c) <- moves; cellTemp <- mod2.neighbours(mod2.board(cell.i)(cell.j)).toSet ++ boundarySet) {
      //println(cell)
      //ensure the neighbour is indeed a neighbour of the same colour (and is the colour of the player)
      if (cellTemp.colour.equals(c) && colour.equals(c) && areNearestNeighbours(cellTemp, cell)) {
        val options = (strongDual(G.find(cell).get) union weakDual(G.find(cell).get)) intersect (mod2.myCells(O).toSet union mod2.myCells(colour).toSet)
        //println(options)
        for (cell_ <- options) {
          //for (cell_ <- mod2.myCells(colour) ++ mod2.myCells(O) ++ boundarySet) {
          //println(cell_)

          if(boundarySet.contains(cell_) || (cell_.i >= 0 && cell_.j < mod2.N && cell_.i >= 0 && cell_.j < mod2.N && (mod2.board(cell_).colour.equals(O) || mod2.board(cell_).colour.equals(colour)))){//for each pair of cells (cellTemp, cell_)
          //find union of strong connections of (cell, cell_) and (cellTemp, cell_)
            val unionS = hsearch.C((hsearch.G.find(cell).get, hsearch.G.find(cell_).get)).union(hsearch.C((hsearch.G.find(cellTemp).get, hsearch.G.find(cell_).get)))

            //Add this union to connections of the respective pairs
            //hsearch.C((hsearch.G.find(cell).get, hsearch.G.find(cell_).get)) = unionS
            //hsearch.C((hsearch.G.find(cell_).get, hsearch.G.find(cell).get)) = unionS
            hsearch.C((hsearch.G.find(cellTemp).get, hsearch.G.find(cell_).get)) = unionS
            hsearch.C((hsearch.G.find(cell_).get, hsearch.G.find(cellTemp).get)) = unionS

            //same for weak connections
            val unionW = hsearch.SC((hsearch.G.find(cell).get, hsearch.G.find(cell_).get)).union(hsearch.SC((hsearch.G.find(cellTemp).get, hsearch.G.find(cell_).get)))
            //hsearch.SC((hsearch.G.find(cell).get, hsearch.G.find(cell_).get)) = unionW
            //hsearch.SC((hsearch.G.find(cell_).get, hsearch.G.find(cell).get)) = unionW
            hsearch.SC((hsearch.G.find(cellTemp).get, hsearch.G.find(cell_).get)) = unionW
            hsearch.SC((hsearch.G.find(cell_).get, hsearch.G.find(cellTemp).get)) = unionW}



        }
        hsearch.G.union(cellTemp, cell)

      }
    }
    */
    //return new H-Search object
    //println("MAKE MOVE FINISH")
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

      val tempC = C.clone()
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
                    if (g.colour == colour) {
                      val cTemp = c1 ++ c2
                      /**/
                      if(minC((G.find(g1).get, G.find(g2).get)) > cTemp.size){
                        C((G.find(g1).get, G.find(g2).get)) = Set(cTemp)
                        C((G.find(g2).get, G.find(g1).get)) = Set(cTemp)
                        minC((G.find(g1).get, G.find(g2).get)) = cTemp.size
                        minC((G.find(g2).get, G.find(g1).get)) = cTemp.size
                        numC((G.find(g1).get, G.find(g2).get)) = 1
                        numC((G.find(g2).get, G.find(g1).get)) = 1

                        strongDual(G.find(g1).get) = strongDual(G.find(g1).get) + G.find(g2).get
                        strongDual(G.find(g2).get) = strongDual(G.find(g2).get) + G.find(g1).get
                        for(cell_ <- cTemp){
                          val tuple = (G.find(g1).get, G.find(g2).get)
                          strongParents(G.find(cell_).get) = strongParents(G.find(cell_).get) + tuple
                        }
                        if (g1.colour == colour && g2.colour == colour) {
                          strong = strong.union(cTemp)
                        }

                      }
                      else if(minC((G.find(g1).get, G.find(g2).get)) == cTemp.size && numC((G.find(g1).get, G.find(g2).get)) < HSearch.M){
                        C((G.find(g1).get, G.find(g2).get)) = C((G.find(g1).get, G.find(g2).get)) + cTemp
                        C((G.find(g2).get, G.find(g1).get)) = C((G.find(g2).get, G.find(g1).get)) + cTemp
                        numC((G.find(g1).get, G.find(g2).get)) += 1
                        numC((G.find(g2).get, G.find(g1).get)) += 1
                        strongDual(G.find(g1).get) = strongDual(G.find(g1).get) + G.find(g2).get
                        strongDual(G.find(g2).get) = strongDual(G.find(g2).get) + G.find(g1).get
                        for(cell_ <- cTemp){
                          val tuple = (G.find(g1).get, G.find(g2).get)
                          strongParents(G.find(cell_).get) = strongParents(G.find(cell_).get) + tuple
                        }
                        if (g1.colour == colour && g2.colour == colour) {
                          strong = strong.union(cTemp)
                        }

                      }
                     // else if(minC((G.find(g1).get, G.find(g2).get)) < cTemp.size){
                      //  C((G.find(g1).get, G.find(g2).get)) = C((G.find(g1).get, G.find(g2).get)) + cTemp
                       // C((G.find(g2).get, G.find(g1).get)) = C((G.find(g2).get, G.find(g1).get)) + cTemp
                    //  }


                      //C((G.find(g1).get, G.find(g2).get)) = C((G.find(g1).get, G.find(g2).get)) + cTemp
                      if(cTemp.size > 0) {

                      }

                    }
                    else {
                      val sc = c1 ++ Set(g) ++ c2

                      /**/
                      var update = false
                      if(minSC((G.find(g1).get, G.find(g2).get)) > sc.size){
                        SC((G.find(g1).get, G.find(g2).get)) = Set(sc)
                        SC((G.find(g2).get, G.find(g1).get)) = Set(sc)
                        minSC((G.find(g1).get, G.find(g2).get)) = sc.size
                        minSC((G.find(g2).get, G.find(g1).get)) = sc.size
                        numSC((G.find(g1).get, G.find(g2).get)) += 1
                        numSC((G.find(g2).get, G.find(g1).get)) += 1
                        update = true

                      }
                      else if(minSC((G.find(g1).get, G.find(g2).get)) == sc.size && numSC((G.find(g1).get, G.find(g2).get)) < HSearch.M){
                        SC((G.find(g1).get, G.find(g2).get)) = SC((G.find(g1).get, G.find(g2).get)) + sc
                        SC((G.find(g2).get, G.find(g1).get)) = SC((G.find(g1).get, G.find(g2).get)) + sc
                        numSC((G.find(g1).get, G.find(g2).get)) += 1
                        numSC((G.find(g2).get, G.find(g1).get)) += 1
                        update = true
                      }
                      else if(minSC((G.find(g1).get, G.find(g2).get)) < sc.size){
                        SC((G.find(g1).get, G.find(g2).get)) = SC((G.find(g1).get, G.find(g2).get)) + sc
                        SC((G.find(g2).get, G.find(g1).get)) = SC((G.find(g2).get, G.find(g1).get)) + sc
                        update = true
                      }

                      //SC((G.find(g1).get, G.find(g2).get)) = SC((G.find(g1).get, G.find(g2).get)) + sc
                      if (sc.size > 0 && update) {

                        for(cell_ <- sc){
                          val tuple = (G.find(g1).get, G.find(g2).get)
                          weakParents(G.find(cell_).get) = weakParents(G.find(cell_).get) + tuple
                        }
                        weakDual(G.find(g1).get) = weakDual(G.find(g1).get) + G.find(g2).get
                        weakDual(G.find(g2).get) = weakDual(G.find(g2).get) + G.find(g1).get
                      }
                      if(update){
                        val temp = apply(C((G.find(g1).get, G.find(g2).get)), (SC((G.find(g1).get, G.find(g2).get)) - sc).take(HSearch._K), sc, sc, end, g1, g2)
                        C((G.find(g1).get, G.find(g2).get)) = temp
                        C((G.find(g2).get, G.find(g1).get)) = temp
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
      }
      oldC = tempC//.clone()
    }
    //println("HSEARCH FINISHED")
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
  def apply(C_set: Set[Set[Cell]], SC_set: Set[Set[Cell]], union: Set[Cell], intersection: Set[Cell], end : Long, g1 : Cell, g2: Cell): Set[Set[Cell]] = {
    var C_clone = C_set

    for (scl <- SC_set) {
      if(System.currentTimeMillis() < end) {
        val ul = scl ++ union
        val il = scl & intersection
        if (il.isEmpty) {
          if(minC((G.find(g1).get, G.find(g2).get)) > ul.size){
            C_clone = Set(ul)

            minC((G.find(g1).get, G.find(g2).get)) = ul.size
            minC((G.find(g2).get, G.find(g1).get)) = ul.size
            numC((G.find(g1).get, G.find(g2).get)) = 1
            numC((G.find(g2).get, G.find(g1).get)) = 1
          }
          else if(minC((G.find(g1).get, G.find(g2).get)) == ul.size && C((G.find(g1).get, G.find(g2).get)).size < HSearch.M){
            C_clone += ul
            numC((G.find(g1).get, G.find(g2).get)) += 1
            numC((G.find(g2).get, G.find(g1).get)) += 1

          }
          //else if(minC((G.find(g1).get, G.find(g2).get)) < ul.size){
            //C_clone += ul
          //}

        }
        else {
          if (System.currentTimeMillis() < end) C_clone = apply(C_clone, SC_set - scl, ul, il, end, g1, g2)
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

