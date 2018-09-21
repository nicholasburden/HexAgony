package HSearch

import hexagony._

class HSearch(val model: Model, colour: Colour, maxDepth : Int) extends Const{
  val boundaryRed1 : Cell = new Cell(0, -1)
  val boundaryRed2 : Cell = new Cell(0, model.N)
  val boundaryBlue1 : Cell = new Cell(-1, 0)
  val boundaryBlue2 : Cell = new Cell(model.N, 0)

  var Gtemp : Set[Cell] = Set()
  for(cell <- model.myCells(colour)){
    Gtemp = Gtemp + cell
  }
  for(cell <- model.myCells(O)){
    Gtemp = Gtemp + cell
  }
  if(colour == B){
    Gtemp = Gtemp + boundaryBlue1
    Gtemp = Gtemp + boundaryBlue2
  }
  else{
    Gtemp = Gtemp + boundaryRed1
    Gtemp = Gtemp + boundaryRed2
  }
  var G : DisjointSets[Cell] = new DisjointSets[Cell]()
  for(g <- Gtemp){
    G.add(g)
  }
  for(g1 <- Gtemp){
    for(g2 <- Gtemp){
      if(g1.colour == colour && g2.colour == colour && areNearestNeighbours(g1, g2)){
        G.union(g1, g2)
      }
    }
  }

  var C : collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]] = collection.mutable.Map()
  var SC : collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]] = collection.mutable.Map()
  var oldC : collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]] = collection.mutable.Map()
  for(g1 <- G.getReps){
    for(g2 <- G.getReps){
      if(!areNearestNeighbours(g1,g2)){
        C((g1, g2)) = Set()
      }
      else{
        C((g1, g2)) = Set(Set())
      }
      SC((g1, g2)) = Set()
      oldC((g1, g2)) = Set()
    }
  }



  def search : Unit = {

    var currentNewVC = false
    var previousNewVC = true
    var depth = 0
    while ((currentNewVC || previousNewVC) && depth < maxDepth) {
      //println("GOING")
      val tempC = C
      previousNewVC = currentNewVC
      currentNewVC = false
      for (g <- G.getReps) {
        for (g1 <- G.getReps; g2 <- G.getReps) {
          //println((newCarrier(oldC, g1, g) || newCarrier(oldC, g2, g)))
          if (g1 != g2 && (newCarrier(oldC, g1, g) || newCarrier(oldC, g2, g)) && (!(g.colour == colour) || (g1.colour == O && g2.colour == O))) {

            for (c1 <- C((g1, g)); c2 <- C((g2, g))) {

              if ((!oldC((g1, g)).contains(c1) || !oldC((g2, g)).contains(c2)) && (c1 & c2).isEmpty && !c2.contains(g1) && !c1.contains(g2)) {
                currentNewVC = true
                if (g.colour == colour) {
                  C((g1, g2)) = C((g1, g2)) + (c1 ++ c2)

                }
                else {
                  val sc = c1 ++ Set(g) ++ c2
                  SC((g1, g2)) = SC((g1, g2)) + sc
                  C((g1, g2)) = apply(C((g1, g2)), SC((g1, g2)) - sc, sc, sc)
                }
              }
            }
          }
        }
      }
      oldC = tempC
      depth += 1
    }
  }

  def getStrongCarriers(cell1 : Cell, cell2 : Cell) :Set[Cell] = {
    var minSet : Set[Cell] = Set()
    var minSize = Int.MaxValue
    for(set <- C((G.find(cell1).get, G.find(cell2).get))){
      if(minSize > set.size){
        minSet = set
        minSize = set.size
      }
    }
    minSet
  }
  def getWeakCarriers(cell1 : Cell, cell2 : Cell) :Set[Cell] = {
    var minSet : Set[Cell] = Set()
    var minSize = Int.MaxValue
    for(set <- SC((G.find(cell1).get, G.find(cell2).get))){
      if(minSize > set.size){
        minSet = set
        minSize = set.size
      }
    }
    minSet
  }
  def newCarrier(oldC: collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]], x : Cell, y : Cell) : Boolean = {

    val a = try{
      oldC((x, y)).size
    }
    catch{
      case _ : Exception => 0
    }
    val b = try{
      C((x, y)).size
    }
    catch{
      case _ : Exception => 0
    }

    return a!=b


  }
  def apply(C_set : Set[Set[Cell]], SC_set : Set[Set[Cell]], union : Set[Cell], intersection: Set[Cell]) : Set[Set[Cell]] = {
    var C_clone = C_set
    for(scl <- SC_set){
      val ul = scl ++ union
      val il = scl & intersection
      if(il.isEmpty){
        C_clone += ul
      }
      else{

       C_clone = apply(C_clone, SC_set - scl, ul, il)
      }
    }
    return C_clone
  }
  def areNearestNeighbours(g1 : Cell, g2: Cell): Boolean ={
    val x1 = g1.i
    val x2 = g2.i
    val y1 = g1.j
    val y2 = g2.j

    var h = (x1 == x2 && y1 == y2+1) || (x1 == x2 && y1 == y2-1) || (y1 == y2 && x1 == x2+1) || (y1 == y2 && x1 == x2-1) || (x1 == x2+1 && y1 == y2+1) || (x1 == x2-1 && y1 == y2-1)
    h = h || (x1 == 0 && x2 == -1) || (x1 == model.N-1 && x2 == model.N) || (y1 == 0 && y2 == -1) || (y1 == model.N-1 && y2 == model.N)
    h = h || (x2 == 0 && x1 == -1) || (x2 == model.N-1 && x1 == model.N) || (y2 == 0 && y1 == -1) || (y2 == model.N-1 && y1 == model.N)
    return h
  }


}