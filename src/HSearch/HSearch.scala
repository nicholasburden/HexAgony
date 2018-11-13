package HSearch

import hexagony._
//BIG ISSUE: cells are added to the disjoint sets, and colour is not updated, so it ignores colours. add getCell method to fetch from model instead, and change in search method
class HSearch(val model: Model, colour: Colour) extends Const{



  var Gtemp : Set[Cell] = Set()

  var G : DisjointSets[Cell] = new DisjointSets[Cell]()


  var C : collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]] = collection.mutable.Map()
  var SC : collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]] = collection.mutable.Map()
  var oldC : collection.mutable.Map[(Cell, Cell), Set[Set[Cell]]] = collection.mutable.Map()



  def initial = {
    for(cell <- model.myCells(colour)){
      Gtemp = Gtemp + cell
    }
    for(cell <- model.myCells(O)){
      Gtemp = Gtemp + cell
    }
    if(colour == B){
      Gtemp = Gtemp + HSearch.boundaryBlue1
      Gtemp = Gtemp + HSearch.boundaryBlue2
    }
    else{
      Gtemp = Gtemp + HSearch.boundaryRed1
      Gtemp = Gtemp + HSearch.boundaryRed2
    }
    for(g <- Gtemp){

      G.add(g)
    }

    for(g1 <- Gtemp){

      for(g2 <- Gtemp){
        if(g1.colour.equals(colour) && g2.colour.equals(colour) && areNearestNeighbours(g1, g2)){

          G.union(g1, g2)
        }
      }
    }
    for(g1 <- G.getReps){
      for(g2 <- G.getReps){
        oldC((g1, g2)) = Set()
      }
    }
    val added : collection.mutable.Map[(Cell, Cell), Boolean] = collection.mutable.Map[(Cell, Cell), Boolean]().withDefaultValue(false)
    for(g1 <- Gtemp){

      for(g2 <- Gtemp){
        val rep1 = G.find(g1).get
        val rep2 = G.find(g2).get
        if(!areNearestNeighbours(g1,g2) && !added((rep1, rep2))){
          var op : Option[(Cell, Cell)] = None
          try{
            op = getBridge(g1, g2)
          }catch{
            case e : Exception => e.printStackTrace()
          }

          if(op.isDefined){
            println(g1 + " " + g2 + "  " + op)
            C((rep1, rep2)) = Set(Set(op.get._1, op.get._2))
          }
          else {
            C((rep1, rep2)) = Set()
          }

        }
        else{
          C((rep1, rep2)) = Set(Set())
          added((rep1, rep2)) = true
        }
        SC((rep1, rep2)) = Set()

      }
    }
  }
  def getBridge(cell1 : Cell, cell2 : Cell) : Option[(Cell, Cell)] = {
    var boundary1 = HSearch.boundaryBlue1
    var boundary2 = HSearch.boundaryBlue2
    var indexCheck1 = cell1.i
    var indexCheck2 = cell2.i

    if(colour.equals(R)) {boundary1 = HSearch.boundaryRed1; boundary2 = HSearch.boundaryRed2}
    if((model.pie && colour.equals(B)) || (!model.pie && colour.equals(R))){ indexCheck1 = cell1.j; indexCheck2 = cell2.j}
    def getBoundaryCarrier(a : Cell, b : Cell) : Option[(Cell, Cell)] = {
      if(a.equals(boundary1) && indexCheck2 == 1){
        if((model.pie && colour.equals(R)) || (!model.pie && colour.equals(B))) {
          if (b.i >= 1 && b.j >= 1 && model.board(b.i - 1)(b.j).colour.equals(O) && model.board(b.i - 1)(b.j - 1).colour.equals(O)){
            return Some((model.board(b.i-1)(b.j),model.board(b.i - 1)(b.j - 1)))
          }
        }
        else{
          if (b.i >= 1 && b.j >= 1 && model.board(b.i)(b.j-1).colour.equals(O) && model.board(b.i - 1)(b.j - 1).colour.equals(O)){
            return Some((model.board(b.i)(b.j-1),model.board(b.i - 1)(b.j - 1)))
          }
        }
      }
      if(a.equals(boundary2) && indexCheck2 == model.N - 2){
        if((model.pie && colour.equals(B)) || (!model.pie && colour.equals(R))) {
          if (b.i < model.N - 1 && b.j < model.N-1 && model.board(b.i)(b.j+1).colour.equals(O) && model.board(b.i + 1)(b.j + 1).colour.equals(O)){
            return Some((model.board(b.i)(b.j+1),model.board(b.i + 1)(b.j + 1)))
          }
        }
        else{
          if (b.i < model.N-1 && b.j < model.N - 1 && model.board(b.i+1)(b.j).colour.equals(O) && model.board(b.i + 1)(b.j + 1).colour.equals(O)){
            return Some((model.board(b.i+1)(b.j),model.board(b.i + 1)(b.j + 1)))
          }
        }
      }
      return None

    }
    val possibleCarrier1 = getBoundaryCarrier(cell1, cell2)
    val possibleCarrier2 = getBoundaryCarrier(cell2, cell1)
    if(possibleCarrier1.isDefined) return possibleCarrier1
    if(possibleCarrier2.isDefined) return possibleCarrier2
    var a = new Cell(-1,-1)
    var b = new Cell(-1,-1)
    var found = false
    if(cell1.i == cell2.i-1 && cell1.j == cell2.j-2){
      a = model.board(cell1.i)(cell1.j+1)
      b = model.board(cell2.i)(cell2.j-1)
      found = true
    }

    if(cell1.i == cell2.i-2 && cell1.j == cell2.j-1){
      a = model.board(cell1.i+1)(cell1.j)
      b = model.board(cell2.i-1)(cell2.j)
      found = true
    }
    if (cell1.i == cell2.i-1 && cell1.j == cell2.j+1){
      a = model.board(cell1.i)(cell2.j)
      b = model.board(cell2.i)(cell1.j)
      found = true
    }
    if (cell1.i == cell2.i+1 && cell1.j == cell2.j+2){
      a = model.board(cell1.i)(cell1.j-1)
      b = model.board(cell2.i)(cell2.j+1)
      found = true
    }
    if(cell1.i == cell2.i+2 && cell1.j == cell2.j+1){
      a = model.board(cell1.i-1)(cell1.j)
      b = model.board(cell2.i+1)(cell2.j)
      found = true
    }
    if(cell1.i == cell2.i+1 && cell1.j == cell2.j-1){
      a = model.board(cell1.i)(cell2.j)
      b = model.board(cell2.i)(cell1.j)
      found = true
    }
    if(found && a.colour.equals(O) && b.colour.equals(O)){
      return Some((a,b))
    }
    return None
  }
  def makeMove(i : Int, j : Int, c : Colour): HSearch = {
    val cell = model.board(i)(j)

    val mod2 = result(model, cell, c)
    val hsearch = new HSearch(mod2, colour)
    val newG = G.clone()
    hsearch.G = newG.asInstanceOf[DisjointSets[Cell]]

    //if(colour.equals(c)) hsearch.G.add(mod2.board(cell.i)(cell.j))
    if(!colour.equals(c)) hsearch.G.remove(mod2.board(cell.i)(cell.j))
    try {
      var set : Set[Cell] = Set()
      if(colour.equals(R)) set = Set(HSearch.boundaryRed1, HSearch.boundaryRed2)
      else set = Set(HSearch.boundaryBlue1, HSearch.boundaryBlue2)
      for (cellTemp: Cell <- mod2.neighbours(mod2.board(cell.i)(cell.j))) {
        if (cellTemp.colour.equals(c) && colour.equals(c)) {

          hsearch.G.union(cellTemp, mod2.board(cell.i)(cell.j))
        }
      }
      for(cellTemp <- set){
        if(areNearestNeighbours(mod2.board(cell.i)(cell.j), cellTemp) && colour.equals(c)){
          hsearch.G.union(cellTemp, mod2.board(cell.i)(cell.j))
        }

      }
    }
    catch{
      case e : Exception => e.printStackTrace()
    }

    for(g1 <- hsearch.G.getReps){
      for(g2 <- hsearch.G.getReps){
        hsearch.oldC((g1, g2)) = Set()
      }
    }


    val C_clone = C.clone()
    val SC_clone = SC.clone()

    hsearch.C = C_clone
    hsearch.SC = SC_clone
    var set : Set[Cell] = Set()
    if(colour.equals(R)){
      set = Set(HSearch.boundaryRed1, HSearch.boundaryRed2)
    }
    else{
      set = Set(HSearch.boundaryBlue1, HSearch.boundaryBlue2)
    }
    try {
      for (cell1 <- mod2.myCells(colour) ++ mod2.myCells(O) ++ set) {
        for (cell2 <- mod2.myCells(colour) ++ mod2.myCells(O) ++ set) {
          val strongCarriers = getStrongCarriers(cell1, cell2, true)
          val weakCarriers = getWeakCarriers(cell1, cell2, true)

          if (!c.equals(colour) && strongCarriers.contains(cell)) {
            hsearch.SC((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set(hsearch.getStrongCarriers(cell1, cell2, true) - cell)
            hsearch.C((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set()
          }
          else if (c.equals(colour) && strongCarriers.contains(cell)) {
            hsearch.C((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set(hsearch.getStrongCarriers(cell1, cell2, true) - cell)
            if (hsearch.getStrongCarriers(hsearch.G.find(cell1).get, hsearch.G.find(cell2).get, true).size == 1) {
              hsearch.C((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set()

            }

          }
          else if (!c.equals(colour) && weakCarriers.contains(cell)) {

            hsearch.SC((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set()

          }
          else if (c.equals(colour) && weakCarriers.contains(cell)) {

            hsearch.C((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set(hsearch.getWeakCarriers(cell1, cell2, true) - cell)
            hsearch.SC((hsearch.G.find(cell1).get, hsearch.G.find(cell2).get)) = Set()
          }
        }
      }
    }
    catch{
      case e : Exception => e.printStackTrace()
    }
        return hsearch
  }


  def search : Unit = {

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
  }

  def getStrongCarriers(cell1 : Cell, cell2 : Cell, getAll : Boolean) :Set[Cell] = {

    var minSet : Set[Cell] = Set()
    var minSize = Int.MaxValue
    if(cell1.equals(cell2)) return Set()


    if(!((cell1.colour.equals(colour) && cell2.colour.equals(colour)) || (cell1.colour.equals(colour) && cell2.colour.equals(O)) || (cell2.colour.equals(colour) && cell1.colour.equals(O)) || (cell1.colour.equals(O) && cell2.colour.equals(O)))){

      return Set()
    }
    if(getAll){
      for(set <- C((G.find(cell1).get, G.find(cell2).get))){

        if(minSize > set.size){
          minSet = set
          minSize = set.size
        }
      }
      return minSet
    }
    else{
      if(cell1.colour.equals(colour) || cell2.colour.equals(colour)){

        for(set <- C((G.find(cell1).get, G.find(cell2).get))){

          if(minSize > set.size){
            minSet = set
            minSize = set.size
          }
        }
      }
      return minSet
    }

  }
  def getWeakCarriers(cell1 : Cell, cell2 : Cell, getAll : Boolean) :Set[Cell] = {

    var minSet : Set[Cell] = Set()
    var minSize = Int.MaxValue
    if(cell1.equals(cell2)) return Set()

    if(!((cell1.colour.equals(colour) && cell2.colour.equals(colour)) || (cell1.colour.equals(colour) && cell2.colour.equals(O)) || (cell2.colour.equals(colour) && cell1.colour.equals(O)) || (cell1.colour.equals(O) && cell2.colour.equals(O)))){

      return Set()
    }
    if(getAll){
      for(set <- SC((G.find(cell1).get, G.find(cell2).get))){

        if(minSize > set.size){
          minSet = set
          minSize = set.size
        }
      }
      return minSet
    }
    else{
      if(cell1.colour.equals(colour) || cell2.colour.equals(colour)){
        for(set <- SC((G.find(cell1).get, G.find(cell2).get))){

          if(minSize > set.size){
            minSet = set
            minSize = set.size
          }
        }
      }
      return minSet
    }

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
      if(il.isEmpty && ul.size <= HSearch.M){
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
    h = h || (x1 == 0 && x2 == -1) || (x1 == model.N-1 && x2 == -3) || (y1 == 0 && y2 == -1) || (y1 == model.N-1 && y2 == -3)
    h = h || (x2 == 0 && x1 == -1) || (x2 == model.N-1 && x1 == -3) || (y2 == 0 && y1 == -1) || (y2 == model.N-1 && y1 == -3)
    return h
  }

  private def result(mod: Model, cell: Cell, col: Colour): Model = {
    val mod2 = mod.copy()
    mod2.playMove(cell, col)
    return mod2

  }


}

object HSearch extends Const{
  val M = 12
  val X = 5
  var boundaryRed1 : Cell = new Cell(0, -1)
  var boundaryRed2 : Cell = new Cell(0, -3)
  var boundaryBlue1 : Cell = new Cell(-1, 0)
  var boundaryBlue2 : Cell = new Cell(-3, 0)
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


}

