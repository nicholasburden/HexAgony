package graph

class NetworkFlow(g : Graph) {
  val nodes = g.getNodes
  val V = nodes.length
  val graph = Array.ofDim[Array[Int]](V)
  for(i <- 0 until V){
    graph(i) = Array.ofDim[Int](V)
    for(j <- 0 until V){
      if(nodes(i).adjacentTo(j)){
        graph(i)(j) = cap(i, j)
      }
      else{
        graph(i)(j) = 0
      }
    }
  }
  def maxFlow : Int = fordFulkerson(graph, 0, V - 1)

  def cap(i : Int, j : Int) : Int = {
    if(i != 0 && j != 0 && i != (V-1) && j != (V-1)){
      return 1
    }
    else if(i == 0 || j == (V-1)){
      return Int.MaxValue
    }
    return 0

  }
  def bfs(rGraph : Array[Array[Int]], s : Int, t : Int, parent : Array[Int]) : Boolean = {
    val visited = Array.ofDim[Boolean](V)
    for(i <- 0 until V){
      visited(i) = false
    }
    val queue = new scala.collection.mutable.Queue[Int]()
    queue.enqueue(s)
    visited(s) = true
    parent(s) = -1
    while(!queue.isEmpty){
      val u = queue.dequeue()
      for(v <- 0 until V){
        if(!visited(v) && rGraph(u)(v) > 0){
          queue.enqueue(v)
          parent(v) = u
          visited(v) = true
        }
      }
    }
    return visited(t)
  }

  def fordFulkerson(graph : Array[Array[Int]], s : Int, t : Int) : Int = {
    var u, v = 0
    val rGraph = Array.ofDim[Array[Int]](V)
    for(u <- 0 until V){
      rGraph(u) = Array.ofDim[Int](V)
      for(v <- 0 until V){
        rGraph(u)(v) = graph(u)(v)
      }
    }
    val parent = Array.ofDim[Int](V)
    var maxFlow = 0
    while(bfs(rGraph, s, t, parent)){
      var pathFlow = Int.MaxValue
      v = t
      while(v != s){
        u = parent(v)
        pathFlow = Math.min(pathFlow, rGraph(u)(v))
        v = parent(v)
      }
      v = t
      while(v != s){
        u = parent(v)
        rGraph(u)(v) -= pathFlow
        rGraph(v)(u) += pathFlow
        v = parent(v)
      }
      maxFlow += pathFlow
    }
    maxFlow
  }
}
