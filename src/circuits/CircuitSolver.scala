package circuits
import heuristic._
import Jama._
class CircuitSolver {

  def getResistance(circuit : Circuit) : Double = {

    //Method to decide if a node has connections to other nodes of finite resistance
    def notIsolated(n : Node):Boolean = {
      for(n2 <- n.getAdjacencies){
        if(!circuit.getResistance(n, n2).isInfinite){
          return true
        }
      }
      return false
    }

    var N = circuit.nodes.size

    //Delete any isolated nodes, not useful to us
    for(i <- N-1 to 0 by -1){
      if(!notIsolated(circuit.getNodes(i))){
        circuit.deleteNode(i)
      }
    }
    N = circuit.nodes.size


    //Formulate matrix system to solve Kirchoff's equations
    val mat : Matrix = new Matrix(N, N)

    val constants : Matrix = new Matrix(N, 1)
    constants.set(0,0,1)

    //set V0 to 1
    mat.set(0,0,1)
    //set V(N-1) to 1
    mat.set(N-1, N-1, 1)

    for (node1 <- circuit.getNodes){
      for(node2 <- circuit.getNodes){
        if(node1.id != node2.id){
          if(circuit.getResistance(node1,node2) != Double.PositiveInfinity && node1.id != 0 && node1.id != (N-1)){
            if(circuit.getResistance(node1, node2)==0){
              //Set to negligible value to avoid dealing with 0
              circuit.setResistance(node1, node2, ResistanceHeuristic.epsilon)
            }

            val r = 1d/circuit.getResistance(node1,node2)
            val v1 = mat.get(node1.id, node1.id)
            val v2 = mat.get(node1.id, node2.id)

            //Kirchoff's equations
            mat.set(node1.id, node1.id, v1+r)
            mat.set(node1.id, node2.id, v2-r)
          }
        }
      }
    }

    try {

      //Solve equation
      val result: Matrix = mat.solve(constants)

      //Tally up current
      var current = 0.0
      val lastNode = circuit.getNodes(N - 1)
      for (node <- lastNode.getAdjacencies) {
        if (circuit.getResistance(lastNode, node) == 0) {
          //Avoid dealing with 0
          circuit.setResistance(lastNode, node, ResistanceHeuristic.epsilon)
        }
        if (!circuit.getResistance(lastNode, node).isInfinite) {
          current += (result.get(node.id, 0) / circuit.getResistance(lastNode, node))
        }
      }

      //Calculate resistance
      Math.abs(1d / current)
    }catch{
      case _ : Exception => 0.0
    }
  }
}
