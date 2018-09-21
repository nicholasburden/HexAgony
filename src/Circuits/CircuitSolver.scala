package Circuits

class CircuitSolver {

  def getResistance(circuit : Circuit) : Float = {
    val N = circuit.getNodes.size
    val mat : Array[Array[Float]] = Array.ofDim(N)
    for(i <- 0 until N){
      mat(i) = Array.ofDim(N)
      for(j <- 0 until N){
        mat(i)(j) = 0
      }
    }

    val constants : Array[Array[Float]] = Array.ofDim(N)
    for(i <- 0 until N){
      constants(i) = Array.ofDim(1)
      if(i == 0){
        constants(i)(0) = 1
      }

      else{
        constants(i)(0) = 0
      }

    }
    //set V0 to 1
    mat(0)(0) = 1
    for(i <- 1 until N){
      mat(0)(i) = 0
    }
    //set V(N-1) to 0
    mat(N-1)(N-1) = 1
    for(i <- 0 until N-1){
      mat(N-1)(i) = 0
    }


    for (node1 <- circuit.getNodes){
      for(node2 <- circuit.getNodes){
        if(node1.id != node2.id){
          if(circuit.getResistance(node1,node2) != Float.PositiveInfinity && node1.id != 0 && node1.id != (N-1)){
            mat(node1.id)(node1.id) += 1f/circuit.getResistance(node1,node2)
            mat(node1.id)(node2.id) -= 1f/circuit.getResistance(node1,node2)
          }

        }
      }

    }



    val inverted_mat = invert(mat)

    val result : Array[Array[Float]] = Array.ofDim[Float](N,1)
    for(i <- 0 until N){
      for(j <- 0 until 1) {
        for (k <- 0 until N) {
          result(i)(j) = result(i)(j) + inverted_mat(i)(k) * constants(k)(j)
        }
      }
    }



    var current = 0.0
    val lastNode = circuit.getNodes(N-1)
    for(node <- lastNode.getAdjacencies){
      current += (result(node.id)(0) / circuit.getResistance(lastNode, node))
      println(current)
    }

    Math.abs(1f/current).toFloat
  }


  def invert(m : Array[Array[Float]]) : Array[Array[Float]] = {
    var mat = m
    val n = mat.length
    val x  = Array.ofDim[Float](n,n)
    val b  = Array.ofDim[Float](n,n)
    var index = Array.ofDim[Int](n)
    for(i <- 0 until n){
      b(i)(i) = 1
    }
    val tmp = gaussian(mat, index)
    mat = tmp._1
    index = tmp._2
    for(i <- 0 until n-1){
      for(j <- i+1 until n){
        for(k <- 0 until n){
          b(index(j))(k) -= mat(index(j))(i)*b(index(i))(k)
        }
      }
    }
    for(i <- 0 until n){
      x(n-1)(i) = b(index(n-1))(i)/mat(index(n-1))(n-1)
      for(j <- n-2 to 0 by -1){
        x(j)(i) = b(index(j))(i)
        for( k <- j+1 until n){
          x(j)(i) -= mat(index(j))(k)*x(k)(i)
        }
        x(j)(i) /= mat(index(j))(j)
      }

    }

    x

  }


  def gaussian(arr : Array[Array[Float]], ind : Array[Int]) : (Array[Array[Float]], Array[Int]) = {
    val a = arr
    val index = ind
    val n = index.length
    val c : Array[Float] = Array.ofDim[Float](n)
    for(i <- 0 until n){
      index(i) = i
    }

    for(i <- 0 until n){
      var c1 : Float = 0
      for(j <- 0 until n){
        val c0 : Float = Math.abs(a(i)(j))
        if(c0 > c1){
          c1 = c0
        }
      }
      c(i) = c1
    }

    var k = 0
    for(j <- 0 until n-1){
      var pi1 : Float = 0
      for(i <- j until n){
        var pi0 : Float = Math.abs(a(index(i))(j))
        pi0 /= c(index(i))
        if(pi0 > pi1){
          pi1 = pi0
          k = i
        }
      }
      val itmp = index(j)
      index(j) = index(k)
      index(k) = itmp
      for(i <- j+1 until n){
        val pj : Float = a(index(i))(j)/a(index(j))(j)
        a(index(i))(j) = pj
        for(l <- j+1 until n){
          a(index(i))(l) -= pj*a(index(j))(l)
        }
      }
    }
    return (a, index)
  }


}
