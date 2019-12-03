  /**
  *   Lsd radix sort. Learned since to use more informative names than i, j and a as indices for counters.
  *
  *
  */
  
  
  
  /**
   * Least-significant-digit-first radix sort for integer arrays.
   * Sorts the argument array in ascending order.
   * Assumes that all the integers are non-negative.
   * Interpret 'digit' as 'byte' (8 bits) here and use bit-level operations
   *  such as shifting (>> and <<) and masking (&) to extract 'digits'.
   */


def lsdRadixSort(a: Array[Int]): Unit = {
    val N = a.length
    if(N <= 1) return

    var i = 0
    val rounds = 4
    val filter = 255
    
    while(i < rounds)
    {
      
      val auxArray: Array[Int] = Array.ofDim(N)
      var j = 0
      val count: Array[Int] = Array.ofDim[Int](256)
      while(j < N)
      {
        val number = a(j)
        val binNumber = (number >> 8*i) & 255 // modulo to get the correct 8 bits
        count(binNumber) += 1
        j += 1
      }
      if(count(0) != N) {
        count(0) -= 1
        
        var z = 1
        while(z < 256)
        {
          count(z) = count(z - 1) + count (z)
          z += 1
        }
        var k = N - 1
        while(k >= 0)
        {
          val number = a(k)
          val binNumber = (number >> 8*i) & 255
          val index = count(binNumber)
          auxArray(index) = number
          count(binNumber) -= 1
          k -= 1
        }
        auxArray.copyToArray(a)
      }
      i += 1
    }
    
  }
