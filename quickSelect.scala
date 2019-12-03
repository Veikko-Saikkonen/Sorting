  /**
  *   Again, messy code but works.
  *   Algorithm is the same as in quick sort, but quick select stops when it finds the k:th smallest element.
  *
  */
  
  
  /**
   * Find the k:th smallest (0 for the smallest) element in
   * the integer sequence seq by using the quickselect algorithm.
   */
  def find(seq: Seq[Int], k: Int): Int = {
    require(0 <= k && k < seq.length)
    // Make a working copy of the sequence as we cannot modify the argument sequence
    val a: Array[Int] = seq.toArray
    
    
    def swap(az:Array[Int], i:Int, j: Int): Unit ={
      val t = az(i)
      az(i) = az(j)
      az(j) = t
    }
    
    def partition(ax: Array[Int], lo: Int, hi: Int): (Int, Int) = {
      
      val randIndex = lo + rand.nextInt(hi - lo) + 1
      
      val pivot = ax(randIndex)
      swap(ax, hi, randIndex)
      
      var i = lo
      var gt = hi
      var lt = lo
      
      while(i <= gt) {
        
        while(i < gt && ax(i) < pivot) {
          lt += 1
          i += 1
        }
        while(i < gt && ax(i) <= pivot){
          
          if(ax(i) < pivot) 
            {
            swap(ax, i, lt)
            lt += 1
            }
          
          i += 1
        }
        do
          gt -= 1
         while (i <= gt && ax(gt) > pivot)
           
        if(i<gt)
          swap(ax, i, gt)
      }
      swap(ax, hi, i)
      (lt, i)
    }
    
    def myFunction(a: Array[Int], left: Int, right: Int): Int = {
      if(left == right)
      {
        (a(left))
      }
      else 
      {
        val index = partition(a, left, right)
        
        if(k == index._2)
        {
          a(k)
        } else 
          if(k < index._2)
          {
            if(k > index._1) a(index._2)
            else
            myFunction(a, left, index._2-1)
          } else
          {
            myFunction(a, index._2 + 1, right)
          }
      }
    }
    myFunction(a, 0, a.length - 1)
  }
}
