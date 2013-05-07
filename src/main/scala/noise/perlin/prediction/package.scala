package noise.perlin

package object prediction {
  // Split Bezier Curves with Decasteljau Algorithm
  def splitleft(h:Array[Double],t:Double):Array[Double] = {
    val n = h.size - 1
    val result = h.clone
    var tmp = 0.0
    var save = 0.0

    var i = 0
    var j = 0
    val nm1 = n-1
    while( i <= nm1 ) {
      tmp = result(i)
      j = i+1
      while( j <= n ) {
        save = lerp(t, tmp, result(j))
        tmp = result(j)
        result(j) = save
        j += 1
      }
      i += 1
    }

    result
  }

  def splitright(h:Array[Double],t1:Double):Array[Double] = {
    import collection.immutable.Range.inclusive
    val t = 1-t1
    val n = h.size - 1
    val result = h.clone
    var tmp = 0.0
    var save = 0.0

    var i = 0
    var j = 0
    val nm1 = n-1

    while( i <= nm1 ) {
      tmp = result(n-i)
      j = nm1-i
      while( j >= 0 ) {
        save = lerp(t, tmp, result(j))
        tmp = result(j)
        result(j) = save
        j -= 1
      }
      i += 1
    }

    result
  }

  def slice(h:Array[Double], t0:Double, t1:Double) = {
    if( t1 == 0 )
      splitleft(splitright(h,t0),-t0/(1-t0))
    else
      splitright(splitleft(h,t1),t0/t1)
  }
}
