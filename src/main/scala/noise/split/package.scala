package noise

import simplex3d.math.doublex.functions._
import simplex3d.math.double._

package object split {
  def fastfloor(x:Double) = x.floor.toInt //(if(x > 0) x else (x-1)).toInt
  def hash(k:Int) = mod(((k*34)+1)*k, 289).toInt

  def splitNoise3(v:ReadVec3, split:Int, index:Int):Double = splitNoise3(v.x, v.y, v.z, split, index)
  def splitNoise3(x:Double, y:Double, z:Double, split:Int, index:Int):Double = {
    def grad(hash:Int, x:Double, y:Double, z:Double):Double = {
      if( hash % split != index ) return 0.0
      val t = 1.0-(x*x + y*y + z*z)
      if( t > 0.0 ) t*t
      else 0.0
    }

    val X = fastfloor(x)
    val Y = fastfloor(y)
    val Z = fastfloor(z)

    val relx = x - X
    val rely = y - Y
    val relz = z - Z

    val A = hash(X  )+Y; val AA = hash(A)+Z; val AB = hash(A+1)+Z    // HASH COORDINATES OF
    val B = hash(X+1)+Y; val BA = hash(B)+Z; val BB = hash(B+1)+Z    // THE 8 CUBE CORNERS,

    grad(hash(AA  ), relx  , rely  , relz   ) +
    grad(hash(BA  ), relx-1, rely  , relz   ) +
    grad(hash(AB  ), relx  , rely-1, relz   ) +
    grad(hash(BB  ), relx-1, rely-1, relz   ) +
    grad(hash(AA+1), relx  , rely  , relz-1 ) +
    grad(hash(BA+1), relx-1, rely  , relz-1 ) +
    grad(hash(AB+1), relx  , rely-1, relz-1 ) +
    grad(hash(BB+1), relx-1, rely-1, relz-1 )
  }
}
