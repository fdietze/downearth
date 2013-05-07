package noise

import simplex3d.math.doublex.functions._
import simplex3d.math.double._

package object perlin {
  def fastfloor(x:Double) = x.floor.toInt //(if(x > 0) x else (x-1)).toInt
  def fastceil(x:Double) = x.ceil.toInt
  def fadeSimple(t:Double) = t * t * (3 - 2*t)
  def fadeImproved(t:Double) = t * t * t * (t * (t * 6 - 15) + 10)
  def lerp(t:Double, a:Double, b:Double) = a + t * (b - a)
  def hash(k:Int) = mod(((k*34)+1)*k, 289).toInt

  def improved(v:Vec3):Double = improved(v.x, v.y, v.z)
  def improved(x:Double, y:Double, z:Double):Double = noise3(x, y, z, fadeImproved _)

  def simple(v:Vec3):Double = simple(v.x, v.y, v.z)
  def simple(x:Double, y:Double, z:Double):Double = noise3(x, y, z, fadeSimple _)

  def noise3(x:Double, y:Double, z:Double, fade:Double => Double):Double = {
    def grad(hash:Int, x:Double, y:Double, z:Double) = {
      val h = hash & 15
      val u = if(h<8) x else y
      val v = if(h<4) y else {if(h==12 || h==14) x else z}
      (if((h&1) == 0) u else -u) + (if((h&2) == 0) v else -v)
    }

    val X = fastfloor(x)
    val Y = fastfloor(y)
    val Z = fastfloor(z)

    val relx = x - X
    val rely = y - Y
    val relz = z - Z

    val u = fade(relx)
    val v = fade(rely)
    val w = fade(relz)

    val A = hash(X  )+Y; val AA = hash(A)+Z; val AB = hash(A+1)+Z    // HASH COORDINATES OF
    val B = hash(X+1)+Y; val BA = hash(B)+Z; val BB = hash(B+1)+Z    // THE 8 CUBE CORNERS,

    lerp(w,  lerp(v, lerp(u, grad(hash(AA  ), relx  , rely  , relz   ),  // AND ADD
                             grad(hash(BA  ), relx-1, rely  , relz   )), // BLENDED
                     lerp(u, grad(hash(AB  ), relx  , rely-1, relz   ),  // RESULTS
                             grad(hash(BB  ), relx-1, rely-1, relz   ))),// FROM  8
             lerp(v, lerp(u, grad(hash(AA+1), relx  , rely  , relz-1 ),  // CORNERS
                             grad(hash(BA+1), relx-1, rely  , relz-1 )), // OF CUBE
                     lerp(u, grad(hash(AB+1), relx  , rely-1, relz-1 ),
                             grad(hash(BB+1), relx-1, rely-1, relz-1 ))))
  }

  val gradients3 = Array(
    Vec3( 1, 1, 0),
    Vec3(-1, 1, 0),
    Vec3( 1,-1, 0),
    Vec3(-1,-1, 0),

    Vec3( 1, 0, 1),
    Vec3(-1, 0, 1),
    Vec3( 1, 0,-1),
    Vec3(-1, 0,-1),

    Vec3( 0, 1, 1),
    Vec3( 0,-1, 1),
    Vec3( 0, 1,-1),
    Vec3( 0,-1,-1),

    Vec3( 1, 1, 0),
    Vec3( 0,-1, 1),
    Vec3(-1, 1, 0),
    Vec3( 0,-1,-1)
  )

  def gradientAt3(X:Int, Y:Int, Z:Int) = {
    gradients3(hash(hash(hash(X)+Y)+Z) & 15)
  }
}
