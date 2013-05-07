 
package downearth.gen

import noise.perlin.{simple => noise3}

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

// case class Material(color:Int = 0x000000)

object density {

def apply(world:Vec3) = {

def scalesrcv_uid4e7b7b1d(scale:Double):Vec3 = {world   * scale}
def scalesrcz_uid4e7b7b1d(scale:Double):Double = {world.z * scale}
def perlinnoise3_uid4e7b7b1d(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}

val vn2_scalesrcz_uid4e7b7b1d = scalesrcz_uid4e7b7b1d(0.12158186842653576)
val vn2_scalesrcv_uid4e7b7b1d = scalesrcv_uid4e7b7b1d(0.12158186842653576)
val vn1_perlinnoise3_uid4e7b7b1d = perlinnoise3_uid4e7b7b1d(vn2_scalesrcv_uid4e7b7b1d, 0.0, 0.0, 0.0, 0.0, vn2_scalesrcz_uid4e7b7b1d, 1.0, 0.4600938253124375, 0.0)

vn1_perlinnoise3_uid4e7b7b1d
}

}
