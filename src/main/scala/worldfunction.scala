 
package xöpäx.gen

import noise.Noise.noise3

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

case class Material(color:Int = 0x000000)

object density {

def apply(world:Vec3) = {
def scalesrcv(scale:Double):Vec3 = {world   * scale}
def perlinnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcz(scale:Double):Double = {world.z * scale}

val vn2_scalesrcz = scalesrcz(1.0)
val vn2_scalesrcv = scalesrcv(1.0)
val vn1_perlinnoise3 = perlinnoise3(vn2_scalesrcv, 0.0, 0.0, 0.0, 0.0, vn2_scalesrcz, 0.014782150730087436, 0.4117955086337865, -0.18000000000000005)

vn1_perlinnoise3
}

}
