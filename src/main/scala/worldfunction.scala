 
package xöpäx.gen

import noise.Noise.noise3

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

case class Material(color:Int = 0x000000)

object density {

def apply(world:Vec3) = {

def perlinnoise3_uid4e563a9c(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcz_uid4e563a9c(scale:Double):Double = {world.z * scale}
def scalesrcv_uid4e563a9c(scale:Double):Vec3 = {world   * scale}

val vn9_scalesrcv_uid4e563a9c = scalesrcv_uid4e563a9c(0.18946457081379972)
val vn9_scalesrcz_uid4e563a9c = scalesrcz_uid4e563a9c(0.18946457081379972)
val vn11_perlinnoise3_uid4e563a9c = perlinnoise3_uid4e563a9c(vn9_scalesrcv_uid4e563a9c, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2642545101403451, 6.588728138140588, 0.0)
val vn8_perlinnoise3_uid4e563a9c = perlinnoise3_uid4e563a9c(vn9_scalesrcv_uid4e563a9c, vn11_perlinnoise3_uid4e563a9c, vn11_perlinnoise3_uid4e563a9c, vn11_perlinnoise3_uid4e563a9c, 0.0, vn9_scalesrcz_uid4e563a9c, 0.07802065930635076, 1.0, -0.18000000000000005)

vn8_perlinnoise3_uid4e563a9c
}

}
