 
package openworld.gen

import noise.Noise.noise3

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

case class Material(color:Int = 0x000000)

object density {

def apply(world:Vec3) = {

def perlinnoise3_uid4e7cbecf(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcz_uid4e7cbecf(scale:Double):Double = {world.z * scale}
def max2_uid4e7cbecf(a:Double, b:Double):Double = {max(a,b)}
def scalesrcv_uid4e7cbecf(scale:Double):Vec3 = {world   * scale}

val vn8_scalesrcz_uid4e7cbecf = scalesrcz_uid4e7cbecf(1.0)
val vn8_scalesrcv_uid4e7cbecf = scalesrcv_uid4e7cbecf(1.0)
val vn2_scalesrcz_uid4e7cbecf = scalesrcz_uid4e7cbecf(1.0)
val vn2_scalesrcv_uid4e7cbecf = scalesrcv_uid4e7cbecf(1.0)
val vn6_perlinnoise3_uid4e7cbecf = perlinnoise3_uid4e7cbecf(vn8_scalesrcv_uid4e7cbecf, 0.0, 0.0, 0.0, 0.0, vn8_scalesrcz_uid4e7cbecf, 0.05006686734935137, 4.228072162245522, -0.62)
val vn1_perlinnoise3_uid4e7cbecf = perlinnoise3_uid4e7cbecf(vn2_scalesrcv_uid4e7cbecf, 0.0, 0.0, 0.0, 0.0, vn2_scalesrcz_uid4e7cbecf, 0.01651590688377157, 0.8950250709279723, -0.45999999999999996)
val vn16_max2_uid4e7cbecf = max2_uid4e7cbecf(vn1_perlinnoise3_uid4e7cbecf, vn6_perlinnoise3_uid4e7cbecf)

vn16_max2_uid4e7cbecf
}

}
