 
package openworld.gen

import noise.Noise.noise3

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

case class Material(color:Int = 0x000000)

object density {

def apply(world:Vec3) = {

def perlinnoise3_uid4e7ca7b6(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def max2_uid4e7ca7b6(a:Double, b:Double):Double = {max(a,b)}
def scalesrcz_uid4e7ca7b6(scale:Double):Double = {world.z * scale}
def scalesrcv_uid4e7ca7b6(scale:Double):Vec3 = {world   * scale}

val vn4_scalesrcz_uid4e7ca7b6 = scalesrcz_uid4e7ca7b6(1.0)
val vn4_scalesrcv_uid4e7ca7b6 = scalesrcv_uid4e7ca7b6(1.0)
val vn2_scalesrcz_uid4e7ca7b6 = scalesrcz_uid4e7ca7b6(1.0)
val vn2_scalesrcv_uid4e7ca7b6 = scalesrcv_uid4e7ca7b6(1.0)
val vn12_perlinnoise3_uid4e7ca7b6 = perlinnoise3_uid4e7ca7b6(vn4_scalesrcv_uid4e7ca7b6, 0.0, 0.0, 0.0, 0.0, vn4_scalesrcz_uid4e7ca7b6, 0.05006686734935137, 4.228072162245522, -0.62)
val vn10_perlinnoise3_uid4e7ca7b6 = perlinnoise3_uid4e7ca7b6(vn2_scalesrcv_uid4e7ca7b6, 0.0, 0.0, 0.0, 0.0, vn2_scalesrcz_uid4e7ca7b6, 0.01651590688377157, 0.8950250709279723, -0.45999999999999996)
val vn11_max2_uid4e7ca7b6 = max2_uid4e7ca7b6(vn10_perlinnoise3_uid4e7ca7b6, vn12_perlinnoise3_uid4e7ca7b6)

vn11_max2_uid4e7ca7b6
}

}
