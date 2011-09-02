 
package xöpäx.gen

import noise.Noise.noise3

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

case class Material(color:Int = 0x000000)

object density {

def apply(world:Vec3) = {

def perlinnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def sum2(a:Double, b:Double):Double = {a+b}
def scalesrcv_uid4e4af9b6(scale:Double):Vec3 = {world   * scale}
def scalesrcz_uid4e4af9b6(scale:Double):Double = {world.z * scale}
def perlinnoise3_uid4e4af9b6(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}

val vn2_scalesrcz_uid4e4af9b6 = scalesrcz_uid4e4af9b6(1.0)
val vn2_scalesrcv_uid4e4af9b6 = scalesrcv_uid4e4af9b6(1.0)
val vn1_perlinnoise3_uid4e4af9b6 = perlinnoise3_uid4e4af9b6(vn2_scalesrcv_uid4e4af9b6, 0.0, 0.0, 0.0, 0.0, vn2_scalesrcz_uid4e4af9b6, 0.08717147914690036, 0.5140569133280333, -0.86)
val vn5_perlinnoise3 = perlinnoise3(vn2_scalesrcv_uid4e4af9b6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.23651441168139897, 0.15177436054938087, 0.0)
val vn6_sum2 = sum2(vn5_perlinnoise3, vn1_perlinnoise3_uid4e4af9b6)

vn6_sum2
}

}
