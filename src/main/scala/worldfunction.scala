 
package xöpäx.gen

import noise.Noise.noise3

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

case class Material(color:Int = 0x000000)

object density {

def apply(world:Vec3) = {
def max2(a:Double, b:Double):Double = {max(a,b)}
def perlinnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcy(scale:Double):Double = {world.y * scale}
def diff2(a:Double, b:Double):Double = {a-b}
def addconstantexp(a:Double, value:Double):Double = {a+value}
def createvec3(x:Double, y:Double, z:Double):Vec3 = {Vec3(x,y,z)}
def sphere(v:Vec3, radius:Double):Double = {radius-sqrt(dot(v,v))}
def multiplyconstantexp(a:Double, value:Double):Double = {a*value}
def scalesrcx(scale:Double):Double = {world.x * scale}
def min2(a:Double, b:Double):Double = {min(a,b)}
def scalesrcz(scale:Double):Double = {world.z * scale}
def scalesrcv(scale:Double):Vec3 = {world   * scale}

val vn1_scalesrcz = scalesrcz(0.10881882041201557)
val vn2_addconstantexp = addconstantexp(vn1_scalesrcz, 54.19169999120173)
val vn42_addconstantexp = addconstantexp(vn2_addconstantexp, 0.16957554093095903)
val vn1_scalesrcy = scalesrcy(0.10881882041201557)
val vn1_scalesrcx = scalesrcx(0.10881882041201557)
val vn17_multiplyconstantexp = multiplyconstantexp(vn42_addconstantexp, 22.315898661606493)
val vn15_multiplyconstantexp = multiplyconstantexp(vn1_scalesrcy, 0.7169776240079135)
val vn14_multiplyconstantexp = multiplyconstantexp(vn1_scalesrcx, 0.7169776240079135)
val vn16_createvec3 = createvec3(vn1_scalesrcx, vn1_scalesrcy, vn42_addconstantexp)
val vn6_createvec3 = createvec3(vn14_multiplyconstantexp, vn15_multiplyconstantexp, vn17_multiplyconstantexp)
val vn21_sphere = sphere(vn16_createvec3, 54.19169999120173)
val vn13_sphere = sphere(vn6_createvec3, 84.44850628946526)
val vn1_scalesrcv = scalesrcv(0.10881882041201557)
val vn3_addconstantexp = addconstantexp(vn21_sphere, 31.124958317193155)
val vn43_perlinnoise3 = perlinnoise3(vn1_scalesrcv, 0.0, 0.0, 0.0, vn13_sphere, 0.0, 0.5743491774985172, 183.54627174602587, -0.43999999999999995)
val vn4_diff2 = diff2(0.0, vn3_addconstantexp)
val vn5_perlinnoise3 = perlinnoise3(vn1_scalesrcv, 0.0, 0.0, 0.0, vn21_sphere, 0.0, 0.05593906693299827, 0.5743491774985175, 0.0)
val vn10_min2 = min2(vn4_diff2, vn43_perlinnoise3)
val vn20_max2 = max2(vn5_perlinnoise3, vn21_sphere)
val vn8_max2 = max2(vn20_max2, vn10_min2)

vn8_max2
}

}
