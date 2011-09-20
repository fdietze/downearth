 
package xöpäx.gen

import noise.Noise.noise3

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

case class Material(color:Int = 0x000000)

object density {

def apply(world:Vec3) = {

def scalesrcv_uid4e4b916c(scale:Double):Vec3 = {world   * scale}
def scalesrcx_uid4e4b916c(scale:Double):Double = {world.x * scale}
def min2_uid4e4b916c(a:Double, b:Double):Double = {min(a,b)}
def scalesrcz_uid4e4b916c(scale:Double):Double = {world.z * scale}
def scalesrcy_uid4e4b916c(scale:Double):Double = {world.y * scale}
def sphere_uid4e4b916c(v:Vec3, radius:Double):Double = {radius - sqrt(dot(v,v))}
def max3_uid4e4b916c(a:Double, b:Double, c:Double):Double = {max(max(a,b),c)}
def addconstantexp_uid4e4b916c(a:Double, value:Double):Double = {a+value}
def negate_uid4e4b916c(a:Double):Double = {-a}
def scalevec3_uid4e4b916c(v:Vec3, x:Double, y:Double, z:Double):Vec3 = {v*Vec3(x,y,z)}
def perlinnoise3_uid4e4b916c(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def createvec3_uid4e4b916c(x:Double, y:Double, z:Double):Vec3 = {Vec3(x,y,z)}

val vn3_scalesrcz_uid4e4b916c = scalesrcz_uid4e4b916c(0.10881882041201557)
val vn17_addconstantexp_uid4e4b916c = addconstantexp_uid4e4b916c(vn3_scalesrcz_uid4e4b916c, 54.19169999120173)
val vn8_addconstantexp_uid4e4b916c = addconstantexp_uid4e4b916c(vn17_addconstantexp_uid4e4b916c, 0.16957554093095903)
val vn3_scalesrcy_uid4e4b916c = scalesrcy_uid4e4b916c(0.10881882041201557)
val vn3_scalesrcx_uid4e4b916c = scalesrcx_uid4e4b916c(0.10881882041201557)
val vn23_createvec3_uid4e4b916c = createvec3_uid4e4b916c(vn3_scalesrcx_uid4e4b916c, vn3_scalesrcy_uid4e4b916c, vn8_addconstantexp_uid4e4b916c)
val vn24_scalevec3_uid4e4b916c = scalevec3_uid4e4b916c(vn23_createvec3_uid4e4b916c, 0.7169776240079135, 0.7169776240079135, 22.315898661606493)
val vn16_sphere_uid4e4b916c = sphere_uid4e4b916c(vn23_createvec3_uid4e4b916c, 54.19169999120173)
val vn2_sphere_uid4e4b916c = sphere_uid4e4b916c(vn24_scalevec3_uid4e4b916c, 131.59856981197643)
val vn3_scalesrcv_uid4e4b916c = scalesrcv_uid4e4b916c(0.10881882041201557)
val vn7_addconstantexp_uid4e4b916c = addconstantexp_uid4e4b916c(vn16_sphere_uid4e4b916c, 30.124958317193155)
val vn22_perlinnoise3_uid4e4b916c = perlinnoise3_uid4e4b916c(vn3_scalesrcv_uid4e4b916c, 0.0, 0.0, 0.0, vn2_sphere_uid4e4b916c, 0.0, 0.7169776240079135, 256.0, -0.56)
val vn19_negate_uid4e4b916c = negate_uid4e4b916c(vn7_addconstantexp_uid4e4b916c)
val vn30_min2_uid4e4b916c = min2_uid4e4b916c(vn19_negate_uid4e4b916c, vn22_perlinnoise3_uid4e4b916c)
val vn1_perlinnoise3_uid4e4b916c = perlinnoise3_uid4e4b916c(vn3_scalesrcv_uid4e4b916c, 0.0, 0.0, 0.0, vn16_sphere_uid4e4b916c, 0.0, 0.05593906693299827, 0.2642545101403451, 0.0)
val vn28_max3_uid4e4b916c = max3_uid4e4b916c(vn1_perlinnoise3_uid4e4b916c, vn16_sphere_uid4e4b916c, vn30_min2_uid4e4b916c)

vn28_max3_uid4e4b916c
}

}
