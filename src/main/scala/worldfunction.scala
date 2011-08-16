 
package xöpäx.gen

import noise.Noise.noise3

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

case class Material(color:Int = 0x000000)

object density {

def apply(world:Vec3) = {

def max3_uid4e4afc52(a:Double, b:Double, c:Double):Double = {max(max(a,b),c)}
def perlinnoise3_uid4e4afc52(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def createvec3_uid4e4afc52(x:Double, y:Double, z:Double):Vec3 = {Vec3(x,y,z)}
def scalevec3_uid4e4afc52(v:Vec3, x:Double, y:Double, z:Double):Vec3 = {v*Vec3(x,y,z)}
def scalesrcz_uid4e4afc52(scale:Double):Double = {world.z * scale}
def negate_uid4e4afc52(a:Double):Double = {-a}
def scalesrcv_uid4e4afc52(scale:Double):Vec3 = {world   * scale}
def addconstantexp_uid4e4afc52(a:Double, value:Double):Double = {a+value}
def scalesrcy_uid4e4afc52(scale:Double):Double = {world.y * scale}
def min2_uid4e4afc52(a:Double, b:Double):Double = {min(a,b)}
def sphere_uid4e4afc52(v:Vec3, radius:Double):Double = {radius - sqrt(dot(v,v))}
def scalesrcx_uid4e4afc52(scale:Double):Double = {world.x * scale}

val vn14_scalesrcz_uid4e4afc52 = scalesrcz_uid4e4afc52(0.10881882041201557)
val vn30_addconstantexp_uid4e4afc52 = addconstantexp_uid4e4afc52(vn14_scalesrcz_uid4e4afc52, 54.19169999120173)
val vn28_addconstantexp_uid4e4afc52 = addconstantexp_uid4e4afc52(vn30_addconstantexp_uid4e4afc52, 0.16957554093095903)
val vn14_scalesrcy_uid4e4afc52 = scalesrcy_uid4e4afc52(0.10881882041201557)
val vn14_scalesrcx_uid4e4afc52 = scalesrcx_uid4e4afc52(0.10881882041201557)
val vn3_createvec3_uid4e4afc52 = createvec3_uid4e4afc52(vn14_scalesrcx_uid4e4afc52, vn14_scalesrcy_uid4e4afc52, vn28_addconstantexp_uid4e4afc52)
val vn24_scalevec3_uid4e4afc52 = scalevec3_uid4e4afc52(vn3_createvec3_uid4e4afc52, 0.7169776240079135, 0.7169776240079135, 22.315898661606493)
val vn20_sphere_uid4e4afc52 = sphere_uid4e4afc52(vn3_createvec3_uid4e4afc52, 54.19169999120173)
val vn29_sphere_uid4e4afc52 = sphere_uid4e4afc52(vn24_scalevec3_uid4e4afc52, 131.59856981197643)
val vn14_scalesrcv_uid4e4afc52 = scalesrcv_uid4e4afc52(0.10881882041201557)
val vn25_addconstantexp_uid4e4afc52 = addconstantexp_uid4e4afc52(vn20_sphere_uid4e4afc52, 30.124958317193155)
val vn1_perlinnoise3_uid4e4afc52 = perlinnoise3_uid4e4afc52(vn14_scalesrcv_uid4e4afc52, 0.0, 0.0, 0.0, vn29_sphere_uid4e4afc52, 0.0, 0.5743491774985172, 205.0738886629432, -0.43999999999999995)
val vn31_negate_uid4e4afc52 = negate_uid4e4afc52(vn25_addconstantexp_uid4e4afc52)
val vn6_min2_uid4e4afc52 = min2_uid4e4afc52(vn31_negate_uid4e4afc52, vn1_perlinnoise3_uid4e4afc52)
val vn11_perlinnoise3_uid4e4afc52 = perlinnoise3_uid4e4afc52(vn14_scalesrcv_uid4e4afc52, 0.0, 0.0, 0.0, vn20_sphere_uid4e4afc52, 0.0, 0.05593906693299827, 0.5743491774985175, 0.0)
val vn5_max3_uid4e4afc52 = max3_uid4e4afc52(vn11_perlinnoise3_uid4e4afc52, vn20_sphere_uid4e4afc52, vn6_min2_uid4e4afc52)

vn5_max3_uid4e4afc52
}

}
