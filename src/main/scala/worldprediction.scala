 
package xöpäx.gen

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import noise.Noise.noise3_prediction
import noise.interval
import noise.interval.{Interval, Volume}

object prediction {

def apply(world:Volume) = {

def scalesrcy_uid4e4afc52(scale:Double):Interval = {world.y * scale}
def scalevec3_uid4e4afc52(v:Volume, x:Double, y:Double, z:Double):Volume = {v*Volume(Vec3(x,y,z))}
def negate_uid4e4afc52(a:Interval):Interval = {-a}
def scalesrcx_uid4e4afc52(scale:Double):Interval = {world.x * scale}
def perlinnoise3_uid4e4afc52(v:Volume, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {(noise3_prediction((v + Volume(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcz_uid4e4afc52(scale:Double):Interval = {world.z * scale}
def max3_uid4e4afc52(a:Interval, b:Interval, c:Interval):Interval = {interval.max(interval.max(a,b),c)}
def scalesrcv_uid4e4afc52(scale:Double):Volume = {world   * scale}
def min2_uid4e4afc52(a:Interval, b:Interval):Interval = {interval.min(a,b)}
def sphere_uid4e4afc52(v:Volume, radius:Double):Interval = {-interval.length(v) + radius}
def addconstantexp_uid4e4afc52(a:Interval, value:Double):Interval = {a+value}
def createvec3_uid4e4afc52(x:Interval, y:Interval, z:Interval):Volume = {Volume(x,y,z)}

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
val vn1_perlinnoise3_uid4e4afc52 = perlinnoise3_uid4e4afc52(vn14_scalesrcv_uid4e4afc52, Interval(0,0), Interval(0,0), Interval(0,0), vn29_sphere_uid4e4afc52, Interval(0,0), 0.7169776240079135, 256.0, -0.56)
val vn31_negate_uid4e4afc52 = negate_uid4e4afc52(vn25_addconstantexp_uid4e4afc52)
val vn6_min2_uid4e4afc52 = min2_uid4e4afc52(vn31_negate_uid4e4afc52, vn1_perlinnoise3_uid4e4afc52)
val vn11_perlinnoise3_uid4e4afc52 = perlinnoise3_uid4e4afc52(vn14_scalesrcv_uid4e4afc52, Interval(0,0), Interval(0,0), Interval(0,0), vn20_sphere_uid4e4afc52, Interval(0,0), 0.05593906693299827, 0.2642545101403451, 0.0)
val vn5_max3_uid4e4afc52 = max3_uid4e4afc52(vn11_perlinnoise3_uid4e4afc52, vn20_sphere_uid4e4afc52, vn6_min2_uid4e4afc52)

vn5_max3_uid4e4afc52
}

}
