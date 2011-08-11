 
package xöpäx.gen

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import noise.Noise.noise3_prediction
import noise.interval
import noise.interval.{Interval, Volume}

object prediction {

def apply(world:Volume) = {

def scalevec3_uid4e428756(v:Volume, x:Double, y:Double, z:Double):Volume = {v*Volume(Vec3(x,y,z))}
def sphere_uid4e428756(v:Volume, radius:Double):Interval = {-interval.length(v) + radius}
def scalesrcv_uid4e428756(scale:Double):Volume = {world   * scale}
def max3_uid4e428756(a:Interval, b:Interval, c:Interval):Interval = {interval.max(interval.max(a,b),c)}
def createvec3_uid4e428756(x:Interval, y:Interval, z:Interval):Volume = {Volume(x,y,z)}
def scalesrcz_uid4e428756(scale:Double):Interval = {world.z * scale}
def min2_uid4e428756(a:Interval, b:Interval):Interval = {interval.min(a,b)}
def scalesrcx_uid4e428756(scale:Double):Interval = {world.x * scale}
def negate_uid4e428756(a:Interval):Interval = {-a}
def addconstantexp_uid4e428756(a:Interval, value:Double):Interval = {a+value}
def scalesrcy_uid4e428756(scale:Double):Interval = {world.y * scale}
def perlinnoise3_uid4e428756(v:Volume, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {(noise3_prediction((v + Volume(x,y,z))*size)+offset)*scale/size + add - sub}

val vn1_scalesrcz_uid4e428756 = scalesrcz_uid4e428756(0.10881882041201557)
val vn9_addconstantexp_uid4e428756 = addconstantexp_uid4e428756(vn1_scalesrcz_uid4e428756, 54.19169999120173)
val vn7_addconstantexp_uid4e428756 = addconstantexp_uid4e428756(vn9_addconstantexp_uid4e428756, 0.16957554093095903)
val vn1_scalesrcy_uid4e428756 = scalesrcy_uid4e428756(0.10881882041201557)
val vn1_scalesrcx_uid4e428756 = scalesrcx_uid4e428756(0.10881882041201557)
val vn3_createvec3_uid4e428756 = createvec3_uid4e428756(vn1_scalesrcx_uid4e428756, vn1_scalesrcy_uid4e428756, vn7_addconstantexp_uid4e428756)
val vn6_scalevec3_uid4e428756 = scalevec3_uid4e428756(vn3_createvec3_uid4e428756, 0.7169776240079135, 0.7169776240079135, 22.315898661606493)
val vn12_sphere_uid4e428756 = sphere_uid4e428756(vn3_createvec3_uid4e428756, 54.19169999120173)
val vn14_sphere_uid4e428756 = sphere_uid4e428756(vn6_scalevec3_uid4e428756, 131.59856981197643)
val vn1_scalesrcv_uid4e428756 = scalesrcv_uid4e428756(0.10881882041201557)
val vn17_addconstantexp_uid4e428756 = addconstantexp_uid4e428756(vn12_sphere_uid4e428756, 30.124958317193155)
val vn26_perlinnoise3_uid4e428756 = perlinnoise3_uid4e428756(vn1_scalesrcv_uid4e428756, Interval(0,0), Interval(0,0), Interval(0,0), vn14_sphere_uid4e428756, Interval(0,0), 0.5743491774985172, 205.0738886629432, -0.43999999999999995)
val vn13_negate_uid4e428756 = negate_uid4e428756(vn17_addconstantexp_uid4e428756)
val vn11_min2_uid4e428756 = min2_uid4e428756(vn13_negate_uid4e428756, vn26_perlinnoise3_uid4e428756)
val vn28_perlinnoise3_uid4e428756 = perlinnoise3_uid4e428756(vn1_scalesrcv_uid4e428756, Interval(0,0), Interval(0,0), Interval(0,0), vn12_sphere_uid4e428756, Interval(0,0), 0.05593906693299827, 0.5743491774985175, 0.0)
val vn2_max3_uid4e428756 = max3_uid4e428756(vn28_perlinnoise3_uid4e428756, vn12_sphere_uid4e428756, vn11_min2_uid4e428756)

vn2_max3_uid4e428756
}

}
