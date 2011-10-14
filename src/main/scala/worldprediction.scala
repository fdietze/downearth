 
package openworld.gen

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import noise.Noise.noise3_prediction
import noise.interval
import noise.interval.{Interval, Volume}

object prediction {

def apply(world:Volume) = {

def addconstantexp_uid4e56524d(a:Interval, value:Double):Interval = {a+value}
def scalevec3_uid4e56524d(v:Volume, x:Double, y:Double, z:Double):Volume = {v*Volume(Vec3(x,y,z))}
def scalesrcz_uid4e56524d(scale:Double):Interval = {world.z * scale}
def negate_uid4e56524d(a:Interval):Interval = {-a}
def scalesrcy_uid4e56524d(scale:Double):Interval = {world.y * scale}
def max3_uid4e56524d(a:Interval, b:Interval, c:Interval):Interval = {interval.max(interval.max(a,b),c)}
def scalesrcv_uid4e56524d(scale:Double):Volume = {world   * scale}
def createvec3_uid4e56524d(x:Interval, y:Interval, z:Interval):Volume = {Volume(x,y,z)}
def perlinnoise3_uid4e56524d(v:Volume, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {(noise3_prediction((v + Volume(x,y,z))*size)+offset)*scale/size + add - sub}
def sum2_uid4e56524d(a:Interval, b:Interval):Interval = {a+b}
def scalesrcx_uid4e56524d(scale:Double):Interval = {world.x * scale}
def min2_uid4e56524d(a:Interval, b:Interval):Interval = {interval.min(a,b)}
def sphere_uid4e56524d(v:Volume, radius:Double):Interval = {-interval.length(v) + radius}

val vn2_scalesrcz_uid4e56524d = scalesrcz_uid4e56524d(0.040107059298840744)
val vn5_addconstantexp_uid4e56524d = addconstantexp_uid4e56524d(vn2_scalesrcz_uid4e56524d, 31.124958317193155)
val vn3_addconstantexp_uid4e56524d = addconstantexp_uid4e56524d(vn5_addconstantexp_uid4e56524d, 0.16957554093095903)
val vn2_scalesrcy_uid4e56524d = scalesrcy_uid4e56524d(0.040107059298840744)
val vn2_scalesrcx_uid4e56524d = scalesrcx_uid4e56524d(0.040107059298840744)
val vn12_createvec3_uid4e56524d = createvec3_uid4e56524d(vn2_scalesrcx_uid4e56524d, vn2_scalesrcy_uid4e56524d, vn3_addconstantexp_uid4e56524d)
val vn14_scalevec3_uid4e56524d = scalevec3_uid4e56524d(vn12_createvec3_uid4e56524d, 0.7169776240079135, 0.7169776240079135, 22.315898661606493)
val vn26_sphere_uid4e56524d = sphere_uid4e56524d(vn12_createvec3_uid4e56524d, 31.124958317193155)
val vn28_sphere_uid4e56524d = sphere_uid4e56524d(vn14_scalevec3_uid4e56524d, 256.0)
val vn2_scalesrcv_uid4e56524d = scalesrcv_uid4e56524d(0.040107059298840744)
val vn6_addconstantexp_uid4e56524d = addconstantexp_uid4e56524d(vn26_sphere_uid4e56524d, 9.26740718050323)
val vn15_perlinnoise3_uid4e56524d = perlinnoise3_uid4e56524d(vn2_scalesrcv_uid4e56524d, Interval(0,0), Interval(0,0), Interval(0,0), vn28_sphere_uid4e56524d, Interval(0,0), 0.23651441168139897, 256.0, -0.72)
val vn16_negate_uid4e56524d = negate_uid4e56524d(vn6_addconstantexp_uid4e56524d)
val vn8_perlinnoise3_uid4e56524d = perlinnoise3_uid4e56524d(vn2_scalesrcv_uid4e56524d, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), 1.0, 0.32987697769322366, 0.0)
val vn9_perlinnoise3_uid4e56524d = perlinnoise3_uid4e56524d(vn2_scalesrcv_uid4e56524d, Interval(0,0), Interval(0,0), Interval(0,0), vn26_sphere_uid4e56524d, Interval(0,0), 0.08717147914690036, 0.21168632809063176, 0.0)
val vn1_min2_uid4e56524d = min2_uid4e56524d(vn16_negate_uid4e56524d, vn15_perlinnoise3_uid4e56524d)
val vn10_sum2_uid4e56524d = sum2_uid4e56524d(vn9_perlinnoise3_uid4e56524d, vn8_perlinnoise3_uid4e56524d)
val vn33_max3_uid4e56524d = max3_uid4e56524d(vn10_sum2_uid4e56524d, vn26_sphere_uid4e56524d, vn1_min2_uid4e56524d)

vn33_max3_uid4e56524d
}

}
