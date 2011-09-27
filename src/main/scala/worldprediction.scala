 
package openworld.gen

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import noise.Noise.noise3_prediction
import noise.interval
import noise.interval.{Interval, Volume}

object prediction {

def apply(world:Volume) = {

def perlinnoise3_uid4e7ca7b6(v:Volume, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {(noise3_prediction((v + Volume(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcv_uid4e7ca7b6(scale:Double):Volume = {world   * scale}
def scalesrcz_uid4e7ca7b6(scale:Double):Interval = {world.z * scale}
def max2_uid4e7ca7b6(a:Interval, b:Interval):Interval = {interval.max(a,b)}

val vn4_scalesrcz_uid4e7ca7b6 = scalesrcz_uid4e7ca7b6(1.0)
val vn4_scalesrcv_uid4e7ca7b6 = scalesrcv_uid4e7ca7b6(1.0)
val vn2_scalesrcz_uid4e7ca7b6 = scalesrcz_uid4e7ca7b6(1.0)
val vn2_scalesrcv_uid4e7ca7b6 = scalesrcv_uid4e7ca7b6(1.0)
val vn12_perlinnoise3_uid4e7ca7b6 = perlinnoise3_uid4e7ca7b6(vn4_scalesrcv_uid4e7ca7b6, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), vn4_scalesrcz_uid4e7ca7b6, 0.05006686734935137, 4.228072162245522, -0.62)
val vn10_perlinnoise3_uid4e7ca7b6 = perlinnoise3_uid4e7ca7b6(vn2_scalesrcv_uid4e7ca7b6, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), vn2_scalesrcz_uid4e7ca7b6, 0.01651590688377157, 0.8950250709279723, -0.45999999999999996)
val vn11_max2_uid4e7ca7b6 = max2_uid4e7ca7b6(vn10_perlinnoise3_uid4e7ca7b6, vn12_perlinnoise3_uid4e7ca7b6)

vn11_max2_uid4e7ca7b6
}

}
