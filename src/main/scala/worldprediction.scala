 
package openworld.gen

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import noise.Noise.noise3_prediction
import noise.interval
import noise.interval.{Interval, Volume}

object prediction {

def apply(world:Volume) = {

def perlinnoise3_uid4e7cbecf(v:Volume, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {(noise3_prediction((v + Volume(x,y,z))*size)+offset)*scale/size + add - sub}
def max2_uid4e7cbecf(a:Interval, b:Interval):Interval = {interval.max(a,b)}
def scalesrcz_uid4e7cbecf(scale:Double):Interval = {world.z * scale}
def scalesrcv_uid4e7cbecf(scale:Double):Volume = {world   * scale}

val vn8_scalesrcz_uid4e7cbecf = scalesrcz_uid4e7cbecf(1.0)
val vn8_scalesrcv_uid4e7cbecf = scalesrcv_uid4e7cbecf(1.0)
val vn2_scalesrcz_uid4e7cbecf = scalesrcz_uid4e7cbecf(1.0)
val vn2_scalesrcv_uid4e7cbecf = scalesrcv_uid4e7cbecf(1.0)
val vn6_perlinnoise3_uid4e7cbecf = perlinnoise3_uid4e7cbecf(vn8_scalesrcv_uid4e7cbecf, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), vn8_scalesrcz_uid4e7cbecf, 0.05006686734935137, 4.228072162245522, -0.62)
val vn1_perlinnoise3_uid4e7cbecf = perlinnoise3_uid4e7cbecf(vn2_scalesrcv_uid4e7cbecf, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), vn2_scalesrcz_uid4e7cbecf, 0.01651590688377157, 0.8950250709279723, -0.45999999999999996)
val vn16_max2_uid4e7cbecf = max2_uid4e7cbecf(vn1_perlinnoise3_uid4e7cbecf, vn6_perlinnoise3_uid4e7cbecf)

vn16_max2_uid4e7cbecf
}

}
