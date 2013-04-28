 
package downearth.gen

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import noise.Noise.noise3_prediction
import noise.Worley.cellnoise_prediction

import noise.interval
import noise.interval.{Interval, Volume, Interval4D}

object prediction {

def apply(world:Volume) = {

def scalesrcz_uid4e7b7b1d(scale:Double):Interval = {world.z * scale}
def perlinnoise3_uid4e7b7b1d(v:Volume, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {(noise3_prediction((v + Volume(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcv_uid4e7b7b1d(scale:Double):Volume = {world   * scale}

val vn2_scalesrcz_uid4e7b7b1d = scalesrcz_uid4e7b7b1d(0.12158186842653576)
val vn2_scalesrcv_uid4e7b7b1d = scalesrcv_uid4e7b7b1d(0.12158186842653576)
val vn1_perlinnoise3_uid4e7b7b1d = perlinnoise3_uid4e7b7b1d(vn2_scalesrcv_uid4e7b7b1d, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), vn2_scalesrcz_uid4e7b7b1d, 1.0, 0.4600938253124375, 0.0)

vn1_perlinnoise3_uid4e7b7b1d
}

}
