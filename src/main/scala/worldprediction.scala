 
package xöpäx.gen

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import noise.Noise.noise3_prediction
import noise.interval
import noise.interval.{Interval, Volume}

object prediction {

def apply(world:Volume) = {

def scalesrcz(scale:Double):Interval = {world.z * scale}
def scalesrcv(scale:Double):Volume = {world   * scale}
def perlinnoise3(v:Volume, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {(noise3_prediction((v + Volume(x,y,z))*size)+offset)*scale/size + add - sub}

val vn2_scalesrcz = scalesrcz(1.0)
val vn2_scalesrcv = scalesrcv(1.0)
val vn1_perlinnoise3 = perlinnoise3(vn2_scalesrcv, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), vn2_scalesrcz, 0.014782150730087436, 0.4117955086337865, -0.18000000000000005)

vn1_perlinnoise3
}

}
