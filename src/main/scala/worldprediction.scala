 
package xöpäx.gen

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import noise.Noise.noise3_prediction
import noise.interval
import noise.interval.{Interval, Volume}

object prediction {

def apply(world:Volume) = {

def perlinnoise3(v:Volume, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {(noise3_prediction((v + Volume(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcz_uid4e4af9b6(scale:Double):Interval = {world.z * scale}
def sum2(a:Interval, b:Interval):Interval = {a+b}
def perlinnoise3_uid4e4af9b6(v:Volume, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {(noise3_prediction((v + Volume(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcv_uid4e4af9b6(scale:Double):Volume = {world   * scale}

val vn2_scalesrcz_uid4e4af9b6 = scalesrcz_uid4e4af9b6(1.0)
val vn2_scalesrcv_uid4e4af9b6 = scalesrcv_uid4e4af9b6(1.0)
val vn1_perlinnoise3_uid4e4af9b6 = perlinnoise3_uid4e4af9b6(vn2_scalesrcv_uid4e4af9b6, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), vn2_scalesrcz_uid4e4af9b6, 0.08717147914690036, 0.5140569133280333, -0.86)
val vn5_perlinnoise3 = perlinnoise3(vn2_scalesrcv_uid4e4af9b6, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), 0.23651441168139897, 0.15177436054938087, 0.0)
val vn6_sum2 = sum2(vn5_perlinnoise3, vn1_perlinnoise3_uid4e4af9b6)

vn6_sum2
}

}
