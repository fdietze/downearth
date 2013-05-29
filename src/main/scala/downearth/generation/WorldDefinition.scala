
package downearth.generation

import interval.{Interval, Interval3, Interval4}
import simplex3d.math.double._
import simplex3d.math.double.functions._

object WorldDefinition extends WorldFunction {

   
def density(world:Vec3) = {

def perlinnoise3sum_uid51a5e348(v:Vec3, size:Double, scale:Double, offset:Double, steps:Double, factor:Double):Double = {
val pos = v*size
var sum = 0.0
for(i <- 0 until steps.toInt) {
	val f = pow(factor,i)
	sum += perlinNoise3(pos*f)/f
}
(sum+offset)*scale/size}
def scalesrcv_uid51a5e348(scale:Double):Vec3 = {world   * scale}
def diff2_uid51a5e348(a:Double, b:Double):Double = {a-b}
def scalesrcz_uid51a5e348(scale:Double):Double = {world.z * scale}

val vn7_scalesrcv_uid51a5e348 = scalesrcv_uid51a5e348(0.0094858975343363)
val vn7_scalesrcz_uid51a5e348 = scalesrcz_uid51a5e348(0.0094858975343363)
val vn6_perlinnoise3sum_uid51a5e348 = perlinnoise3sum_uid51a5e348(vn7_scalesrcv_uid51a5e348, 1.0, 1.0, 0.1399999999999999, 4.0, 2.0)
val vn2_diff2_uid51a5e348 = diff2_uid51a5e348(vn6_perlinnoise3sum_uid51a5e348, vn7_scalesrcz_uid51a5e348)

vn2_diff2_uid51a5e348
}


   
def material(world:Vec3) = {

def scalesrcv_uid51a5e348(scale:Double):Vec3 = {world   * scale}
def perlinnoise3sum_uid51a5e348(v:Vec3, size:Double, scale:Double, offset:Double, steps:Double, factor:Double):Double = {
val pos = v*size
var sum = 0.0
for(i <- 0 until steps.toInt) {
	val f = pow(factor,i)
	sum += perlinNoise3(pos*f)/f
}
(sum+offset)*scale/size}
def material__1_uid51a5e348(r:Double, g:Double, b:Double):Material = {Material(1,r,g,b)}
def material__0_uid51a5e348(r:Double, g:Double, b:Double):Material = {Material(0,r,g,b)}
def matmix_uid51a5e348(m1:Material, t:Double, m2:Material, shift:Double):Material = {if(t >= shift) m1 else m2}

val vn7_scalesrcv_uid51a5e348 = scalesrcv_uid51a5e348(0.0094858975343363)
val vn5_material__1_uid51a5e348 = material__1_uid51a5e348(0.2, 0.38, 0.87)
val vn6_perlinnoise3sum_uid51a5e348 = perlinnoise3sum_uid51a5e348(vn7_scalesrcv_uid51a5e348, 1.0, 1.0, 0.1399999999999999, 4.0, 2.0)
val vn4_material__0_uid51a5e348 = material__0_uid51a5e348(0.04, 0.17, 0.4)
val vn3_matmix_uid51a5e348 = matmix_uid51a5e348(vn4_material__0_uid51a5e348, vn6_perlinnoise3sum_uid51a5e348, vn5_material__1_uid51a5e348, 0.24852523575870045)

vn3_matmix_uid51a5e348
}


   
def bounds(world:Interval3) = {

def perlinnoise3sum_uid51a5e348(v:Interval3, size:Double, scale:Double, offset:Double, steps:Double, factor:Double):Interval = {
val pos = v*size
var sum = Interval(0.0)
for(i <- 0 until steps.toInt) {
	val f = pow(factor,i)
	sum += perlinNoise3Prediction(pos*f)/f
}
(sum+offset)*scale/size}
def scalesrcz_uid51a5e348(scale:Double):Interval = {world.z * scale}
def diff2_uid51a5e348(a:Interval, b:Interval):Interval = {a-b}
def scalesrcv_uid51a5e348(scale:Double):Interval3 = {world   * scale}

val vn7_scalesrcv_uid51a5e348 = scalesrcv_uid51a5e348(0.0094858975343363)
val vn7_scalesrcz_uid51a5e348 = scalesrcz_uid51a5e348(0.0094858975343363)
val vn6_perlinnoise3sum_uid51a5e348 = perlinnoise3sum_uid51a5e348(vn7_scalesrcv_uid51a5e348, 1.0, 1.0, 0.1399999999999999, 4.0, 2.0)
val vn2_diff2_uid51a5e348 = diff2_uid51a5e348(vn6_perlinnoise3sum_uid51a5e348, vn7_scalesrcz_uid51a5e348)

vn2_diff2_uid51a5e348
}


   
def intervalExtension(world:Interval3) = {

def perlinnoise3sum_uid51a5e348(v:Interval3, size:Double, scale:Double, offset:Double, steps:Double, factor:Double):Interval = {
val pos = v*size
var sum = Interval(0.0)
for(i <- 0 until steps.toInt) {
	val f = pow(factor,i)
	sum += perlinNoise3Prediction(pos*f)/f
}
(sum+offset)*scale/size}
def scalesrcz_uid51a5e348(scale:Double):Interval = {world.z * scale}
def diff2_uid51a5e348(a:Interval, b:Interval):Interval = {a-b}
def scalesrcv_uid51a5e348(scale:Double):Interval3 = {world   * scale}

val vn7_scalesrcv_uid51a5e348 = scalesrcv_uid51a5e348(0.0094858975343363)
val vn7_scalesrcz_uid51a5e348 = scalesrcz_uid51a5e348(0.0094858975343363)
val vn6_perlinnoise3sum_uid51a5e348 = perlinnoise3sum_uid51a5e348(vn7_scalesrcv_uid51a5e348, 1.0, 1.0, 0.1399999999999999, 4.0, 2.0)
val vn2_diff2_uid51a5e348 = diff2_uid51a5e348(vn6_perlinnoise3sum_uid51a5e348, vn7_scalesrcz_uid51a5e348)

vn2_diff2_uid51a5e348
}


}
            