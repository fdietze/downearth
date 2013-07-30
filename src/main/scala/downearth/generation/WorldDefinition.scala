
package downearth.generation

import interval.{Interval, Interval3, Interval4}
import simplex3d.math.double._
import simplex3d.math.double.functions._
import downearth.resources.Material

object WorldDefinition extends WorldFunction {

   
def density(world:ReadVec3) = {

def perlinnoise3_uid51b9c303(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {((perlinNoise3(v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcv_uid51b9c303(scale:Double):Vec3 = {world   * scale}
def product2_uid51b9c303(a:Double, b:Double):Double = {a*b}
def diff2_uid51b9c303(a:Double, b:Double):Double = {a-b}
def scalarplusvec3_uid51b9c303(v:Vec3, s:Double):Vec3 = {v+s}
def addconstantexp_uid51b9c303(a:Double, value:Double):Double = {a+value}
def min2_uid51b9c303(a:Double, b:Double):Double = {min(a,b)}
def perlinnoise3sum_uid51b9c303(v:Vec3, size:Double, scale:Double, offset:Double, steps:Double, factor:Double):Double = {
val pos = v*size
var sum = 0.0
for(i <- 0 until steps.toInt) {
	val f = pow(factor,i)
	sum += perlinNoise3(pos*f)/f
}
(sum+offset)*scale/size}
def expconstant_uid51b9c303(value:Double):Double = {value}
def scalesrcz_uid51b9c303(scale:Double):Double = {world.z * scale}
def negate_uid51b9c303(a:Double):Double = {-a}

val vn22_expconstant_uid51b9c303 = expconstant_uid51b9c303(2.173469725052115)
val vn9_scalesrcv_uid51b9c303 = scalesrcv_uid51b9c303(0.035896823593657326)
val vn24_scalarplusvec3_uid51b9c303 = scalarplusvec3_uid51b9c303(vn9_scalesrcv_uid51b9c303, vn22_expconstant_uid51b9c303)
val vn16_perlinnoise3_uid51b9c303 = perlinnoise3_uid51b9c303(vn24_scalarplusvec3_uid51b9c303, 0.0, 0.0, 0.0, 0.0, 0.0, 1.11728713807222, 3.386981249450108, 0.040000000000000036)
val vn6_perlinnoise3_uid51b9c303 = perlinnoise3_uid51b9c303(vn9_scalesrcv_uid51b9c303, 0.0, 0.0, 0.0, 0.0, 0.0, 1.741101126592248, 5.278031643091579, -0.07999999999999996)
val vn4_product2_uid51b9c303 = product2_uid51b9c303(vn16_perlinnoise3_uid51b9c303, vn16_perlinnoise3_uid51b9c303)
val vn18_product2_uid51b9c303 = product2_uid51b9c303(vn6_perlinnoise3_uid51b9c303, vn6_perlinnoise3_uid51b9c303)
val vn3_addconstantexp_uid51b9c303 = addconstantexp_uid51b9c303(vn4_product2_uid51b9c303, -0.11737425948457414)
val vn21_addconstantexp_uid51b9c303 = addconstantexp_uid51b9c303(vn18_product2_uid51b9c303, -0.11737425948457414)
val vn15_scalesrcv_uid51b9c303 = scalesrcv_uid51b9c303(1.0)
val vn7_negate_uid51b9c303 = negate_uid51b9c303(vn3_addconstantexp_uid51b9c303)
val vn19_negate_uid51b9c303 = negate_uid51b9c303(vn21_addconstantexp_uid51b9c303)
val vn15_scalesrcz_uid51b9c303 = scalesrcz_uid51b9c303(1.0)
val vn14_perlinnoise3sum_uid51b9c303 = perlinnoise3sum_uid51b9c303(vn15_scalesrcv_uid51b9c303, 0.013230395505664485, 0.5140569133280333, -0.21999999999999997, 4.0, 2.0)
val vn23_min2_uid51b9c303 = min2_uid51b9c303(vn19_negate_uid51b9c303, vn7_negate_uid51b9c303)
val vn8_diff2_uid51b9c303 = diff2_uid51b9c303(vn14_perlinnoise3sum_uid51b9c303, vn15_scalesrcz_uid51b9c303)
val vn13_negate_uid51b9c303 = negate_uid51b9c303(vn23_min2_uid51b9c303)
val vn11_min2_uid51b9c303 = min2_uid51b9c303(vn13_negate_uid51b9c303, vn8_diff2_uid51b9c303)

vn11_min2_uid51b9c303
}


   
def material(world:ReadVec3) = {

def negate(a:Double):Double = {-a}
def addconstantexp(a:Double, value:Double):Double = {a+value}
def material__3(r:Double, g:Double, b:Double):Material = {Material(3,r,g,b)}
def material__2(r:Double, g:Double, b:Double):Material = {Material(2,r,g,b)}
def matmix(m1:Material, t:Double, m2:Material, shift:Double):Material = {if(t >= shift) m1 else m2}
def matmix_uid51b9c303(m1:Material, t:Double, m2:Material, shift:Double):Material = {if(t >= shift) m1 else m2}
def material__1(r:Double, g:Double, b:Double):Material = {Material(1,r,g,b)}
def scalesrcz_uid51b9c303(scale:Double):Double = {world.z * scale}

val vn15_scalesrcz_uid51b9c303 = scalesrcz_uid51b9c303(1.0)
val vn38_negate = negate(vn15_scalesrcz_uid51b9c303)
val vn37_negate = negate(vn15_scalesrcz_uid51b9c303)
val vn12_material__2 = material__2(0.08, 0.32, 0.79)
val vn40_addconstantexp = addconstantexp(vn38_negate, -37.95434097454637)
val vn41_material__3 = material__3(0.5, 0.5, 0.5)
val vn10_material__1 = material__1(0.45, 0.25, 0.05)
val vn39_addconstantexp = addconstantexp(vn37_negate, -11.840124558747576)
val vn36_matmix = matmix(vn41_material__3, vn40_addconstantexp, vn12_material__2, 0.0)
val vn25_matmix_uid51b9c303 = matmix_uid51b9c303(vn36_matmix, vn39_addconstantexp, vn10_material__1, 2.389887512461089)

vn25_matmix_uid51b9c303
}


   
def bounds(world:Interval3) = {

def perlinnoise3sum_uid51b9c303(v:Interval3, size:Double, scale:Double, offset:Double, steps:Double, factor:Double):Interval = {
val pos = v*size
var sum = Interval(0.0)
for(i <- 0 until steps.toInt) {
	val f = pow(factor,i)
	sum += perlinNoise3Bounds(pos*f)/f
}
(sum+offset)*scale/size}
def addconstantexp_uid51b9c303(a:Interval, value:Double):Interval = {a+value}
def product2_uid51b9c303(a:Interval, b:Interval):Interval = {a*b}
def min2_uid51b9c303(a:Interval, b:Interval):Interval = {interval.functions.min(a,b)}
def negate_uid51b9c303(a:Interval):Interval = {-a}
def scalesrcv_uid51b9c303(scale:Double):Interval3 = {world   * scale}
def diff2_uid51b9c303(a:Interval, b:Interval):Interval = {a-b}
def perlinnoise3_uid51b9c303(v:Interval3, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {((perlinNoise3Bounds(v + Interval3(x,y,z))*size)+offset)*scale/size + add - sub}
def scalarplusvec3_uid51b9c303(v:Interval3, s:Interval):Interval3 = {v+s}
def scalesrcz_uid51b9c303(scale:Double):Interval = {world.z * scale}
def expconstant_uid51b9c303(value:Double):Interval = {Interval(value)}

val vn22_expconstant_uid51b9c303 = expconstant_uid51b9c303(2.173469725052115)
val vn9_scalesrcv_uid51b9c303 = scalesrcv_uid51b9c303(0.035896823593657326)
val vn24_scalarplusvec3_uid51b9c303 = scalarplusvec3_uid51b9c303(vn9_scalesrcv_uid51b9c303, vn22_expconstant_uid51b9c303)
val vn16_perlinnoise3_uid51b9c303 = perlinnoise3_uid51b9c303(vn24_scalarplusvec3_uid51b9c303, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), 1.11728713807222, 3.386981249450108, 0.040000000000000036)
val vn6_perlinnoise3_uid51b9c303 = perlinnoise3_uid51b9c303(vn9_scalesrcv_uid51b9c303, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), 1.741101126592248, 5.278031643091579, -0.07999999999999996)
val vn4_product2_uid51b9c303 = product2_uid51b9c303(vn16_perlinnoise3_uid51b9c303, vn16_perlinnoise3_uid51b9c303)
val vn18_product2_uid51b9c303 = product2_uid51b9c303(vn6_perlinnoise3_uid51b9c303, vn6_perlinnoise3_uid51b9c303)
val vn3_addconstantexp_uid51b9c303 = addconstantexp_uid51b9c303(vn4_product2_uid51b9c303, -0.11737425948457414)
val vn21_addconstantexp_uid51b9c303 = addconstantexp_uid51b9c303(vn18_product2_uid51b9c303, -0.11737425948457414)
val vn15_scalesrcv_uid51b9c303 = scalesrcv_uid51b9c303(1.0)
val vn7_negate_uid51b9c303 = negate_uid51b9c303(vn3_addconstantexp_uid51b9c303)
val vn19_negate_uid51b9c303 = negate_uid51b9c303(vn21_addconstantexp_uid51b9c303)
val vn15_scalesrcz_uid51b9c303 = scalesrcz_uid51b9c303(1.0)
val vn14_perlinnoise3sum_uid51b9c303 = perlinnoise3sum_uid51b9c303(vn15_scalesrcv_uid51b9c303, 0.013230395505664485, 0.5140569133280333, -0.21999999999999997, 4.0, 2.0)
val vn23_min2_uid51b9c303 = min2_uid51b9c303(vn19_negate_uid51b9c303, vn7_negate_uid51b9c303)
val vn8_diff2_uid51b9c303 = diff2_uid51b9c303(vn14_perlinnoise3sum_uid51b9c303, vn15_scalesrcz_uid51b9c303)
val vn13_negate_uid51b9c303 = negate_uid51b9c303(vn23_min2_uid51b9c303)
val vn11_min2_uid51b9c303 = min2_uid51b9c303(vn13_negate_uid51b9c303, vn8_diff2_uid51b9c303)

vn11_min2_uid51b9c303
}


   
def intervalExtension(world:Interval3) = {

def addconstantexp_uid51b9c303(a:Interval, value:Double):Interval = {a+value}
def product2_uid51b9c303(a:Interval, b:Interval):Interval = {a*b}
def min2_uid51b9c303(a:Interval, b:Interval):Interval = {interval.functions.min(a,b)}
def negate_uid51b9c303(a:Interval):Interval = {-a}
def perlinnoise3sum_uid51b9c303(v:Interval3, size:Double, scale:Double, offset:Double, steps:Double, factor:Double):Interval = {
val pos = v*size
var sum = Interval(0.0)
for(i <- 0 until steps.toInt) {
	val f = pow(factor,i)
	sum += perlinNoise3Prediction(pos*f)/f
}
(sum+offset)*scale/size}
def perlinnoise3_uid51b9c303(v:Interval3, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {((perlinNoise3Prediction(v + Interval3(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcv_uid51b9c303(scale:Double):Interval3 = {world   * scale}
def diff2_uid51b9c303(a:Interval, b:Interval):Interval = {a-b}
def scalarplusvec3_uid51b9c303(v:Interval3, s:Interval):Interval3 = {v+s}
def scalesrcz_uid51b9c303(scale:Double):Interval = {world.z * scale}
def expconstant_uid51b9c303(value:Double):Interval = {Interval(value)}

val vn22_expconstant_uid51b9c303 = expconstant_uid51b9c303(2.173469725052115)
val vn9_scalesrcv_uid51b9c303 = scalesrcv_uid51b9c303(0.035896823593657326)
val vn24_scalarplusvec3_uid51b9c303 = scalarplusvec3_uid51b9c303(vn9_scalesrcv_uid51b9c303, vn22_expconstant_uid51b9c303)
val vn16_perlinnoise3_uid51b9c303 = perlinnoise3_uid51b9c303(vn24_scalarplusvec3_uid51b9c303, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), 1.11728713807222, 3.386981249450108, 0.040000000000000036)
val vn6_perlinnoise3_uid51b9c303 = perlinnoise3_uid51b9c303(vn9_scalesrcv_uid51b9c303, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), 1.741101126592248, 5.278031643091579, -0.07999999999999996)
val vn4_product2_uid51b9c303 = product2_uid51b9c303(vn16_perlinnoise3_uid51b9c303, vn16_perlinnoise3_uid51b9c303)
val vn18_product2_uid51b9c303 = product2_uid51b9c303(vn6_perlinnoise3_uid51b9c303, vn6_perlinnoise3_uid51b9c303)
val vn3_addconstantexp_uid51b9c303 = addconstantexp_uid51b9c303(vn4_product2_uid51b9c303, -0.11737425948457414)
val vn21_addconstantexp_uid51b9c303 = addconstantexp_uid51b9c303(vn18_product2_uid51b9c303, -0.11737425948457414)
val vn15_scalesrcv_uid51b9c303 = scalesrcv_uid51b9c303(1.0)
val vn7_negate_uid51b9c303 = negate_uid51b9c303(vn3_addconstantexp_uid51b9c303)
val vn19_negate_uid51b9c303 = negate_uid51b9c303(vn21_addconstantexp_uid51b9c303)
val vn15_scalesrcz_uid51b9c303 = scalesrcz_uid51b9c303(1.0)
val vn14_perlinnoise3sum_uid51b9c303 = perlinnoise3sum_uid51b9c303(vn15_scalesrcv_uid51b9c303, 0.013230395505664485, 0.5140569133280333, -0.21999999999999997, 4.0, 2.0)
val vn23_min2_uid51b9c303 = min2_uid51b9c303(vn19_negate_uid51b9c303, vn7_negate_uid51b9c303)
val vn8_diff2_uid51b9c303 = diff2_uid51b9c303(vn14_perlinnoise3sum_uid51b9c303, vn15_scalesrcz_uid51b9c303)
val vn13_negate_uid51b9c303 = negate_uid51b9c303(vn23_min2_uid51b9c303)
val vn11_min2_uid51b9c303 = min2_uid51b9c303(vn13_negate_uid51b9c303, vn8_diff2_uid51b9c303)

vn11_min2_uid51b9c303
}


}
