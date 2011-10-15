 
package openworld.gen

import noise.Noise.noise3

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

// case class Material(color:Int = 0x000000)
import openworld.Util.Material

object material {

def apply(world:Vec3) = {

def scalesrcy_uid4e7cbecf(scale:Double):Double = {world.y * scale}
def matstone_uid4e7cbecf():Material = {Material(0x8e8e8e)}
def scalesrcx_uid4e7cbecf(scale:Double):Double = {world.x * scale}
def scalesrcv_uid4e7cbecf(scale:Double):Vec3 = {world   * scale}
def matrgb_uid4e7cbecf(r:Double, g:Double, b:Double):Material = {Material((r*255).toInt << 16 | (g*255).toInt << 8 | (b*255).toInt)}
def addconstantexp_uid4e7cbecf(a:Double, value:Double):Double = {a+value}
def scalesrcz_uid4e7cbecf(scale:Double):Double = {world.z * scale}
def sum3_uid4e7cbecf(a:Double, b:Double, c:Double):Double = {a+b+c}
def matmix_uid4e7cbecf(m1:Material, t:Double, m2:Material, shift:Double):Material = {if(t >= shift) m1 else m2}
def perlinnoise3_uid4e7cbecf(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}

val vn2_scalesrcv_uid4e7cbecf = scalesrcv_uid4e7cbecf(1.0)
val vn2_scalesrcy_uid4e7cbecf = scalesrcy_uid4e7cbecf(1.0)
val vn2_scalesrcx_uid4e7cbecf = scalesrcx_uid4e7cbecf(1.0)
val vn11_perlinnoise3_uid4e7cbecf = perlinnoise3_uid4e7cbecf(vn2_scalesrcv_uid4e7cbecf, 0.0, 0.0, 0.0, 0.0, 0.0, 6.588728138140584, 1.7411011265922491, 0.0)
val vn9_perlinnoise3_uid4e7cbecf = perlinnoise3_uid4e7cbecf(Vec3(0), vn2_scalesrcx_uid4e7cbecf, vn2_scalesrcy_uid4e7cbecf, 0.0, 0.0, 0.0, 0.10881882041201557, 0.18946457081379972, 0.0)
val vn2_scalesrcz_uid4e7cbecf = scalesrcz_uid4e7cbecf(1.0)
val vn12_sum3_uid4e7cbecf = sum3_uid4e7cbecf(vn2_scalesrcz_uid4e7cbecf, vn9_perlinnoise3_uid4e7cbecf, vn11_perlinnoise3_uid4e7cbecf)
val vn8_scalesrcz_uid4e7cbecf = scalesrcz_uid4e7cbecf(1.0)
val vn8_scalesrcv_uid4e7cbecf = scalesrcv_uid4e7cbecf(1.0)
val vn13_matrgb_uid4e7cbecf = matrgb_uid4e7cbecf(0.84, 0.49, 0.0)
val vn5_perlinnoise3_uid4e7cbecf = perlinnoise3_uid4e7cbecf(Vec3(0), 0.0, 0.0, vn12_sum3_uid4e7cbecf, 0.0, 0.0, 5.278031643091579, 0.12158186842653569, -0.12)
val vn15_matrgb_uid4e7cbecf = matrgb_uid4e7cbecf(0.62, 0.28, 0.0)
val vn6_perlinnoise3_uid4e7cbecf = perlinnoise3_uid4e7cbecf(vn8_scalesrcv_uid4e7cbecf, 0.0, 0.0, 0.0, 0.0, vn8_scalesrcz_uid4e7cbecf, 0.05006686734935137, 4.228072162245522, -0.62)
val vn14_matmix_uid4e7cbecf = matmix_uid4e7cbecf(vn15_matrgb_uid4e7cbecf, vn5_perlinnoise3_uid4e7cbecf, vn13_matrgb_uid4e7cbecf, 0.0)
val vn10_addconstantexp_uid4e7cbecf = addconstantexp_uid4e7cbecf(vn6_perlinnoise3_uid4e7cbecf, 5.597467536876814)
val vn3_matstone_uid4e7cbecf = matstone_uid4e7cbecf()
val vn17_matmix_uid4e7cbecf = matmix_uid4e7cbecf(vn3_matstone_uid4e7cbecf, vn10_addconstantexp_uid4e7cbecf, vn14_matmix_uid4e7cbecf, 0.0)

vn17_matmix_uid4e7cbecf
}

}
