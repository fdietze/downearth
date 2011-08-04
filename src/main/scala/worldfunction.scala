package xöpäx
package object gen {

import noise.Noise.noise3

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

case class Material(color:Int = 0x000000)
def proceduralworld(world:Vec3) = {
def constantexp(value:Double):Double = {value}
def max2(a:Double, b:Double):Double = {max(a,b)}
def scalesrcy(scale:Double):Double = {world.y * scale}
def diff2(a:Double, b:Double):Double = {a-b}
def result(d:Double, m:Material):(Double, Material) = {(d,m)}
def product2(a:Double, b:Double):Double = {a*b}
def summedinputnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def matstone():Material = {Material(0x8e8e8e);}
def sum2(a:Double, b:Double):Double = {a+b}
def matthreshold(m1:Material, t:Double, m2:Material):Material = {if(t >= 0) m1 else m2;}
def scalesrcx(scale:Double):Double = {world.x * scale}
def scalesrcz(scale:Double):Double = {world.z * scale}
def matrgb(r:Double, g:Double, b:Double):Material = {Material((r*255).toInt << 16 | (g*255).toInt << 8 | (b*255).toInt);}
def sum3(a:Double, b:Double, c:Double):Double = {a+b+c}

val vn12_scalesrcy = scalesrcy(1.248330548901612)
val vn12_scalesrcx = scalesrcx(1.248330548901612)
val vn8_summedinputnoise3 = summedinputnoise3(Vec3(0), vn12_scalesrcx, vn12_scalesrcy, 0.0, 0.0, 0.0, 0.00390625, 0.7169776240079135, 0.0)
val vn12_scalesrcz = scalesrcz(1.248330548901612)
val vn30_diff2 = diff2(0.0, vn8_summedinputnoise3)
val vn28_constantexp = constantexp(54.19169999120173)
val vn25_constantexp = constantexp(0.07802065930635076)
val vn11_constantexp = constantexp(0.8950250709279723)
val vn13_diff2 = diff2(vn8_summedinputnoise3, vn12_scalesrcz)
val vn29_sum3 = sum3(vn12_scalesrcz, vn28_constantexp, vn30_diff2)
val vn24_product2 = product2(vn12_scalesrcz, vn25_constantexp)
val vn9_matrgb = matrgb(0.44, 0.63, 0.14)
val vn14_diff2 = diff2(vn13_diff2, vn11_constantexp)
val vn15_matrgb = matrgb(0.44, 0.27, 0.12)
val vn34_constantexp = constantexp(1.9453098948245722)
val vn21_summedinputnoise3 = summedinputnoise3(Vec3(0), vn12_scalesrcx, vn12_scalesrcy, vn24_product2, 0.0, vn29_sum3, 0.08717147914690036, 12.81711804143395, -0.12)
val vn3_matthreshold = matthreshold(vn15_matrgb, vn14_diff2, vn9_matrgb)
val vn33_sum2 = sum2(vn21_summedinputnoise3, vn34_constantexp)
val vn31_matstone = matstone()
val vn32_matthreshold = matthreshold(vn31_matstone, vn33_sum2, vn3_matthreshold)
val vn26_max2 = max2(vn13_diff2, vn21_summedinputnoise3)
val vn16_result = result(vn26_max2, vn32_matthreshold)

vn16_result
}

}