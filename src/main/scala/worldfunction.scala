package xöpäx
package object gen {

import noise.Noise.noise3

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

case class Material(color:Int = 0x000000)
def proceduralworld(world:Vec3) = {
def scalesrcy(scale:Double):Double = {world.y * scale}
def addconstantexp(a:Double, value:Double):Double = {a+value}
def result(d:Double, m:Material):(Double, Material) = {(d,m)}
def matgold():Material = {Material(0xfab614);}
def matgravel():Material = {Material(0x4f4f4f);}
def summedinputnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def matthreshold(m1:Material, t:Double, m2:Material):Material = {if(t >= 0) m1 else m2;}
def scalesrcx(scale:Double):Double = {world.x * scale}
def scalesrcz(scale:Double):Double = {world.z * scale}

val vn7_scalesrcx = scalesrcx(0.16957554093095892)
val vn7_scalesrcz = scalesrcz(0.16957554093095892)
val vn7_scalesrcy = scalesrcy(0.16957554093095892)
val vn8_addconstantexp = addconstantexp(vn7_scalesrcx, 9.189586839976275)
val vn6_matgravel = matgravel()
val vn3_summedinputnoise3 = summedinputnoise3(Vec3(0), vn8_addconstantexp, vn7_scalesrcy, vn7_scalesrcz, 0.0, vn7_scalesrcz, 0.16957554093095892, 0.7169776240079135, -0.74)
val vn2_matgold = matgold()
val vn4_matthreshold = matthreshold(vn2_matgold, vn3_summedinputnoise3, vn6_matgravel)
val vn5_addconstantexp = addconstantexp(vn3_summedinputnoise3, 3.0314331330207955)
val vn1_result = result(vn5_addconstantexp, vn4_matthreshold)

vn1_result
}

}