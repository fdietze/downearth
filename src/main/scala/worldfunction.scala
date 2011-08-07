package xöpäx
package object gen {

import noise.Noise.noise3

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

case class Material(color:Int = 0x000000)
def proceduralworld(world:Vec3) = {
def result(d:Double, m:Material):(Double, Material) = {(d,m)}
def scalesrcv(scale:Double):Vec3 = {world   * scale}
def summedinputnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}

val vn3_scalesrcv = scalesrcv(1.0)
val vn1_summedinputnoise3 = summedinputnoise3(vn3_scalesrcv, 0.0, 0.0, 0.0, 0.0, 0.0, 0.10881882041201557, 1.9453098948245722, 0.3799999999999999)
val vn2_summedinputnoise3 = summedinputnoise3(vn3_scalesrcv, vn1_summedinputnoise3, vn1_summedinputnoise3, vn1_summedinputnoise3, 0.0, 0.0, 0.21168632809063176, 11.471641984126617, 0.17999999999999994)
val vn4_result = result(vn2_summedinputnoise3, Material(0x000000))

vn4_result
}

}