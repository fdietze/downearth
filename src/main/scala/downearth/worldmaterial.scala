 
package downearth.gen

import noise.Noise.noise3
import noise.Worley.cellnoise

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

// case class Material(color:Int = 0x000000)
import downearth.util.Material

object material {

def apply(world:Vec3) = {

def perlinnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcv_uid4e7b7b1d(scale:Double):Vec3 = {world   * scale}
def matmix(m1:Material, t:Double, m2:Material, shift:Double):Material = {if(t >= shift) m1 else m2}
def matgravel():Material = {Material(-6452106,1)}
def matstone():Material = {Material(-4605767,0)}

val vn2_scalesrcv_uid4e7b7b1d = scalesrcv_uid4e7b7b1d(0.12158186842653576)
val vn8_matstone = matstone()
val vn10_perlinnoise3 = perlinnoise3(vn2_scalesrcv_uid4e7b7b1d, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0)
val vn5_matgravel = matgravel()
val vn6_matmix = matmix(vn5_matgravel, vn10_perlinnoise3, vn8_matstone, 0.0)

vn6_matmix
}

}
