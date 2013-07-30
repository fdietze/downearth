package downearth.benchmark

import interval.{Interval, Interval3, Interval4}
import simplex3d.math.double._
import simplex3d.math.double.functions._
import downearth.resources.Material
import downearth.generation.WorldFunction

object TestingWorldDefinition extends WorldFunction {

   
def density(world:ReadVec3) = {

def perlinnoise3_uid51d42a78(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {((perlinNoise3(v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcv_uid51d42a78(scale:Double):Vec3 = {world   * scale}

val vn1_scalesrcv_uid51d42a78 = scalesrcv_uid51d42a78(0.05593906693299827)
val vn4_perlinnoise3_uid51d42a78 = perlinnoise3_uid51d42a78(vn1_scalesrcv_uid51d42a78, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0)

vn4_perlinnoise3_uid51d42a78
}


   
def material(world:ReadVec3) = {

def material__0(r:Double, g:Double, b:Double):Material = {Material(0,r,g,b)}

val vn2_material__0 = material__0(0.5, 0.5, 0.5)

vn2_material__0
}


   
def bounds(world:Interval3) = {

def perlinnoise3_uid51d42a78(v:Interval3, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {((perlinNoise3Bounds(v + Interval3(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcv_uid51d42a78(scale:Double):Interval3 = {world   * scale}

val vn1_scalesrcv_uid51d42a78 = scalesrcv_uid51d42a78(0.05593906693299827)
val vn4_perlinnoise3_uid51d42a78 = perlinnoise3_uid51d42a78(vn1_scalesrcv_uid51d42a78, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), 1.0, 1.0, 0.0)

vn4_perlinnoise3_uid51d42a78
}


   
def intervalExtension(world:Interval3) = {

def perlinnoise3_uid51d42a78(v:Interval3, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {((perlinNoise3Prediction(v + Interval3(x,y,z))*size)+offset)*scale/size + add - sub}
def scalesrcv_uid51d42a78(scale:Double):Interval3 = {world   * scale}

val vn1_scalesrcv_uid51d42a78 = scalesrcv_uid51d42a78(0.05593906693299827)
val vn4_perlinnoise3_uid51d42a78 = perlinnoise3_uid51d42a78(vn1_scalesrcv_uid51d42a78, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), 1.0, 1.0, 0.0)

vn4_perlinnoise3_uid51d42a78
}


}
            
