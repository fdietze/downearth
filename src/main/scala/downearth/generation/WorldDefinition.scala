
package downearth.generation

import interval.{Interval, Interval3, Interval4}
import simplex3d.math.double._
import simplex3d.math.double.functions._
import downearth.resources.Material

object WorldDefinition extends WorldFunction {

   
def density(world:Vec3) = {

def diff2(a:Double, b:Double):Double = {a-b}
def scalesrcz_uid50a7aec4(scale:Double):Double = {world.z * scale}
def scalesrcy_uid50a7aec4(scale:Double):Double = {world.y * scale}
def multiplyconstantexp(a:Double, value:Double):Double = {a*value}

val vn1_scalesrcy_uid50a7aec4 = scalesrcy_uid50a7aec4(1.0)
val vn1_scalesrcz_uid50a7aec4 = scalesrcz_uid50a7aec4(1.0)
val vn10_multiplyconstantexp = multiplyconstantexp(vn1_scalesrcy_uid50a7aec4, 0.5140569133280333)
val vn9_diff2 = diff2(vn10_multiplyconstantexp, vn1_scalesrcz_uid50a7aec4)

vn9_diff2
}


   
def material(world:Vec3) = {

def material__0(r:Double, g:Double, b:Double):Material = {Material(0,r,g,b)}

val vn2_material__0 = material__0(0.21, 0.83, 0.08)

vn2_material__0
}


   
def bounds(world:Interval3) = {

def scalesrcz_uid50a7aec4(scale:Double):Interval = {world.z * scale}
def diff2(a:Interval, b:Interval):Interval = {a-b}
def scalesrcy_uid50a7aec4(scale:Double):Interval = {world.y * scale}
def multiplyconstantexp(a:Interval, value:Double):Interval = {a*value}

val vn1_scalesrcy_uid50a7aec4 = scalesrcy_uid50a7aec4(1.0)
val vn1_scalesrcz_uid50a7aec4 = scalesrcz_uid50a7aec4(1.0)
val vn10_multiplyconstantexp = multiplyconstantexp(vn1_scalesrcy_uid50a7aec4, 0.5140569133280333)
val vn9_diff2 = diff2(vn10_multiplyconstantexp, vn1_scalesrcz_uid50a7aec4)

vn9_diff2
}


   
def intervalExtension(world:Interval3) = {

def scalesrcz_uid50a7aec4(scale:Double):Interval = {world.z * scale}
def diff2(a:Interval, b:Interval):Interval = {a-b}
def scalesrcy_uid50a7aec4(scale:Double):Interval = {world.y * scale}
def multiplyconstantexp(a:Interval, value:Double):Interval = {a*value}

val vn1_scalesrcy_uid50a7aec4 = scalesrcy_uid50a7aec4(1.0)
val vn1_scalesrcz_uid50a7aec4 = scalesrcz_uid50a7aec4(1.0)
val vn10_multiplyconstantexp = multiplyconstantexp(vn1_scalesrcy_uid50a7aec4, 0.5140569133280333)
val vn9_diff2 = diff2(vn10_multiplyconstantexp, vn1_scalesrcz_uid50a7aec4)

vn9_diff2
}


}
            