package downearth.generation

import interval.{Interval, Interval3}
import simplex3d.math.double._

object WorldDefinition extends WorldFunction {

  def density(pos:Vec3) = {
    def scalesrcv_uid4e7b7b1d(scale:Double):Vec3 = {pos   * scale}
    def scalesrcz_uid4e7b7b1d(scale:Double):Double = {pos.z * scale}
    def perlinnoise3_uid4e7b7b1d(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(perlinNoise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}

    val vn2_scalesrcz_uid4e7b7b1d = scalesrcz_uid4e7b7b1d(0.12158186842653576)
    val vn2_scalesrcv_uid4e7b7b1d = scalesrcv_uid4e7b7b1d(0.12158186842653576)
    val vn1_perlinnoise3_uid4e7b7b1d = perlinnoise3_uid4e7b7b1d(vn2_scalesrcv_uid4e7b7b1d, 0.0, 0.0, 0.0, 0.0, vn2_scalesrcz_uid4e7b7b1d, 1.0, 0.4600938253124375, 0.0)

    vn1_perlinnoise3_uid4e7b7b1d
  }

  def material(pos:Vec3) = {
    def perlinnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(perlinNoise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
    def scalesrcv_uid4e7b7b1d(scale:Double):Vec3 = {pos   * scale}
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

  def bounds(area:Interval3) = {
    def scalesrcz_uid4e7b7b1d(scale:Double):Interval = {area.z * scale}
    def perlinnoise3_uid4e7b7b1d(v:Interval3, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {
      (perlinNoise3Bounds((v + Interval3(x,y,z))*size)+offset)*scale/size + add - sub}
    def scalesrcv_uid4e7b7b1d(scale:Double):Interval3 = {area   * scale}

    val vn2_scalesrcz_uid4e7b7b1d = scalesrcz_uid4e7b7b1d(0.12158186842653576)
    val vn2_scalesrcv_uid4e7b7b1d = scalesrcv_uid4e7b7b1d(0.12158186842653576)
    val vn1_perlinnoise3_uid4e7b7b1d = perlinnoise3_uid4e7b7b1d(vn2_scalesrcv_uid4e7b7b1d, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), vn2_scalesrcz_uid4e7b7b1d, 1.0, 0.4600938253124375, 0.0)

    vn1_perlinnoise3_uid4e7b7b1d
  }

  def intervalExtension(area:Interval3) = {
    def scalesrcz_uid4e7b7b1d(scale:Double):Interval = {area.z * scale}
    def perlinnoise3_uid4e7b7b1d(v:Interval3, x:Interval, y:Interval, z:Interval, add:Interval, sub:Interval, size:Double, scale:Double, offset:Double):Interval = {(perlinNoise3Prediction((v + Interval3(x,y,z))*size)+offset)*scale/size + add - sub}
    def scalesrcv_uid4e7b7b1d(scale:Double):Interval3 = {area   * scale}

    val vn2_scalesrcz_uid4e7b7b1d = scalesrcz_uid4e7b7b1d(0.12158186842653576)
    val vn2_scalesrcv_uid4e7b7b1d = scalesrcv_uid4e7b7b1d(0.12158186842653576)
    val vn1_perlinnoise3_uid4e7b7b1d = perlinnoise3_uid4e7b7b1d(vn2_scalesrcv_uid4e7b7b1d, Interval(0,0), Interval(0,0), Interval(0,0), Interval(0,0), vn2_scalesrcz_uid4e7b7b1d, 1.0, 0.4600938253124375, 0.0)

    vn1_perlinnoise3_uid4e7b7b1d
  }
}
