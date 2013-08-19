package downearth.worldoctree

import simplex3d.math.doublex.functions._

object Culling {
  val totallyOutside = -1
  val intersected = 0
  val totallyInside = 1
}

trait Culling { culling =>
  def test(sphere:Sphere):Int
  def test(cube:CubeLike):Int

  def inverted = new Culling {
    def test(sphere:Sphere) = -culling.test(sphere)
    def test(cube:CubeLike) = -culling.test(cube)
  }

  def intersection(that:Culling) = new Culling {
    def test(sphere:Sphere):Int = intersectResults(culling test sphere, that test sphere)
    def test(cube:CubeLike):Int = intersectResults(culling test cube, that test cube)

    def intersectResults(res1:Int, res2:Int):Int = {
      if( res1 == res2 )
        return res1
      else if( res1 == Culling.totallyOutside || res2 == Culling.totallyOutside )
        return Culling.totallyOutside
      else
        return Culling.intersected
    }
  }
}

object CullNothing extends Culling {
  import Culling._
  override def test(sphere:Sphere) = totallyInside
  override def test(cube:CubeLike) = totallyInside
}

class OptimizedFrustumCulling(cullFrustum:Frustum) extends Culling {
  import Culling._
  val frustumCulling = new FrustumCulling(cullFrustum)
  val coneCulling = new ConeCulling(cullFrustum.boundingCone)
  val innerConeCulling = new ConeCulling(cullFrustum.innerCone)
  val sphereCulling = new SphereCulling(cullFrustum.boundingSphere)

  override def test(cube:CubeLike):Int = {
    val sphere = cube.boundingSphere

    if( (sphereCulling test sphere) == totallyOutside )
      return totallyOutside

    if( (coneCulling test sphere) == totallyOutside )
      return totallyOutside

    //if( (innerConeCulling test sphere) == totallyInside )
    //  return totallyInside

    val sphereTest = frustumCulling test sphere

    if( sphereTest == Culling.intersected )
      frustumCulling test cube
    else
      sphereTest
  }
  override def test(sphere:Sphere):Int = ???
}

class FrustumCulling(cullFrustum:Frustum) extends Culling {
  override def test(sphere:Sphere) = frustumTest(cullFrustum, sphere)
  override def test(cube:CubeLike) = frustumTest(cullFrustum, cube)

  def frustumTest(cullFrustum:Frustum, sphere:Sphere):Int = {
    import cullFrustum.{planes => pl}
    import sphere.{centerX,centerY,centerZ}
    import sphere.radius

    var distance = 0.0
    var result = Culling.totallyInside
    var p = 0
    while( p < 6) {
      distance = pl(p).x * centerX + pl(p).y * centerY + pl(p).z * centerZ + pl(p).w
      if( distance < -radius ) return Culling.totallyOutside
      else if( distance.abs < radius ) result = Culling.intersected
      p += 1
    }

    return result
  }

  def frustumTest(cullFrustum:Frustum, cube:CubeLike):Int = {
    // http://www.flipcode.com/archives/Frustum_Culling.shtml
    import cullFrustum.{planes => pl}
    val r = cube.radius
    val x = cube.posX + r
    val y = cube.posY + r
    val z = cube.posZ + r

    var CubesInFrontOfPlane = 0
    var verticesInFrontOfPlane:Int = 8
    var cubeInFrontOfPlane:Int = 1

    var p = 0
    while( p < 6 ) {
      verticesInFrontOfPlane = 8
      cubeInFrontOfPlane = 1

      if( pl(p).x * (x - r) + pl(p).y * (y - r) + pl(p).z * (z - r) + pl(p).w <= 0 ) {cubeInFrontOfPlane = 0; verticesInFrontOfPlane -= 1}
      if( pl(p).x * (x + r) + pl(p).y * (y - r) + pl(p).z * (z - r) + pl(p).w <= 0 ) {cubeInFrontOfPlane = 0; verticesInFrontOfPlane -= 1}
      if( pl(p).x * (x - r) + pl(p).y * (y + r) + pl(p).z * (z - r) + pl(p).w <= 0 ) {cubeInFrontOfPlane = 0; verticesInFrontOfPlane -= 1}
      if( pl(p).x * (x + r) + pl(p).y * (y + r) + pl(p).z * (z - r) + pl(p).w <= 0 ) {cubeInFrontOfPlane = 0; verticesInFrontOfPlane -= 1}
      if( pl(p).x * (x - r) + pl(p).y * (y - r) + pl(p).z * (z + r) + pl(p).w <= 0 ) {cubeInFrontOfPlane = 0; verticesInFrontOfPlane -= 1}
      if( pl(p).x * (x + r) + pl(p).y * (y - r) + pl(p).z * (z + r) + pl(p).w <= 0 ) {cubeInFrontOfPlane = 0; verticesInFrontOfPlane -= 1}
      if( pl(p).x * (x - r) + pl(p).y * (y + r) + pl(p).z * (z + r) + pl(p).w <= 0 ) {cubeInFrontOfPlane = 0; verticesInFrontOfPlane -= 1}
      if( pl(p).x * (x + r) + pl(p).y * (y + r) + pl(p).z * (z + r) + pl(p).w <= 0 ) {cubeInFrontOfPlane = 0; verticesInFrontOfPlane -= 1}

      if(verticesInFrontOfPlane == 0) return Culling.totallyOutside

      CubesInFrontOfPlane += cubeInFrontOfPlane

      p += 1
    }

    if(CubesInFrontOfPlane == 6) return Culling.totallyInside

    return Culling.intersected
  }
}

class ConeCulling(cullCone:Cone) extends Culling {
  override def test(sphere:Sphere) = coneTest(cullCone, sphere)
  override def test(cube:CubeLike) = ???

  def coneTest(cullCone:Cone, sphere:Sphere) = {
    // http://www.cbloom.com/3d/techdocs/culling.txt

    //val V = sphere.center - cullCone.apex_location
    val Vx = sphere.centerX - cullCone.apex_location.x
    val Vy = sphere.centerY - cullCone.apex_location.y
    val Vz = sphere.centerZ - cullCone.apex_location.z
    //val a = dot(V,cullCone.direction_normal)
    val a = Vx*cullCone.direction_normal.x + Vy*cullCone.direction_normal.y + Vz*cullCone.direction_normal.z
    val b = a * cullCone.angleTan
    val c = sqrt( (Vx*Vx + Vy*Vy + Vz*Vz) - a*a )
    val d = c - b
    val e = d * cullCone.angleCos

    if ( e >= sphere.radius ) Culling.totallyOutside
    else if ( e <= -sphere.radius ) Culling.totallyInside
    else Culling.intersected

    /* TODO
    // without sqrt: (seems to also include spheres in the other direction too?!?)
    // http://blog.julien.cayzac.name/2009/12/frustum-culling-sphere-cone-test-with.html
    val V = sphere.center - cullCone.apex_location
    val a = dot(V, cullCone.direction_normal)
    val p = a*cullCone.angleSin
    val q = cullCone.angleCos*cullCone.angleCos * dot(V, V) - a*a
    val r = q - sphere.radius*sphere.radius
    if (p<sphere.radius || q>0) {
      if (r < 2 * sphere.radius * p) Culling.intersected
      else if (q<0) Culling.totallyInside
      else Culling.totallyOutside
    }
    else{
      if ( -r < 2 * sphere.radius * p) Culling.intersected
      else if (q<0) Culling.totallyInside
      else Culling.totallyOutside
    }*/
  }

}

class SphereCulling(cullSphere:Sphere) extends Culling {
  override def test(sphere:Sphere) = sphereTest(cullSphere, sphere)
  override def test(cube:CubeLike) = sphereTest(cullSphere, cube)

  def sphereTest(cullSphere:Sphere, sphere:Sphere) = {
    @inline def squared(x:Double): Double = x * x
    import cullSphere.{centerX => cx, centerY => cy, centerZ => cz}
    import sphere.{centerX => sx, centerY => sy, centerZ => sz}

    val distanceSquared = {
      val dx = cx - sx
      val dy = cy - sy
      val dz = cz - sz
      dx*dx + dy*dy + dz*dz
    }

    if ( cullSphere.radius >= sphere.radius && distanceSquared <= squared( cullSphere.radius - sphere.radius ) ) Culling.totallyInside
    else if ( distanceSquared <= squared( cullSphere.radius + sphere.radius ) ) Culling.intersected
    else Culling.totallyOutside
  }

  def sphereTest(cullSphere:Sphere, cube:CubeLike) = {
    //TODO: totally inside
    if(cullSphere overlaps cube) Culling.intersected
    else Culling.totallyOutside
  }
}

class CubeCulling(cullCube:CubeLike) extends Culling {
  override def test(sphere:Sphere) = ???
  override def test(cube:CubeLike) = cubeTest(cullCube, cube)

  def cubeTest(cullCube:CubeLike, cube:CubeLike) = {
    //TODO: totally inside
    if(cullCube overlaps cube) Culling.intersected
    else Culling.totallyOutside
  }
}

