package downearth.worldoctree

object Culling {
  val totallyInside = 1
  val totallyOutside = 0
  val intersected = -1
}

trait Culling {
  def test(sphere:Sphere):Int
  def test(cube:CubeLike):Int
}

object CullNothing extends Culling {
  import Culling._
  override def test(sphere:Sphere) = totallyInside
  override def test(cube:CubeLike) = totallyInside
}

class FrustumCulling(cullFrustum:Frustum) extends Culling {
  override def test(sphere:Sphere) = frustumTest(cullFrustum, sphere)
  override def test(cube:CubeLike) = frustumTest(cullFrustum, cube)

  def frustumTest(cullFrustum:Frustum, sphere:Sphere) = ???
  def frustumTest(cullFrustum:Frustum, cube:CubeLike):Int = {
    //TODO: totally inside
    // http://web.archive.org/web/20030601123911/http://www.markmorley.com/opengl/frustumculling.html
    import cullFrustum.planes
    val radius = cube.radius
    val x = cube.posX + radius
    val y = cube.posY + radius
    val z = cube.posZ + radius
    var p = 0
    while( p < 6 )
    {
      if(  planes(p).x * (x - radius) + planes(p).y * (y - radius) + planes(p).z * (z - radius) + planes(p).w <= 0
        && planes(p).x * (x + radius) + planes(p).y * (y - radius) + planes(p).z * (z - radius) + planes(p).w <= 0
        && planes(p).x * (x - radius) + planes(p).y * (y + radius) + planes(p).z * (z - radius) + planes(p).w <= 0
        && planes(p).x * (x + radius) + planes(p).y * (y + radius) + planes(p).z * (z - radius) + planes(p).w <= 0
        && planes(p).x * (x - radius) + planes(p).y * (y - radius) + planes(p).z * (z + radius) + planes(p).w <= 0
        && planes(p).x * (x + radius) + planes(p).y * (y - radius) + planes(p).z * (z + radius) + planes(p).w <= 0
        && planes(p).x * (x - radius) + planes(p).y * (y + radius) + planes(p).z * (z + radius) + planes(p).w <= 0
        && planes(p).x * (x + radius) + planes(p).y * (y + radius) + planes(p).z * (z + radius) + planes(p).w <= 0 )
        return Culling.totallyOutside
      p += 1
    }
    return Culling.intersected
  }
}

class ConeCulling(cullCone:Cone) extends Culling {
  override def test(sphere:Sphere) = coneTest(cullCone, sphere)
  override def test(cube:CubeLike) = ???

  def coneTest(cullCone:Cone, sphere:Sphere) = ???
}

class SphereCulling(cullSphere:Sphere) extends Culling {
  override def test(sphere:Sphere) = sphereTest(cullSphere, sphere)
  override def test(cube:CubeLike) = sphereTest(cullSphere, cube)

  def sphereTest(cullSphere:Sphere, sphere:Sphere) = ???
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

