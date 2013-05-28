package downearth.rendering

import simplex3d.math.double._


import java.io.{InputStreamReader, Reader, BufferedReader, InputStream}
import scala.collection.mutable.{ArrayBuffer, HashMap, ArrayBuilder}

import org.lwjgl.BufferUtils

/**
 * User: arne
 * Date: 06.05.13
 * Time: 02:40
 */
class ObjLoader {
  case class ComposedVertex(v:Int,t:Int,n:Int)
  case class ComposedTriangle(v1:ComposedVertex,v2:ComposedVertex,v3:ComposedVertex)
  case class Vertex(pos:Vec3, texCoord:Vec2, normal:Vec3)
  case class Triangle(v1:Int, v2:Int, v3:Int)

  def composedVertex(str:String) = {
    val a = str.split('/')
    if(a.size == 1)
      ComposedVertex(a(0).toInt, 1, 1)
    else if(a.size == 2)
      ComposedVertex(a(0).toInt, a(1).toInt, 1)
    else if(a.size == 3 && a(1) != "")
      ComposedVertex(a(0).toInt, a(1).toInt, a(2).toInt)
    else
      ComposedVertex(a(0).toInt, 1, a(2).toInt)
  }

  def load(fileName:String):ObjMesh = load( getClass.getResourceAsStream(fileName) )

  def load(inputStream: InputStream):ObjMesh = {
    val reader = new BufferedReader( new InputStreamReader( inputStream ) )

    val positions = ArrayBuilder.make[Vec3]
    val texCoords = ArrayBuilder.make[Vec2]
    val normals   = ArrayBuilder.make[Vec3]

    val vertices  = HashMap[ComposedVertex,ComposedVertex]()
    val triangles = ArrayBuilder.make[ComposedTriangle]

    var line = reader.readLine()
    while( line != null ) {
      line.split("\\s+") match {
        case Array("v", x,y,z) =>
          positions += Vec3( x.toDouble, y.toDouble, z.toDouble )
        case Array("vt", u,v) =>
          texCoords += Vec2( u.toDouble, v.toDouble )
        case Array("vn", x,y,z) =>
          normals += Vec3( x.toDouble, y.toDouble, z.toDouble )
        case Array("f", v1str, v2str, v3str) =>
          val cv1 = composedVertex(v1str)
          val v1 = vertices.getOrElseUpdate(cv1,cv1)
          val cv2 = composedVertex(v2str)
          val v2 = vertices.getOrElseUpdate(cv2,cv2)
          val cv3 = composedVertex(v3str)
          val v3 = vertices.getOrElseUpdate(cv3,cv3)

          triangles += ComposedTriangle(v1,v2,v3)
        case Array("f", v1str, v2str, v3str, v4str) =>

          val cv1 = composedVertex(v1str)
          val v1 = vertices.getOrElseUpdate(cv1,cv1)
          val cv2 = composedVertex(v2str)
          val v2 = vertices.getOrElseUpdate(cv2,cv2)
          val cv3 = composedVertex(v3str)
          val v3 = vertices.getOrElseUpdate(cv3,cv3)
          val cv4 = composedVertex(v4str)
          val v4 = vertices.getOrElseUpdate(cv4,cv4)

          triangles += ComposedTriangle(v1,v2,v3)
          triangles += ComposedTriangle(v3,v2,v4)
        case _ =>
          println("cant parse line: "+line)
      }
      line = reader.readLine()
    }
    reader.readLine()

    val posArray = positions.result
    val texCoordsArray = { val result = texCoords.result; if(result.length ==0) Array(Vec2(0.5)) else result }
    val normalsArray = { val result = normals.result; if(result.length ==0) Array(Vec3(0)) else result }

    // normalizing positions to [0..1]
    val min = Vec3(Double.MaxValue)
    val max = Vec3(Double.MinValue)
    for(pos <- posArray){
      if(pos.x < min.x) min.x = pos.x
      if(pos.y < min.y) min.y = pos.y
      if(pos.z < min.z) min.z = pos.z
      if(pos.x > max.x) max.x = pos.x
      if(pos.y > max.y) max.y = pos.y
      if(pos.z > max.z) max.z = pos.z
    }
    for( pos <- posArray ) {
      pos := (pos - min) / (max - min)
    }

    val vertexMapper = HashMap[ComposedVertex,Int]()
    val vertexBuilder = ArrayBuilder.make[Vertex]

    var counter = 0
    for( vert <- vertices.keys ) { 
      val pos:Vec3  = posArray(vert.v-1)
      val texCoord:Vec2 = texCoordsArray(vert.t-1)
      val normal:Vec3   = normalsArray(vert.n-1)

      vertexBuilder += Vertex(pos,texCoord,normal)
      vertexMapper += (vert -> counter)

      counter += 1
    }

    val finalVerts = vertexBuilder.result
    val vertexBuffer = BufferUtils.createFloatBuffer(8 * finalVerts.length)
    for( Vertex(pos, texCoord, normal) <- finalVerts ) {
      vertexBuffer.put(pos.x.toFloat).put(pos.y.toFloat).put(pos.z.toFloat)
      vertexBuffer.put(texCoord.x.toFloat).put(texCoord.y.toFloat)
      vertexBuffer.put(normal.x.toFloat).put(normal.y.toFloat).put(normal.z.toFloat)
    }
    vertexBuffer.flip

    val stride = 4*8
    val offsetPosition = 4*0
    val offsetTexCoord = 4*3
    val offsetNormal   = 4*5

    val finalTriangles = triangles.result
    val vertexIndexBuffer = BufferUtils.createIntBuffer(finalTriangles.length * 3)
    for( ComposedTriangle(v1,v2,v3) <- finalTriangles ) {
      vertexIndexBuffer.put(vertexMapper(v1)).put(vertexMapper(v2)).put(vertexMapper(v3))
    }
    vertexIndexBuffer.flip

    new ObjMesh(vertexBuffer,vertexIndexBuffer)
  }
}
