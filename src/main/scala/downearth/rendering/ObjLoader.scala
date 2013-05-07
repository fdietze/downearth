package downearth.rendering

import simplex3d.math.double._

import downearth.ObjMesh

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
          val a1 = v1str.split('/').map(_.toInt)
          val cv1 = ComposedVertex(a1(0),a1(1),a1(2))
          val v1 = vertices.getOrElseUpdate(cv1,cv1)

          val a2 = v2str.split('/').map(_.toInt)
          val cv2 = ComposedVertex(a2(0),a2(1),a2(2))
          val v2 = vertices.getOrElseUpdate(cv2,cv2)

          val a3 = v3str.split('/').map(_.toInt)
          val cv3 = ComposedVertex(a3(0),a3(1),a3(2))
          val v3 = vertices.getOrElseUpdate(cv3,cv3)

          triangles += ComposedTriangle(v1,v2,v3)
        case _ =>
          println("cant parse line: "+line)
      }
      line = reader.readLine()
    }
    reader.readLine()

    val posArray = positions.result
    val texCoordsArray = texCoords.result
    val normalsArray = normals.result

    val vertexMapper = HashMap[ComposedVertex,Int]()
    val vertexBuilder = ArrayBuilder.make[Vertex]

    var counter = 0
    for( vert <- vertices.keys ) { 
      val pos  = posArray(vert.v-1)
      val texCoord = texCoordsArray(vert.t-1)
      val normal   = normalsArray(vert.n-1)

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
