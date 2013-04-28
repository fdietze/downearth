
package downearth

import simplex3d.math.double._
import simplex3d.math.double.functions._
import scala.collection.mutable._

object ConvexHull3d {
  
  def giftWrap(vertices:collection.Seq[Vec3]) = {
    var verts = Array(vertices:_*)
    
    // todo ganz flache datenstruktur verwenden für 0 Garbage collection
    val triangles = new ArrayBuffer[Int](50)
    
    val size = vertices.size
    def swap(i:Int,j:Int){
      val tmp = verts(i)
      verts(i) = verts(j)
      verts(j) = tmp
    }
    
    var i = 1
    
    while( i < size ){
      if( verts(i).x < verts(0).x )
        swap(0,i)
      i += 1
    }
    
    i = 2
    
    while( i < size ){
      if( (verts(0).x-verts(1).x) * (verts(1).y-verts(i).y) > 0 ){
        swap(i,1)
      }
      i += 1
    }
    
    val outline = ListBuffer(0,1)
    var pos = 2
    var osize = outline.size
    while( pos < size ) {
      var mem = -1
      val left  = outline(0 % osize)
      val l1    = outline(1 % osize)
      val l2    = outline(2 % osize)
      val right = outline(3 % osize)
      
      val n = cross(verts(l2)-verts(l1),verts(pos)-verts(l1))
      var h = dot(n,verts(l1))
      
      i = pos + 1
      while( i < size ) {
        val d = dot(n, verts(i))
        if( d == h )
          mem = i
        if( d > h ){
          swap(pos,i)
          n := cross(verts(l2)-verts(l1),verts(pos)-verts(l1))
          h = dot(n,verts(l1))
          if( mem != -1 ){
            if( dot(n, verts(mem)) > h ) {
              swap(pos,mem)
              n := cross(verts(l2)-verts(l1), verts(pos)-verts(l1))
              h = dot(n,verts(l1))
            }
          }
        }
        i += 1
      }
      
      
      if( osize > 2 && dot(n,verts(left)) > h ) {
        n := cross(verts(l2)-verts(l1),verts(left)-verts(l1))
        h  = dot(n,verts(l1))
        if( dot(n,verts(right)) > h ){
          outline remove 1
          osize -= 1
          triangles ++= Seq(l1,l2,right)
        }
        else {
          outline remove 2
          osize -= 1
          triangles ++= Seq(left,l1,l2)
        }
      }
      else if( osize > 2 && dot(n,verts(right)) > h ){
        outline remove 2
        osize -= 1
        triangles ++= Seq(l1,l2,right)
      }
      else {
        outline.insert(2,pos) // zwischen l1 und l2 einfügen
        osize += 1
        triangles ++= Seq(l1,l2,pos)
      }
      pos += 1
      
      // println(outline map verts)
      // println(triangles map verts)
    }
    
    while( osize > 2 ){
      val left  = outline(0)
      val l1    = outline(1)
      val l2    = outline(2)
      val right = outline(3 % osize)
      
      val n = cross(verts(l2)-verts(l1),verts(left)-verts(l1))
      val h = dot(n,verts(l1))
      if( dot(n,verts(right)) > h ) {
        outline remove 1
        triangles ++= Seq(l1,l2,right)
      }
      else {
        outline remove 2
        triangles ++= Seq(left,l1,l2)
      }
      osize -= 1
    }
    
    triangles map verts
  }
}

