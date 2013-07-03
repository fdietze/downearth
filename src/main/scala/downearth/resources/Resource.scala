package downearth.resources

import collection.mutable
import downearth.rendering.Texture2D
import simplex3d.math.double._
import downearth.rendering.{Texture2D, TextureManager}
import java.io.{File, FileInputStream}
import downearth.message.implicits._
import downearth.message


abstract class Resource


object Material {
  type Property = String

  // map material definition from WorldDefinition
  def apply(id:Int, r:Double, g:Double, b:Double):Material = Resources.materials(id)

}

case class Material(
                     id:Int,
                     name:String = "",
                     parents:List[Int] = Nil,
                     properties:Map[Material.Property, Double] = Map.empty,

                     texture:Texture2D = null,
                     texPos:Vec2 = null,
                     texSize:Vec2 = null
                    ) extends Resource {
  override def toString = s"Material($name($id)${parents.map(Resources.materials(_).name).mkString(" is ",",","")}${properties.map{case (name,value) => s"$name=$value"}.mkString(" with ",",","")})"
}

class Product extends Resource

object Resources {
  val file = new File("materialdefinitions.bin")

  val testData = {
    import downearth.{message => m}
    m.MaterialSet(Vector(
      m.Material(id =  1, "Baumaterial", properties = Vector.empty, parents = Vector.empty),
      m.Material(id =  2, "Brennstoff",  properties = Vector(m.Property("Brennwert",0)), parents = Vector.empty),
      m.Material(id =  3, "Stein",       properties = Vector.empty, parents = Vector(1)),
      m.Material(id =  4, "Kohle",       properties = Vector.empty, parents = Vector(2)),
      m.Material(id =  5, "Holz",        properties = Vector.empty, parents = Vector(1,2)),
      m.Material(id =  6, "SandStein",   properties = Vector.empty, parents = Vector(3)),
      m.Material(id =  7, "Erz",         properties = Vector(m.Property("Metallanteil",0)), parents = Vector(3)),
      m.Material(id =  8, "Granit",      properties = Vector.empty, parents = Vector(3)),
      m.Material(id =  9, "Eisenerz",    properties = Vector.empty, parents = Vector(7)),
      m.Material(id = 10, "Steinkohle",  properties = Vector.empty, parents = Vector(3,4)),
      m.Material(id = 11, "Birke",       properties = Vector.empty, parents = Vector(5)),
      m.Material(id = 12, "Eiche",       properties = Vector.empty, parents = Vector(5))
    ))
  }

  val materials = new mutable.HashMap[Int, Material] {
    // takes the material graph and imports it as a flat fast data structure
    def fromMessage(materialSet:message.MaterialSet) {
      this.clear()
      // add all materials
      for( mat <- materialSet.set )
        this += (mat.id -> Material(
          mat.id,
          mat.name,
          mat.parents.toList,
          mat.properties.map{case message.Property(name,value) => (name -> value)}.toMap)
          )

      // inherit properties
      def collectProperties(id:Int):Map[Material.Property, Double] = this(id).parents.flatMap(collectProperties).toMap ++ this(id).properties
      for( (id,mat) <- this )
        this(id) = mat.copy(properties = collectProperties(id))

      // set indirect parent ids
      def collectParents(id:Int):List[Int] = this(id).parents ::: this(id).parents.flatMap(collectParents)
      for( (id,mat) <- this )
        this(id) = mat.copy(parents = collectParents(id).distinct)

      println(this.values.mkString("\n"))
    }
  }

  materials.fromMessage(testData)
  materials(0) = Material(0)

  def load() {
    println("Resources: Loading Material Definitions")
    try{
      if( file.exists() ) {
        val fis = new FileInputStream(file)
        val msg = message.MaterialSet().mergeFrom(fis)
        println(msg)
        materials.fromMessage(msg)
      }
    } catch {
      case e:Exception =>
        e.printStackTrace()
        println("couldn't open file: " + file)
    }
  }
}



object MaterialManager {
  val inset = 1 // material separation texture atlas border for texture coordinates
  val textureAtlas = TextureManager.materials
  val materialCount = textureAtlas.width / textureAtlas.height
  val materials = Array.tabulate(materialCount){ id =>
    Material(id,
      texture = textureAtlas,
      texPos  = Vec2(id.toDouble/materialCount + inset,0 + inset),
      texSize = Vec2(1.0/materialCount - 2*inset,1 - 2*inset))
  }
}
