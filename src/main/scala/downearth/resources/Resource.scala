package downearth.resources

import collection.mutable
import simplex3d.math.double._
import downearth.rendering.{TextureManager}
import java.io._
import downearth.message.implicits._
import downearth.message
import glwrapper.Texture2D
import downearth.message.MaterialDefinitions.{Property, MaterialSet}
import downearth.message.MaterialDefinitions
import com.google.protobuf.TextFormat


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
  val file = getClass.getClassLoader.getResourceAsStream("materials.conf")

  val materials = new mutable.HashMap[Int, Material] {
    import collection.JavaConversions._
    // takes the material graph and imports it as a flat fast data structure
    def fromMessage(materialSet:MaterialSet) {
      this.clear()
      // add all materials
      for( mat <- materialSet.getMaterialList )
        this += (mat.getId -> Material(
          mat.getId,
          mat.getName,
          mat.getParentList.map(_.toInt).toList,
          mat.getPropertyList.map{p => (p.getKey -> p.getValue)}.toMap)
          )

      // inherit properties
      def collectProperties(id:Int):Map[Material.Property, Double] = this(id).parents.flatMap(collectProperties).toMap ++ this(id).properties
      for( (id,mat) <- this )
        this(id) = mat.copy(properties = collectProperties(id))

      // set indirect parent ids
      def collectParents(id:Int):List[Int] = this(id).parents ::: this(id).parents.flatMap(collectParents)
      for( (id,mat) <- this )
        this(id) = mat.copy(parents = collectParents(id).distinct)

      //println(this.values.mkString("\n"))
    }

    def toMessage:MaterialSet = {
      val setBuilder = MaterialSet.newBuilder
        for( (id,m) <- this )
          setBuilder.addMaterial{
            val matBuilder = MaterialDefinitions.Material.newBuilder
            matBuilder
            .setId(m.id)
            .setName(m.name)

            for( (key,value) <- m.properties )
              matBuilder.addProperty(
                Property.newBuilder
                  .setKey(key)
                  .setValue(value)
                  .build
              )

            for( p <- m.parents )
              matBuilder.addParent(p)

            matBuilder.build
          }
      setBuilder.build
    }
  }

  /*def save() {
    try{
      val fw = new OutputStreamWriter(file)
      TextFormat.print(materials.toMessage,fw)
      fw.close()
    } catch {
      case e:Exception =>
        println("Resources: couldn't write file: " + file)
    }
  }*/

  def load() {
    try{
      val builder = MaterialSet.newBuilder
      val fr = new InputStreamReader(file)
      TextFormat.merge(fr, builder)
      fr.close()
      val msg = builder.build
      materials.fromMessage(msg)
    } catch {
      case e:Exception =>
        println("Resources: couldn't open file: " + file)
    }
  }

  /*val testData = {

    List(
      Material(id =  1, name= "Baumaterial", properties = Map.empty[Material.Property,Double], parents = List.empty[Int]),
      Material(id =  2, name= "Brennstoff",  properties = Map("Brennwert" -> 0.0), parents = List.empty[Int]),
      Material(id =  3, name= "Stein",       properties = Map.empty[Material.Property,Double], parents = List(1)),
      Material(id =  4, name= "Kohle",       properties = Map.empty[Material.Property,Double], parents = List(2)),
      Material(id =  5, name= "Holz",        properties = Map.empty[Material.Property,Double], parents = List(1,2)),
      Material(id =  6, name= "SandStein",   properties = Map.empty[Material.Property,Double], parents = List(3)),
      Material(id =  7, name= "Erz",         properties = Map("Metallanteil" -> 0.0), parents = List(3)),
      Material(id =  8, name= "Granit",      properties = Map.empty[Material.Property,Double], parents = List(3)),
      Material(id =  9, name= "Eisenerz",    properties = Map.empty[Material.Property,Double], parents = List(7)),
      Material(id = 10, name= "Steinkohle",  properties = Map.empty[Material.Property,Double], parents = List(3,4)),
      Material(id = 11, name= "Birke",       properties = Map.empty[Material.Property,Double], parents = List(5)),
      Material(id = 12, name= "Eiche",       properties = Map.empty[Material.Property,Double], parents = List(5))
    )
  }

  materials ++= testData.indices zip testData
  */
  load()
  println(materials.values.mkString("\n"))
  sys.exit(0)
}

class MaterialManager {
  val inset = 1 // material separation texture atlas border for texture coordinates
  val textureAtlas = TextureManager.materials
  val materialCount = 4//textureAtlas.width / textureAtlas.height
  val materials = Array.tabulate(materialCount){ id =>
    Material(id,
      texture = textureAtlas,
      texPos  = Vec2(id.toDouble/materialCount + inset,0 + inset),
      texSize = Vec2(1.0/materialCount - 2*inset,1 - 2*inset))
  }
}
