package downearth.resources

import collection.mutable
import simplex3d.math.double._
import downearth.rendering.{TextureManager}
import java.io._
import downearth.message.implicits._
import downearth.message
import glwrapper.Texture2D
import downearth.message.MaterialDefinitions.{Materials, Property}
import downearth.message.MaterialDefinitions
import com.google.protobuf.TextFormat
import java.net.URI


abstract class Resource


object Material {
  type Property = String

  // map material definition from WorldDefinition
  def apply(name:String, r:Double, g:Double, b:Double):Material = Resources.materials(name)

}

case class Material(
                    id:Int,
                    name:String = "",
                    parents:List[Int] = Nil,
                    properties:Map[Material.Property, Double] = Map.empty,
                    texId:Int = 0
                   ) extends Resource {
  override def toString = s"Material($name($id)${parents.map(Resources.materials(_).name).mkString(" is ",",","")}${properties.map{case (name,value) => s"$name=$value"}.mkString(" with ",",","")}, texId = $texId)"
}

class Product extends Resource

object Resources {
  val file = getClass.getClassLoader.getResourceAsStream("materials.conf")

  var textures = Array[String]()
  val materials = new mutable.HashMap[Int, Material] {
    def apply(name:String):Material = this.values.find(_.name == name).get

    // takes the material graph and imports it as a flat fast data structure
    def fromMessage(materialSet:Materials) {
      import collection.JavaConversions.{asScalaBuffer}
      this.clear()
      // add all materials and generate ids
      var id = 0
      for( mat <- materialSet.getMaterialList ) {
        this += (id -> Material(
          id,
          mat.getName,
          Nil,
          mat.getPropertyList.map{p => (p.getKey -> p.getValue)}.toMap
        ))
        id += 1
      }

      require(this.values.map(_.name).toList.distinct.size == this.size)

      val nameToId = (this map {case (id,mat) => (mat.name,mat.id)}).toMap

      // set the parent ids
      for( mat <- materialSet.getMaterialList ) {
        val id = nameToId(mat.getName)
        this(id) = this(id).copy(parents = mat.getParentList.toList.map(nameToId))
      }

      // check for available textures,
      // else dummy texture
      val materialNames = this.values.map(_.name)
      val materialsWithFiles = this.values filter { (m:Material) =>
        val fileName = "materials/" + m.name.toLowerCase + ".png"
        val url = getClass.getClassLoader.getResource(fileName)
        val fileExists = url != null
        fileExists
      }
      for( (mat,i) <- materialsWithFiles zipWithIndex ) {
        val texId = i+1
        this(mat.id) = mat.copy(texId = texId)
      }
      textures = Array("materials/dummy.png") ++ materialsWithFiles.map("materials/" + _.name.toLowerCase + ".png")

      println(textures.mkString("\n"))

      // inherit texIds
      def collectTexIds(id:Int):Int = {
          if( this(id).texId == 0 )
            this(id).parents.map(collectTexIds).lastOption getOrElse 0
          else this(id).texId
      }
      for( (id,mat) <- this if mat.texId == 0 )
        this(id) = mat.copy(texId = collectTexIds(id))

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

    def toMessage:Materials = {
      val materialsBuilder = Materials.newBuilder
        for( (id,m) <- this )
          materialsBuilder.addMaterial{
            val matBuilder = MaterialDefinitions.Material.newBuilder
            matBuilder
            .setName(m.name)

            for( (key,value) <- m.properties )
              matBuilder.addProperty(
                Property.newBuilder
                  .setKey(key)
                  .setValue(value)
                  .build
              )

            for( p <- m.parents )
              matBuilder.addParent(this(p).name)

            matBuilder.build
          }
      materialsBuilder.build
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
      val builder = Materials.newBuilder
      val fr = new InputStreamReader(file)
      TextFormat.merge(fr, builder)
      fr.close()
      val msg = builder.build
      materials.fromMessage(msg)
    } catch {
      case e:Exception =>
        println("Resources: couldn't open file: " + file)
        e.printStackTrace()
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
}
