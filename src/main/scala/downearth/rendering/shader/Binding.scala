package downearth.rendering.shader

import simplex3d.math.double._
import downearth.rendering.Texture
import downearth.util.AddString
import scala.collection.mutable
import simplex3d.math.floatx.{ReadVec4f, ReadVec3f, ReadVec2f, ReadMat4f}

/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:45
 */

abstract class Binding( val program:Program ) extends AddString {
  val attributes:Seq[Attribute[_]]
  val uniforms:Seq[Uniform[_]]

  val changedUniforms = mutable.Queue[Uniform[_]]()
  val changedAttributes = mutable.Queue[Attribute[_]]()

  def bind() {
    bind(attributes, uniforms)
  }

  def bindChanges() {
    val t:Any => Boolean = _ => true
    bind(changedAttributes.dequeueAll(t), changedUniforms.dequeueAll(t))
  }

  private def bind(attributes:Seq[Attribute[_]],uniforms:Seq[Uniform[_]]) {
    require(program.isActive, "program must be active before data is written")

    val groupedAttributes = attributes.groupBy( _.bufferBinding.buffer )

    for( (buffer, atList) <- groupedAttributes ) {
      buffer.bind()
      for( binding <- atList ) {
        binding.writeData()
      }
    }

    for( binding <- uniforms ) {
      binding.writeData()
    }
  }

  def addString(sb:StringBuilder) = {
    sb append program
    attributes.addString(sb,"\n\t","\n\t","\n\t")
    uniforms.addString(sb,"\n\t","\n\t","\n\t")
    sb append "\n"
    sb
  }

  def attributeInt(name:String)   = attributes.find( _.name == name ).get.asInstanceOf[AttributeInt]
  def attributeFloat(name:String) = attributes.find( _.name == name ).get.asInstanceOf[AttributeFloat]
  def attributeVec2f(name:String) = attributes.find( _.name == name ).get.asInstanceOf[AttributeVec2f]
  def attributeVec3f(name:String) = attributes.find( _.name == name ).get.asInstanceOf[AttributeVec3f]
  def attributeVec4f(name:String) = attributes.find( _.name == name ).get.asInstanceOf[AttributeVec4f]

  def uniformFloat(name:String) = uniforms.find( _.name == name ).get.asInstanceOf[UniformFloat]
  def uniformVec2f(name:String) = uniforms.find( _.name == name ).get.asInstanceOf[UniformVec2f]
  def uniformVec3f(name:String) = uniforms.find( _.name == name ).get.asInstanceOf[UniformVec3f]
  def uniformVec4f(name:String) = uniforms.find( _.name == name ).get.asInstanceOf[UniformVec4f]
  def uniformMat4f(name:String) = uniforms.find( _.name == name ).get.asInstanceOf[UniformMat4f]
  def uniformSampler2D(name:String) = uniforms.find( _.name == name ).get.asInstanceOf[UniformSampler2D]
}
