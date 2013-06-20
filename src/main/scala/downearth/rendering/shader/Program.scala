package downearth.rendering.shader


import org.lwjgl.opengl._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
//import simplex3d.backend.lwjgl.ArbEquivalents.GL20._

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL13._

import downearth.rendering._
import downearth.util.AddString
import org.lwjgl.opengl.GL15._

/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:47
 */
object Program {
  def apply(name:String)(vertexShaders: VertexShader*)(fragmentShaders: FragmentShader*) = {


    val shaderList = vertexShaders ++ fragmentShaders

    val program = new Program(name)
    program.create()

    for(shader <- shaderList)
      program attach shader

    program.link()

//    for(shader <- shaderList)
//      program detach shader

    program
  }

  import GL21._
  import GL30._
  import GL31._
  import GL32._

  def isSampler(_type:Int) = samplers contains _type
  val samplers = Set(
    GL_SAMPLER_1D,
    GL_SAMPLER_2D,
    GL_SAMPLER_3D,
    GL_SAMPLER_CUBE,
    GL_SAMPLER_1D_SHADOW,
    GL_SAMPLER_2D_SHADOW ,
    GL_SAMPLER_1D_ARRAY,
    GL_SAMPLER_2D_ARRAY,
    GL_SAMPLER_1D_ARRAY_SHADOW,
    GL_SAMPLER_2D_ARRAY_SHADOW,
    GL_SAMPLER_2D_MULTISAMPLE,
    GL_SAMPLER_2D_MULTISAMPLE_ARRAY,
    GL_SAMPLER_CUBE_SHADOW,
    GL_SAMPLER_BUFFER,
    GL_SAMPLER_2D_RECT,
    GL_SAMPLER_2D_RECT_SHADOW,
    GL_INT_SAMPLER_1D,
    GL_INT_SAMPLER_2D,
    GL_INT_SAMPLER_3D,
    GL_INT_SAMPLER_CUBE,
    GL_INT_SAMPLER_1D_ARRAY,
    GL_INT_SAMPLER_2D_ARRAY,
    GL_INT_SAMPLER_2D_MULTISAMPLE,
    GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY,
    GL_INT_SAMPLER_BUFFER,
    GL_INT_SAMPLER_2D_RECT
  )


  import GL41._
  import GL42._

  val shaderTypeString =   Map(
    GL_FLOAT -> "float",
    GL_FLOAT_VEC2 -> "vec2",
    GL_FLOAT_VEC3 -> "vec3",
    GL_FLOAT_VEC4 -> "vec4",
    GL_DOUBLE -> "double",
    GL_DOUBLE_VEC2 -> "dvec2",
    GL_DOUBLE_VEC3 -> "dvec3",
    GL_DOUBLE_VEC4 -> "dvec4",
    GL_INT -> "int",
    GL_INT_VEC2 -> "ivec2",
    GL_INT_VEC3 -> "ivec3",
    GL_INT_VEC4 -> "ivec4",
    GL_UNSIGNED_INT -> "unsigned int",
    GL_UNSIGNED_INT_VEC2 -> "uvec2",
    GL_UNSIGNED_INT_VEC3 -> "uvec3",
    GL_UNSIGNED_INT_VEC4 -> "uvec4",
    GL_BOOL -> "bool",
    GL_BOOL_VEC2 -> "bvec2",
    GL_BOOL_VEC3 -> "bvec3",
    GL_BOOL_VEC4 -> "bvec4",
    GL_FLOAT_MAT2 -> "mat2",
    GL_FLOAT_MAT3 -> "mat3",
    GL_FLOAT_MAT4 -> "mat4",
    GL_FLOAT_MAT2x3 -> "mat2x3",
    GL_FLOAT_MAT2x4 -> "mat2x4",
    GL_FLOAT_MAT3x2 -> "mat3x2",
    GL_FLOAT_MAT3x4 -> "mat3x4",
    GL_FLOAT_MAT4x2 -> "mat4x2",
    GL_FLOAT_MAT4x3 -> "mat4x3",
    GL_DOUBLE_MAT2 -> "dmat2",
    GL_DOUBLE_MAT3 -> "dmat3",
    GL_DOUBLE_MAT4 -> "dmat4",
    GL_DOUBLE_MAT2x3 -> "dmat2x3",
    GL_DOUBLE_MAT2x4 -> "dmat2x4",
    GL_DOUBLE_MAT3x2 -> "dmat3x2",
    GL_DOUBLE_MAT3x4 -> "dmat3x4",
    GL_DOUBLE_MAT4x2 -> "dmat4x2",
    GL_DOUBLE_MAT4x3 -> "dmat4x3",
    GL_SAMPLER_1D -> "sampler1D",
    GL_SAMPLER_2D -> "sampler2D",
    GL_SAMPLER_3D -> "sampler3D",
    GL_SAMPLER_CUBE -> "samplerCube",
    GL_SAMPLER_1D_SHADOW -> "sampler1DShadow",
    GL_SAMPLER_2D_SHADOW -> "sampler2DShadow",
    GL_SAMPLER_1D_ARRAY -> "sampler1DArray",
    GL_SAMPLER_2D_ARRAY -> "sampler2DArray",
    GL_SAMPLER_1D_ARRAY_SHADOW -> "sampler1DArrayShadow",
    GL_SAMPLER_2D_ARRAY_SHADOW -> "sampler2DArrayShadow",
    GL_SAMPLER_2D_MULTISAMPLE -> "sampler2DMS",
    GL_SAMPLER_2D_MULTISAMPLE_ARRAY -> "sampler2DMSArray",
    GL_SAMPLER_CUBE_SHADOW -> "samplerCubeShadow",
    GL_SAMPLER_BUFFER -> "samplerBuffer",
    GL_SAMPLER_2D_RECT -> "sampler2DRect",
    GL_SAMPLER_2D_RECT_SHADOW -> "sampler2DRectShadow",
    GL_INT_SAMPLER_1D -> "isampler1D",
    GL_INT_SAMPLER_2D -> "isampler2D",
    GL_INT_SAMPLER_3D -> "isampler3D",
    GL_INT_SAMPLER_CUBE -> "isamplerCube",
    GL_INT_SAMPLER_1D_ARRAY -> "isampler1DArray",
    GL_INT_SAMPLER_2D_ARRAY -> "isampler2DArray",
    GL_INT_SAMPLER_2D_MULTISAMPLE -> "isampler2DMS",
    GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY -> "isampler2DMSArray",
    GL_INT_SAMPLER_BUFFER -> "isamplerBuffer",
    GL_INT_SAMPLER_2D_RECT -> "isampler2DRect",
    GL_UNSIGNED_INT_SAMPLER_1D -> "usampler1D",
    GL_UNSIGNED_INT_SAMPLER_2D -> "usampler2D",
    GL_UNSIGNED_INT_SAMPLER_3D -> "usampler3D",
    GL_UNSIGNED_INT_SAMPLER_CUBE -> "usamplerCube",
    GL_UNSIGNED_INT_SAMPLER_1D_ARRAY -> "usampler2DArray",
    GL_UNSIGNED_INT_SAMPLER_2D_ARRAY -> "usampler2DArray",
    GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE -> "usampler2DMS",
    GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY -> "usampler2DMSArray",
    GL_UNSIGNED_INT_SAMPLER_BUFFER -> "usamplerBuffer",
    GL_UNSIGNED_INT_SAMPLER_2D_RECT -> "usampler2DRect",
    GL_IMAGE_1D -> "image1D",
    GL_IMAGE_2D -> "image2D",
    GL_IMAGE_3D -> "image3D",
    GL_IMAGE_2D_RECT -> "image2DRect",
    GL_IMAGE_CUBE -> "imageCube",
    GL_IMAGE_BUFFER -> "imageBuffer",
    GL_IMAGE_1D_ARRAY -> "image1DArray",
    GL_IMAGE_2D_ARRAY -> "image2DArray",
    GL_IMAGE_2D_MULTISAMPLE -> "image2DMS",
    GL_IMAGE_2D_MULTISAMPLE_ARRAY -> "image2DMSArray",
    GL_INT_IMAGE_1D -> "iimage1D",
    GL_INT_IMAGE_2D -> "iimage2D",
    GL_INT_IMAGE_3D -> "iimage3D",
    GL_INT_IMAGE_2D_RECT -> "iimage2DRect",
    GL_INT_IMAGE_CUBE -> "iimageCube",
    GL_INT_IMAGE_BUFFER -> "iimageBuffer",
    GL_INT_IMAGE_1D_ARRAY -> "iimage1DArray",
    GL_INT_IMAGE_2D_ARRAY -> "iimage2DArray",
    GL_INT_IMAGE_2D_MULTISAMPLE -> "iimage2DMS",
    GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY -> "iimage2DMSArray",
    GL_UNSIGNED_INT_IMAGE_1D -> "uimage1D",
    GL_UNSIGNED_INT_IMAGE_2D -> "uimage2D",
    GL_UNSIGNED_INT_IMAGE_3D -> "uimage3D",
    GL_UNSIGNED_INT_IMAGE_2D_RECT -> "uimage2DRect",
    GL_UNSIGNED_INT_IMAGE_CUBE -> "uimageCube",
    GL_UNSIGNED_INT_IMAGE_BUFFER -> "uimageBuffer",
    GL_UNSIGNED_INT_IMAGE_1D_ARRAY -> "uimage1DArray",
    GL_UNSIGNED_INT_IMAGE_2D_ARRAY -> "uimage2DArray",
    GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE -> "uimage2DMS",
    GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY -> "uimage2DMSArray",
    GL_UNSIGNED_INT_ATOMIC_COUNTER -> "atomic_uint"
  )

}

class Program(val name:String) { program =>
  var id = 0

  def attributeLocation(name:CharSequence) = glGetAttribLocation(id,name)
  def uniformLocation(name:CharSequence) = glGetUniformLocation(id,name)

  override def toString = name

  def getBinding = {
    val numUniforms   = glGetProgrami(id, GL_ACTIVE_UNIFORMS)
    val numAttributes = glGetProgrami(id, GL_ACTIVE_ATTRIBUTES)

    val sizetypeBuffer = BufferUtils.createIntBuffer(2)

    var currentSampler = 0

    new Binding(program) { binding =>
      val attributes =
        for( location <- 0 until numAttributes ) yield {
          val name = glGetActiveAttrib(id, location, 1000, sizetypeBuffer)
          val size = sizetypeBuffer.get(0)
          if( size != 1 ) {
            throw new NotImplementedError("currently not supported attribute size: "+size)
          }
          val glType = sizetypeBuffer.get(1)

          val bufferBinding = {
            val b = new ArrayGlBuffer()
            b.create()
            b.bind()
            val bb = new BufferBinding(
              buffer = b,
              size = glType match {
                case GL_FLOAT | GL_INT => 1
                case GL_FLOAT_VEC2 => 2
                case GL_FLOAT_VEC3 => 3
                case GL_FLOAT_VEC4 => 4
                case _ => ??? // TODO implement other types
              },
              glType = glType match {
                case GL_FLOAT => GL_FLOAT
                case GL_FLOAT_VEC2 => GL_FLOAT
                case GL_FLOAT_VEC3 => GL_FLOAT
                case GL_FLOAT_VEC4 => GL_FLOAT
                case _ => glType // TODO implement other types
              },
              normalized = false,
              stride = size * glType match {
                case GL_FLOAT | GL_INT => 4
                case GL_FLOAT_VEC2 => 8
                case GL_FLOAT_VEC3 => 12
                case GL_FLOAT_VEC4 => 16
              },
              offset = 0
            )
            glBindBuffer(b.target,0)
            Util.checkGLError()
            bb
          }

          glType match {
            case GL_FLOAT =>
              new AttributeFloat(program, binding, name, location, bufferBinding)
            case GL_FLOAT_VEC2 =>
              new AttributeVec2f(program, binding, name, location, bufferBinding)
            case GL_FLOAT_VEC3 =>
              new AttributeVec3f(program, binding, name, location, bufferBinding)
            case GL_FLOAT_VEC4 =>
              new AttributeVec4f(program, binding, name, location, bufferBinding)
            case _ =>
              throw new NotImplementedError("currently not supported attribute type: "+Program.shaderTypeString(glType) )

          }
        }

      val uniforms:Seq[Uniform[_]] =
        for( i <- 0 until numUniforms) yield {
          val name = glGetActiveUniform(id, i, 1000, sizetypeBuffer)
          val size = sizetypeBuffer.get(0)
          val _type = sizetypeBuffer.get(1)
          val location = glGetUniformLocation(id, name)

          println(s"size:$size type:${_type}")

          val config = new UniformConfig(
            program = program,
            binding = binding,
            name = name,
            location = location,
            glType = _type,
            size = size
          )

          if( Program.isSampler(_type) ) {
            val uniform = new UniformSampler2D(
              currentSampler,
              config
            )
            currentSampler += 1
            uniform
          }
          else {
            _type match {
              case GL_FLOAT =>
                new UniformFloat(config)
              case GL_FLOAT_VEC2 =>
                new UniformVec2f(config)
              case GL_FLOAT_VEC3 =>
                new UniformVec3f(config)
              case GL_FLOAT_VEC4 =>
                new Vec4Uniform(config)
              case GL_FLOAT_MAT4 =>
                new UniformMat4f(config)
              case _ =>
                throw new NotImplementedError("currently not supported uniform type: "+Program.shaderTypeString(_type) )
            }
          }
        }
    }
  }

  def setupTransformFeedbackBuffer() {
    import EXTTransformFeedback._
    import ARBUniformBufferObject._

    val attr = Array[CharSequence]("gl_Position")
    Util.checkGLError()
    // generating the buffer, note that GL_TRANSFORM_FEEDBACK_BUFFER is NOT a buffer type
    val tfvbo = glGenBuffers
    glBindBuffer( GL_ARRAY_BUFFER, tfvbo )
    glBufferData( GL_ARRAY_BUFFER, BufferUtils.createFloatBuffer(2*3*4), GL_DYNAMIC_DRAW )

    // bind the TFB to get the feedback;  MUST be done here, not in display() !
    glBindBufferBase( GL_TRANSFORM_FEEDBACK_BUFFER_EXT, 0, tfvbo )
    glTransformFeedbackVaryingsEXT( id, attr, GL_INTERLEAVED_ATTRIBS_EXT )

    Util.checkGLError()
  }



  def create() {
    id = glCreateProgram
  }

  def attach(shader:Shader) {
    glAttachShader(id, shader.id)
  }

  def link() {
    glLinkProgram(id)
    checkLinkStatus()
  }

  def use() {
    glUseProgram(id)
  }

  def use(block: => Unit) {
    glUseProgram(id)
    block
    glUseProgram(0)
  }

  def isActive = id == glGetInteger(GL_CURRENT_PROGRAM)

  def delete() {
    glDeleteProgram(id)
    id = 0
  }

  def detach(shader:Shader) {
    glDetachShader(id, shader.id)
  }

  def checkLinkStatus() {
    val status = glGetProgrami(id, GL_LINK_STATUS)
    if (status == GL_FALSE)
    {
      val infoLogLength = glGetProgrami(id, GL_INFO_LOG_LENGTH)
      val strInfoLog = glGetProgramInfoLog(id, infoLogLength)
      throw new ShaderLinkError(s"Linker failure: $strInfoLog\n")
    }
  }

  override def finalize() {
    if( id != 0 ) {
      delete()
    }
  }
}
