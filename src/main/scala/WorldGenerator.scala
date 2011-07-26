package xöpäx

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import simplex3d.noise._

import Util._

final class FloatNoise(source: NoiseSource) {
	def apply(x: Double) :Float = source(x).toFloat
	def apply(u: inVec2) :Float = source(u.x, u.y).toFloat
	def apply(u: inVec3) :Float = source(u.x, u.y, u.z).toFloat
	def apply(u: inVec4) :Float = source(u.x, u.y, u.z, u.w).toFloat
}

object WorldGenerator {
	val noise1 = new FloatNoise(ClassicalGradientNoise)
	val cubesize = 64
	val densityfunction:(Vec3 => Float) = hypermegadangerous _
	
	def genWorld:WorldOctree = {
		genWorldAt(Vec3i(-cubesize/2),cubesize)
	}

	def genSlice(nodepos:Vec3i ,nodesize:Int, size:Vec3i) = {
		assert(size.x == 1 || size.y == 1 || size.z == 1)
		val data = new Array3D[Octant](size)
		for(vi <- Vec3i(0) until size){
			val npos = nodepos+vi*nodesize
			val insertion = genWorldAt(npos,nodesize)
			insertion.genMesh
			data(vi) = insertion.root
		}
		data
	}

	def genWorldAt(nodepos:Vec3i,nodesize:Int):WorldOctree = {
		import MarchingHexaeder._
		
		val noiseData = new Array3D[Float](Vec3i(nodesize+3))
		//braucht eine zusätzliche größe um 2 damit die Nachbarn besser angrenzen können
		val exactCaseData = new Array3D[Short](Vec3i(nodesize+2))
		
		def extractData(pos:Vec3i) = {
			assert(exactCaseData.indexInRange(pos))
			offset map (o => noiseData(pos+o))
		}

		time("noiseData.fill: "){	noiseData.fill(v =>	densityfunction(nodepos+v-1) ) }
		
		val casecounter = new Array[Int](22)

		time("exactCaseData: "){
			for( coord <- Vec3i(0) until Vec3i(nodesize+2) ){
				val exactCase = dataToCase(extractData(coord))
				exactCaseData(coord) = exactCase.toShort
			}
		}
		
		time("caseTypeData, transformToStable: "){
			for( coord <- Vec3i(0) until Vec3i(nodesize+2) ) {
				val data = extractData(coord)
				val exactCase = exactCaseData(coord)
				val caseType = caseTypeLookup(exactCase)
			
				if( !isStableCase(caseType) ) {
					val (newData, newCase) = transformToStable(data,exactCase)
					noiseData(coord) = newData
					exactCaseData(coord) = newCase.toShort		
				}
			}
		}

		println("nodepos: " + nodepos)
		def fillfun(v:Vec3i) = {
			val arraypos = v + 1 - nodepos
			val h = data2hexaeder(extractData(arraypos), exactCaseData(arraypos))
			if( h.noVolume )
				EmptyHexaeder
			else h
		}
		
		val octree = new WorldOctree( nodesize, nodepos.clone )
		time("cube.fill(data2hexaeder): "){
			octree.fill( fillfun _ )
		}
		assert(octree.rootNodePos == nodepos)
		assert(octree.rootNodeSize == nodesize)
		octree
	}

	def sphere(v:Vec3) = { val vt = v - 8; -(vt.x*vt.x + vt.y*vt.y + vt.z*vt.z - 64f) }
	def smooth(v:Vec3) = noise1(v*0.1f )
	def plane(v:Vec3) = {0.9f*v.x + 0.98f*v.y + 1.01f*v.z -15.3f}

	def desert(originalsource:Vec3) = {
		val source = (originalsource.xzy - cubesize/2) * Vec3(1,-1,1)
		def sourcey() = {source.y}
		def sourcex() = {source.x}
		def min(xs:Seq[Float]=Nil) = {xs.min}
		def fnoise(v:Seq[Vec3]=Seq(Vec3(0)), x:Seq[Float]=Nil, y:Seq[Float]=Nil, z:Seq[Float]=Nil, plus:Seq[Float]=Nil, minus:Seq[Float]=Nil, size:Float, outscale:Float, outoffset:Float) = {
		val invexpsize = pow(256,((0.5f-size)*2f))
		val expoutscale = pow(256,((outscale-0.5f)*2f))
		val linearoutoffset = (outoffset-0.5f)*2f
		val sumv = v.reduce( (x,y) => x+y ) + Vec3(x.sum,y.sum,z.sum);
		(noise1(sumv*invexpsize)+linearoutoffset)*expoutscale + plus.sum - minus.sum
		}
		def sourcev() = {source}
		def sourcez() = {source.z}
			
		val vn2_sourcez = sourcez()
		val vn2_sourcey = sourcey()
		val vn2_sourcex = sourcex()
		val vn2_sourcev = sourcev()
		val vn16_fnoise = fnoise(Seq(vn2_sourcev), Nil, Nil, Nil, Nil, Nil, 0.93f, 0.81f, 0.57f)
		val vn1_fnoise = fnoise(Seq(vn2_sourcev), Nil, Nil, Nil, Seq(vn2_sourcey), Nil, 0.85f, 0.79f, 0.5f)
		val vn4_fnoise = fnoise(Seq(Vec3(0)), Nil, Seq(vn2_sourcey, vn16_fnoise), Nil, Nil, Nil, 0.64f, 0.51f, 0.74f)
		val vn14_min = min(Seq(vn4_fnoise, vn1_fnoise))
		
		vn14_min
	}

	def dangerous(originalsource:Vec3) = {
		val source = (originalsource.xzy - cubesize/2) * Vec3(1,-1,1)
		def sourcey() = {source.y}
		def sourcex() = {source.x}
		def vec3p(v:Vec3=Vec3(0), factor:Float) = {v*(pow(256,((factor-0.5f)*2f)))}
		def fnoise(v:Seq[Vec3]=Seq(Vec3(0)), x:Seq[Float]=Nil, y:Seq[Float]=Nil, z:Seq[Float]=Nil, plus:Seq[Float]=Nil, minus:Seq[Float]=Nil, size:Float, outscale:Float, outoffset:Float) = {
			val invexpsize = pow(256,((0.5f-size)*2f))
			val expoutscale = pow(256,((outscale-0.5f)*2f))
			val linearoutoffset = (outoffset-0.5f)*2f
			val sumv = v.reduce( (x,y) => x+y ) + Vec3(x.sum,y.sum,z.sum);
			(noise1(sumv*invexpsize)+linearoutoffset)*expoutscale + plus.sum - minus.sum
		}
		def sourcev() = {source}
		def vec3y(v:Vec3=Vec3(0)) = {v.y}
		def sourcez() = {source.z}
		
		val vn2_sourcez = sourcez()
		val vn2_sourcey = sourcey()
		val vn2_sourcex = sourcex()
		val vn2_sourcev = sourcev()
		val vn6_vec3p = vec3p(vn2_sourcev, 0.4f)
		val vn8_vec3y = vec3y(vn6_vec3p)
		val vn4_fnoise = fnoise(Seq(vn6_vec3p), Nil, Nil, Nil, Nil, Nil, 0.73f, 0.83f, 0.5f)
		val vn3_fnoise = fnoise(Seq(vn6_vec3p), Nil, Seq(vn4_fnoise), Nil, Seq(vn8_vec3y), Nil, 0.8f, 0.74f, 0.5f)
		
		vn3_fnoise
	}

	def tunnels(originalsource:Vec3) = {
		val source = (originalsource.xzy - cubesize/2 +Vec3(1243,1253,1346)) * Vec3(1,-1,1)

		def product_1308958664(xs:Seq[Float]=Nil) = {xs.product}
		def constantm1_1308958664(factor:Float) = {(factor-0.5f)*2f}
		def fsrcx_1308958664(scale:Float, x:Float, y:Float, z:Float) = {(source.x + (x-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
		def fnoise(v:Seq[Vec3]=Seq(Vec3(0)), x:Seq[Float]=Nil, y:Seq[Float]=Nil, z:Seq[Float]=Nil, add:Seq[Float]=Nil, sub:Seq[Float]=Nil, size:Float, scale:Float, offset:Float) = {
					val invexpsize = pow(256,((0.5f-size)*2f))
					val expoutscale = pow(256,((scale-0.5f)*2f))/invexpsize
					val linearoutoffset = (offset-0.5f)*2f
					val sumv = v.reduce( (x,y) => x+y ) + Vec3(x.sum,y.sum,z.sum);
					(noise1(sumv*invexpsize)+linearoutoffset)*expoutscale + add.sum - sub.sum
					}
		def fnoise_1308958664(v:Seq[Vec3]=Seq(Vec3(0)), x:Seq[Float]=Nil, y:Seq[Float]=Nil, z:Seq[Float]=Nil, add:Seq[Float]=Nil, sub:Seq[Float]=Nil, size:Float, scale:Float, offset:Float) = {
					val invexpsize = pow(256,((0.5f-size)*2f))
					val expoutscale = pow(256,((scale-0.5f)*2f))/invexpsize
					val linearoutoffset = (offset-0.5f)*2f
					val sumv = v.reduce( (x,y) => x+y ) + Vec3(x.sum,y.sum,z.sum);
					(noise1(sumv*invexpsize)+linearoutoffset)*expoutscale + add.sum - sub.sum
					}
		def product(xs:Seq[Float]=Nil) = {xs.product}
		def fsrcz_1308958664(scale:Float, x:Float, y:Float, z:Float) = {(source.z + (z-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
		def fsrcy_1308958664(scale:Float, x:Float, y:Float, z:Float) = {(source.y + (y-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
		def fsrcv_1308958664(scale:Float, x:Float, y:Float, z:Float) = {(source + (Vec3(x,y,z)-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
		def vec3p(v:Vec3=Vec3(0), s:Float) = {v*s}
		def constant256(factor:Float) = {pow(256,((factor-0.5f)*2f))}
		def min(xs:Seq[Float]=Nil) = {xs.min}
		def constant256_1308958664(factor:Float) = {pow(256,((factor-0.5f)*2f))}
		def vec3(x:Seq[Float], y:Seq[Float], z:Seq[Float]) = {Vec3(x.sum,y.sum,z.sum)}

		val vn1_fsrcz_1308958664 = fsrcz_1308958664(0.76f, 0.5f, 0.5f, 0.5f)
		val vn1_fsrcy_1308958664 = fsrcy_1308958664(0.76f, 0.5f, 0.5f, 0.5f)
		val vn1_fsrcx_1308958664 = fsrcx_1308958664(0.76f, 0.5f, 0.5f, 0.5f)
		val vn1_fsrcv_1308958664 = fsrcv_1308958664(0.76f, 0.5f, 0.5f, 0.5f)
		val vn36_constant256 = constant256(0.33f)
		val vn34_vec3 = vec3(Seq(vn1_fsrcx_1308958664), Nil, Seq(vn1_fsrcz_1308958664))
		val vn33_vec3 = vec3(Nil, Seq(vn1_fsrcy_1308958664), Seq(vn1_fsrcz_1308958664))
		val vn35_vec3 = vec3(Seq(vn1_fsrcx_1308958664), Seq(vn1_fsrcy_1308958664), Nil)
		val vn31_vec3p = vec3p(vn34_vec3, vn36_constant256)
		val vn27_vec3p = vec3p(vn33_vec3, vn36_constant256)
		val vn32_vec3p = vec3p(vn35_vec3, vn36_constant256)
		val vn8_constant256_1308958664 = constant256_1308958664(0.0f)
		val vn37_constant256 = constant256(0.7f)
		val vn23_fnoise = fnoise(Seq(vn31_vec3p), Nil, Nil, Nil, Nil, Nil, 0.5f, 0.5f, 0.5f)
		val vn25_fnoise = fnoise(Seq(vn27_vec3p), Nil, Nil, Nil, Nil, Nil, 0.5f, 0.5f, 0.5f)
		val vn24_fnoise = fnoise(Seq(vn32_vec3p), Nil, Nil, Nil, Nil, Nil, 0.5f, 0.5f, 0.5f)
		val vn15_constantm1_1308958664 = constantm1_1308958664(0.88f)
		val vn2_product_1308958664 = product_1308958664(Seq(vn1_fsrcz_1308958664, vn8_constant256_1308958664))
		val vn29_product = product(Seq(vn23_fnoise, vn37_constant256))
		val vn28_product = product(Seq(vn25_fnoise, vn37_constant256))
		val vn30_product = product(Seq(vn37_constant256, vn24_fnoise))
		val vn6_product_1308958664 = product_1308958664(Seq(vn1_fsrcx_1308958664, vn8_constant256_1308958664))
		val vn4_product_1308958664 = product_1308958664(Seq(vn1_fsrcy_1308958664, vn8_constant256_1308958664))
		val vn3_fnoise_1308958664 = fnoise_1308958664(Seq(Vec3(0)), Seq(vn28_product, vn1_fsrcx_1308958664), Seq(vn1_fsrcy_1308958664, vn29_product), Seq(vn2_product_1308958664), Seq(vn15_constantm1_1308958664), Nil, 0.5f, 0.5f, 0.5f)
		val vn9_fnoise_1308958664 = fnoise_1308958664(Seq(Vec3(0)), Seq(vn6_product_1308958664), Seq(vn1_fsrcy_1308958664, vn29_product), Seq(vn1_fsrcz_1308958664, vn30_product), Seq(vn15_constantm1_1308958664), Nil, 0.5f, 0.5f, 0.5f)
		val vn12_fnoise_1308958664 = fnoise_1308958664(Seq(Vec3(0)), Seq(vn28_product, vn1_fsrcx_1308958664), Seq(vn4_product_1308958664), Seq(vn1_fsrcz_1308958664, vn30_product), Seq(vn15_constantm1_1308958664), Nil, 0.5f, 0.51f, 0.5f)
		val vn16_min = min(Seq(vn12_fnoise_1308958664, vn9_fnoise_1308958664, vn3_fnoise_1308958664))

		vn16_min
	}

	def predictable(originalsource:Vec3) = {
			val source = (originalsource.xzy - cubesize/2 +Vec3(1243,1253,1346)) * Vec3(1,-1,1)

			def fsrcy_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(scale:Float, x:Float, y:Float, z:Float) = {(source.y + (y-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
			def fsrcx_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(scale:Float, x:Float, y:Float, z:Float) = {(source.x + (x-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
			def fsrcv_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(scale:Float, x:Float, y:Float, z:Float) = {(source + (Vec3(x,y,z)-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
			def fsrcz_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(scale:Float, x:Float, y:Float, z:Float) = {(source.z + (z-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
			def custom_f2(a:Float, b:Float, c:Float, d:Float, s1:Float, s2:Float, s3:Float) = {object gradients {
				//util.Random.shuffle(for(i <- 0 until 16) yield { val a = i*2*Pi/16.toFloat; Vec2(cos(a),sin(a)) }).toArray
				val g3d16 = Array(Vec3( +1.000e+00f,+0.000e+00f,+0.000e+00f ),   Vec3( +4.607e-01f,-8.512e-01f,-2.514e-01f ),   Vec3( -8.579e-01f,-2.110e-01f,+4.686e-01f ),   Vec3( -1.817e-01f,-8.059e-01f,+5.635e-01f ),   Vec3( -7.634e-01f,+6.329e-01f,-1.294e-01f ),   Vec3( -1.817e-01f,-3.705e-01f,-9.109e-01f ),   Vec3( -4.443e-01f,+5.611e-01f,+6.984e-01f ),   Vec3( -1.928e-01f,+5.310e-01f,-8.252e-01f ),   Vec3( -4.443e-01f,-8.681e-01f,-2.212e-01f ),   Vec3( -8.923e-01f,-1.287e-01f,-4.326e-01f ),   Vec3( +6.039e-01f,-1.342e-01f,-7.857e-01f ),   Vec3( +6.557e-01f,-5.042e-01f,+5.620e-01f ),   Vec3( +5.900e-01f,+5.313e-01f,+6.080e-01f ),   Vec3( +2.207e-02f,-5.640e-02f,+9.982e-01f ),   Vec3( +6.039e-01f,+6.773e-01f,-4.202e-01f ),   Vec3( +2.207e-02f,+9.967e-01f,+7.794e-02f )	)
			} 

			def noise3(pos:Vec3,seed:Int=0) = {
				def kernel(v:Vec3, gradient:Vec3) = {
					import v.{x,y,z}
					val t = s1-(x*x + y*y + z*z)
					if( t > 0 )
						t*t*t*t*(gradient.x * x + gradient.y * y + gradient.z * z)
					else
						0f
				}
				def hash(k:Int,seed:Int=0) = (((k*0x12345678) >>> (k*0x87754351))^seed) & 0x7FFFFFFF

				def gradient(vertex:Vec3i) = {
					gradients.g3d16( hash(hash(hash(vertex.x,seed) + vertex.y,seed) + vertex.z,seed) & 15)
				}

				def fastfloor(x:Float):Int = if(x > 0) x.toInt else (x-1).toInt

				val vertex1 = Vec3i(fastfloor(pos.x)  , fastfloor(pos.y)  , fastfloor(pos.z))
				val vertex2 = Vec3i(fastfloor(pos.x)+1, fastfloor(pos.y)  , fastfloor(pos.z))
				val vertex3 = Vec3i(fastfloor(pos.x)  , fastfloor(pos.y)+1, fastfloor(pos.z))
				val vertex4 = Vec3i(fastfloor(pos.x)+1, fastfloor(pos.y)+1, fastfloor(pos.z))
				val vertex5 = Vec3i(fastfloor(pos.x)  , fastfloor(pos.y)  , fastfloor(pos.z)+1)
				val vertex6 = Vec3i(fastfloor(pos.x)+1, fastfloor(pos.y)  , fastfloor(pos.z)+1)
				val vertex7 = Vec3i(fastfloor(pos.x)  , fastfloor(pos.y)+1, fastfloor(pos.z)+1)
				val vertex8 = Vec3i(fastfloor(pos.x)+1, fastfloor(pos.y)+1, fastfloor(pos.z)+1)

				(
				kernel(pos-vertex1, gradient(vertex1)) +
				kernel(pos-vertex2, gradient(vertex2)) +
				kernel(pos-vertex3, gradient(vertex3)) +
				kernel(pos-vertex4, gradient(vertex4)) +
				kernel(pos-vertex5, gradient(vertex5)) +
				kernel(pos-vertex6, gradient(vertex6)) +
				kernel(pos-vertex7, gradient(vertex7)) +
				kernel(pos-vertex8, gradient(vertex8)) )*3.1604938271604937f
			}

			noise3(Vec3(a,b,c),(s2*100).toInt)

			}

			val vn1_fsrcz_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872 = fsrcz_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(0.69f, 0.5f, 0.5f, 0.5f)
			val vn1_fsrcy_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872 = fsrcy_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(0.69f, 0.5f, 0.5f, 0.5f)
			val vn1_fsrcx_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872 = fsrcx_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(0.69f, 0.5f, 0.5f, 0.5f)
			val vn1_fsrcv_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872 = fsrcv_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(0.69f, 0.5f, 0.5f, 0.5f)
			val vn2_custom_f2 = custom_f2(vn1_fsrcx_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872, vn1_fsrcy_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872, vn1_fsrcz_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872, 0f, 1.0f, 0.0f, 0.5f)

			vn2_custom_f2
		}

	def hyperdangerous(originalsource:Vec3) = {
		val source = (originalsource.xzy - cubesize) * Vec3(1,-1,1)

		def fsrcz(scale:Float, x:Float, y:Float, z:Float) = {(source.z + (z-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
		def fsrcv(scale:Float, x:Float, y:Float, z:Float) = {(source + (Vec3(x,y,z)-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
		def fnoise_1308761766(v:Seq[Vec3]=Seq(Vec3(0)), x:Seq[Float]=Nil, y:Seq[Float]=Nil, z:Seq[Float]=Nil, plus:Seq[Float]=Nil, minus:Seq[Float]=Nil, size:Float, outscale:Float, outoffset:Float) = {
		val invexpsize = pow(256,((0.5f-size)*2f))
		val expoutscale = pow(256,((outscale-0.5f)*2f))
		val linearoutoffset = (outoffset-0.5f)*2f
		val sumv = v.reduce( (x,y) => x+y ) + Vec3(x.sum,y.sum,z.sum);
		(noise1(sumv*invexpsize)+linearoutoffset)*expoutscale + plus.sum - minus.sum
		}
		def fsrcy(scale:Float, x:Float, y:Float, z:Float) = {(source.y + (y-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
		def fsrcx(scale:Float, x:Float, y:Float, z:Float) = {(source.x + (x-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
		
		val vn5_fsrcz = fsrcz(0.59f, 0.5f, 0.33f, 0.5f)
		val vn5_fsrcy = fsrcy(0.59f, 0.5f, 0.33f, 0.5f)
		val vn5_fsrcx = fsrcx(0.59f, 0.5f, 0.33f, 0.5f)
		val vn5_fsrcv = fsrcv(0.59f, 0.5f, 0.33f, 0.5f)
		val vn4_fnoise_1308761766 = fnoise_1308761766(Seq(vn5_fsrcv), Nil, Nil, Nil, Nil, Nil, 0.73f, 0.87f, 0.5f)
		val vn3_fnoise_1308761766 = fnoise_1308761766(Seq(vn5_fsrcv), Nil, Seq(vn4_fnoise_1308761766), Nil, Seq(vn5_fsrcy), Nil, 0.77f, 0.77f, 0.5f)
		
		vn3_fnoise_1308761766
	}

	def hypermegadangerous(originalsource:Vec3) = {
		val source = (originalsource.xzy - cubesize) * Vec3(1,-1,1)

		def noise3v(v:Vec3=Vec3(0), size:Float, scale:Float, offset:Float) = {(noise1(v*size)+offset)*scale/size}
		def srcxyzz() = {source.z}
		def richnoise3(v:Seq[Vec3], x:Seq[Float], y:Seq[Float], z:Seq[Float], add:Seq[Float], sub:Seq[Float], size:Float, scale:Float, offset:Float) = {
					val sumv = v.fold[Vec3](Vec3(0))( (a,b) => a+b ) + Vec3(x.sum,y.sum,z.sum)
					(noise1(sumv*size)+offset)*scale/size + add.sum - sub.sum
					}
		def srcxyzy() = {source.y}
		def srcv() = {source}
		def srcxyzx() = {source.x}

		val vn6_srcxyzz = srcxyzz()
		val vn6_srcxyzy = srcxyzy()
		val vn6_srcxyzx = srcxyzx()
		val vn6_srcv = srcv()
		val vn11_noise3v = noise3v(vn6_srcv, 0.008490115f, 2.4283893f, 0.0f)
		val vn8_richnoise3 = richnoise3(Seq(vn6_srcv), Nil, Nil, Nil, Nil, Nil, 0.03589682f, 10.267405f, 0.0f)
		val vn7_richnoise3 = richnoise3(Seq(vn6_srcv), Nil, Seq(vn8_richnoise3), Nil, Seq(vn6_srcxyzy, vn11_noise3v), Nil, 0.007598867f, 0.5743491f, 0.0f)

		vn7_richnoise3
	}

	// NoiseSum
	def noiseSum(v:Vec3) = {
		val octaves = 6
		val lacunarity = 1.7f
		val amplitudeDivisor = 1.9f
		val expectedMagnitude = 1.5f

		val frequencyFactors = (for (i <- 0 until octaves) yield pow(lacunarity, i)).toArray
		val amplitudeFactors = (for (i <- 0 until octaves) yield pow(amplitudeDivisor, -i)).toArray

		def noiseSum(p: inVec3) = {
			def octave(i: Int, p: inVec3) = {
				noise1(p*frequencyFactors(i))*amplitudeFactors(i)
			}

			var sum = 0.0f; var i = 0; while (i < octaves) {
				sum += octave(i, p + i)
				i += 1
			}
			sum
		}
		
		noiseSum(v/16)
	}
	
	def surface(v:Vec3) = noiseSum(v) + (cubesize - 2 * v.z)/(cubesize)
	
	def fastgen = {
		def nextFloat = util.Random.nextFloat*2-1
	
		val noiseData = new Array3D[Float](Vec3i(cubesize+1))
		val exactCaseData = new Array3D[Short](Vec3i(cubesize))
		val cube = new Array3D[Hexaeder](Vec3i(cubesize))
		
		var distance = cubesize
		var pos = Vec3i(0)
		var depth = 0
		
		println("ecken");
		for(offset <- Vec3i(0) until Vec3i(2)){
			val coord = pos+offset*cubesize
			noiseData(coord) = 0.5f-offset.z
		}
		
		val unitVectors = Array(Vec3i(1,0,0),Vec3i(0,1,0),Vec3i(0,0,1))

		dostep(cubesize,Vec3i(0),1)
		
		def dostep(distance:Int,pos:Vec3i,depth:Int){
			// interpolieren
			// auf allen Kanten interpolieren
			for( offset <- Vec3i(0) until Vec3i(2) ){
				for( axis <- Seq(0,1,2) ){
					if( offset(axis) == 0 ){
						val a = noiseData(pos + offset * distance)
						val b = noiseData(pos + offset * distance + unitVectors(axis) * distance)
						val result = (a+b)/2 + nextFloat / pow(2,depth)
						val coord  = pos + offset*distance + unitVectors(axis) * distance/2
//						println(coord)
						if(noiseData(coord) == 0)
							noiseData(coord) = result
					}
				}
			}
		
			var sumb = 0f
		
			// auf allen Seitenflächen interpolieren
			for( axisa <- 0 to 2; dira <- Seq(0,1)){
				var sum = 0f
				for(axisb <- Set(0,1,2)-axisa; dirb <- Seq(0,1) ){
					val coorda = Vec3i(1)
					coorda(axisa) = dira*2
					coorda(axisb) = dirb*2
					coorda *= distance/2
					coorda += pos
					sum += noiseData(coorda)
				}
				val coordb = Vec3i(1)
				coordb(axisa) = dira*2
				coordb *= distance/2
				coordb += pos
				val result = sum/4 + nextFloat / pow(2,depth)
//				println(coord)
				if(noiseData(coordb) == 0){
					noiseData(coordb) = result
					sumb += result
				}
				else
					sumb += noiseData(coordb)
				
			}

			noiseData(pos + Vec3i(distance/2)) = sumb/6 + nextFloat / pow(2,depth)
			
			if( distance > 2 ){
				for(offset <- Vec3i(0) until Vec3i(2)){
					dostep(distance/2,pos+offset*(distance/2),depth+1)
				}
			}
		}
		
		noiseData
	}
}

