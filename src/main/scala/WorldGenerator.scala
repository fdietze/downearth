package xöpäx

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import simplex3d.noise._

import Util._
import Config._

import WorldNodeGenerator.generateFutureNodeAt

final class FloatNoise(source: NoiseSource) {
	def apply(x: Double) :Float = source(x).toFloat
	def apply(u: inVec2) :Float = source(u.x, u.y).toFloat
	def apply(u: inVec3) :Float = source(u.x, u.y, u.z).toFloat
	def apply(u: inVec4) :Float = source(u.x, u.y, u.z, u.w).toFloat
}

object WorldGenerator {
	val noise1 = new FloatNoise(ClassicalGradientNoise)
	val cubesize = Config.worldWindowSize
	
	def genWorld:WorldOctree = {
		val octree = new WorldOctree(cubesize,Vec3i(-cubesize/2))
		octree.root = DeadInnderNode
		try{
			val start = Vec3i(-cubesize/2)
			val size = minMeshNodeSize
			for(vi <- Vec3i(0) until Vec3i(worldWindowSize/minMeshNodeSize)){
				val pos = start+vi*minMeshNodeSize

				println( pos,size)
				octree.insert(pos, size, generateFutureNodeAt(pos,size) )
			}
		}
		catch{
			case e =>
				println(octree)
				throw e
		}
		octree.meshGenerated = true
		
		octree
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
		
		val octree = new WorldOctree( nodesize, nodepos.clone )
		
		val interval = time("prediction: "){
			prediction(Vec3(nodepos),Vec3(nodepos+nodesize))
		}
		
		if(interval.isPositive){
			println("yay full")
			octree.root = new Leaf(FullHexaeder)
		}
		else if(interval.isNegative){
			println("yay Empty")
			octree.root = new Leaf(EmptyHexaeder)
		}
		
		/* if(false){} */
		else{
			val noiseData = new Array3D[Float](Vec3i(nodesize+3))
			//braucht eine zusätzliche größe um 2 damit die Nachbarn besser angrenzen können
			val exactCaseData = new Array3D[Short](Vec3i(nodesize+2))
		
			def extractData(pos:Vec3i) = {
				assert(exactCaseData.indexInRange(pos))
				offset map (o => noiseData(pos+o))
			}

			noiseData.fill(v =>	densityfunction(nodepos+v-1) )
		
			val casecounter = new Array[Int](22)

			
			for( coord <- Vec3i(0) until Vec3i(nodesize+2) ){
				val exactCase = dataToCase(extractData(coord))
				exactCaseData(coord) = exactCase.toShort
			}
		
			
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

			def fillfun(v:Vec3i) = {
				val arraypos = v + 1 - nodepos
				val h = data2hexaeder(extractData(arraypos), exactCaseData(arraypos))
				if( h.noVolume )
					EmptyHexaeder
				else h
			}
			
			octree.fill( fillfun _ )
			
			assert(octree.rootNodePos == nodepos)
			assert(octree.rootNodeSize == nodesize)
		}
		
		octree
	}

	def sphere(v:Vec3) = { val vt = v - 8; -(vt.x*vt.x + vt.y*vt.y + vt.z*vt.z - 64f) }
	def smooth(v:Vec3) = noise1(v*0.1f )
	def plane(v:Vec3) = {0.9f*v.x + 0.98f*v.y + 1.01f*v.z -15.3f}

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
	

}

