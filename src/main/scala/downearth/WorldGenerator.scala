package downearth

import simplex3d.math._
import simplex3d.math.double._


import Util._
import Config._


object WorldGenerator {
	import Config.{worldWindowSize => cubesize}
	
	def genWorld:WorldOctree = {
		val octree = new WorldOctree(cubesize,Vec3i(-cubesize/2))
		octree.generateStartArea
		octree
	}
	
	def genWorldAt(nodeinfo:NodeInfo):WorldOctree = {
		val NodeInfo(nodepos, nodesize) = nodeinfo
		import HexaederMC._
		
		val octree = new WorldOctree( nodesize, nodepos.clone )
		
		// Predichtion hat kein eindeutiges Ergebnis,
		// Bereich KANN Oberfläche enthalten
		
		// Bereich des Nodes wird vollständig abgetastet
		//Draw addSampledNode nodeinfo  // Für DebugDraw

		// Braucht eine zusätzliche größe um 2 damit die Nachbarn besser angrenzen können
		// Marching-Cubes für n Cubes: n+1 Datenpunkte
		// Für Umrandungen: n+2 Cubes mit n+3 Datenpunkten
		val originalNoiseData = new Array3D[Double](Vec3i(nodesize+3))
		// Füllen der Datenpunkte mit Hilfe der Dichtefunktion
		originalNoiseData.fill(v =>	densityfunction(Vec3(nodepos+v-1)))
		val modifiedNoiseData = originalNoiseData.clone
		val exactCaseData = new Array3D[Short](Vec3i(nodesize+2))
	

		// Fall für jeden Cube ermitteln und abspeichern
		for( coord <- Vec3i(0) until Vec3i(nodesize+2) ){
			val exactCase = dataToCase(originalNoiseData.extract(coord))
			exactCaseData(coord) = exactCase.toShort
		}
	
		// für jeden Cube:
		for( coord <- Vec3i(0) until Vec3i(nodesize+2) ) {
			// Datenpunkte extrahieren
			val originalData = originalNoiseData.extract(coord)
			val modifiedData = modifiedNoiseData.extract(coord)
			// Fall für diesen Cube auslesen und benennen

			val exactCase = exactCaseData(coord)
			val caseType = caseTypeLookup(exactCase)
	
			// Wenn Fall nicht darstellbar
			if( !isStableCase(caseType) ) {
				// In einen darstellbaren Fall transformieren
				val (newData, newCase) = transformToStable(originalData, exactCase)
				
				// Stabilisierung auf die schon modifizierten Datan anwenden
				val merge = 
					for( i <- 0 until 8 ) yield {
						if( newData(i) == 0 )
							0
						else
							modifiedData(i)
					}
				
				// Transformierten Cube abspeichern
				modifiedNoiseData(coord) = merge
				exactCaseData(coord) = newCase.toShort
			}
		}

		// Liest die abgespeicherten Fälle aus und erzeugt entsprechende Hexaeder
		def fillfun(v:Vec3i) = {
			val arraypos = v + 1 - nodepos
			val h = data2hexaeder( modifiedNoiseData.extract(arraypos), exactCaseData(arraypos) )

			if( h.noVolume )
				EmptyHexaeder
			else 
				h
		}
		
		// Octree mit Hexaedern füllen
		octree.fill( v => Leaf(fillfun(v)) )
		octree.genMesh( fillfun _ )
		assert(octree.rootNodePos == nodepos)
		assert(octree.rootNodeSize == nodesize)

		octree
	}
}

