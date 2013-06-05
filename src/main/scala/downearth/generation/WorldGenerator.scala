package downearth.generation

import simplex3d.math._
import simplex3d.math.double._

import downearth._
import downearth.Config._
import downearth.worldoctree._
import downearth.util._
import downearth.worldoctree.NodeInfo
import downearth.rendering.ObjManager
import downearth.server.message.{GetDeltaInRange, DeltaSet}
import akka.util.Timeout
import downearth.server.LocalServer
import scala.concurrent.Await
import akka.pattern.ask

object WorldGenerator {
	import Config.{worldWindowSize => cubesize}
	
	def genWorld:WorldOctree = {
		// val rootNodeInfo = NodeInfo(Vec3i(-cubesize/2), cubesize)
		val initArea = NodeInfo( Vec3i(-cubesize/4), cubesize/2 )
		val octree = new WorldOctree(initArea,genWorldAt(initArea))
		octree.incDepth()

//    octree( Vec3i(1,2,3) ) = new ObjLeaf(ObjManager.testMesh)

		octree
	}
	
	def genWorldAt(nodeInfo:NodeInfo):NodeOverMesh = {

    // Ask server for World delta asynchronly
    import scala.concurrent.duration._
    implicit val timeout = Timeout(5 seconds)
    val deltaSetFuture = LocalServer.server ? GetDeltaInRange(nodeInfo.pos, nodeInfo.size)

    // while waiting for answer, start to sample node
    val NodeInfo(nodepos, nodesize) = nodeInfo
		import HexaederMC._

		// Predichtion hat kein eindeutiges Ergebnis,
		// Bereich KANN Oberfläche enthalten
		
		// Bereich des Nodes wird vollständig abgetastet
		//Draw addSampledNode toNodeinfo  // Für DebugDraw

		// Braucht eine zusätzliche größe um 2 damit die Nachbarn besser angrenzen können
		// Marching-Cubes für n Cubes: n+1 Datenpunkte
		// Für Umrandungen: n+2 Cubes mit n+3 Datenpunkten
		val originalNoiseData = new Array3D[Double](Vec3i(nodesize+3))
		// Füllen der Datenpunkte mit Hilfe der Dichtefunktion
		originalNoiseData.fill(v =>	WorldDefinition.density(Vec3(nodepos+v-1)))
		val modifiedNoiseData = originalNoiseData.clone
		val exactCaseData = new Array3D[Short](Vec3i(nodesize+2))
	

		// Fall für jeden Cube ermitteln und abspeichern
		for( coord <- Vec3i(0) until Vec3i(nodesize+2) ) {
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

    // wait for world delta from server to overwrite generated blocks
    val deltaSet = Await.result(deltaSetFuture, timeout.duration).asInstanceOf[DeltaSet]

    // Liest die abgespeicherten Fälle aus und erzeugt entsprechende Hexaeder
    // Berücksichtigt auch gespeicherte User-Ändarungen
		def fillfun(v:Vec3i) = {
			val arraypos = v + 1 - nodepos
			val h = data2hexaeder( modifiedNoiseData.extract(arraypos), exactCaseData(arraypos) )

      if(deltaSet.delta.isDefinedAt(v)) { //TODO: Faster with only one lookup?
        deltaSet.delta(v).shape
      } else
        if( h.noVolume )
          EmptyHexaeder
        else
          h
		}

    // Octree mit Hexaedern füllen
    // TODO use prediction here
    val root = EmptyLeaf.fill( nodeInfo, pos => Leaf(fillfun(pos)) )

    root.genMesh( nodeInfo, minMeshNodeSize, (x => {if( nodeInfo.indexInRange(x) ) root(nodeInfo,x).h else fillfun(x) }) )
	}
}

