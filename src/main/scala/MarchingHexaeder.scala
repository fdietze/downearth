package xöpäx

import simplex3d.data._
import simplex3d.data.float._
import simplex3d.math.Vec3i
import simplex3d.math.float._
import simplex3d.math.float.functions._

case class NotHandledYetException(message:String) extends Exception

object CaseType extends Enumeration {
	type CaseType = Value
	val allPositive,oneNegative,threePositivesConnected,fourPositivesTwoTimesTwoConnected,
	twoPositivesOverTwoEdges,fourPositivesUnconnected,twoNegativesOverTwoEdges,
	twoNegativesOverThreeEdges,diagonalHalf,fourPositivesConnectedOnPlane,
	threeNegativesUnconnected,fourPositivesOneUnconnected,threePositivesUnconnected,
	twoNegativesConnected,threeNegativesConnected,allNegative,onePositive,
	twoPositivesOverThreeEdges,fourPositivesInLine,threeNegativesOneUnconnected,
	twoPositivesConnected,threePositivesOneUnconnected,broken = Value
}

import CaseType._

object MarchingHexaeder{
	import Util._
	def sameEdge(a:Int,b:Int) = isPowerOfTwo(a ^ b)
	def over2edges(a:Int,b:Int) = isPowerOfTwo((~(a^b)) & 7)
	def extractPositiveEdges(x:Int) = for(i <- 0 until 8 if(((1 << i) & x) == (1 << i))) yield i
	def extractNegativeEdges(x:Int) = extractPositiveEdges(~x&255)
	def getNeighbours(x:Int) = Set(x^1, x^2, x^4)
	def getNeighboursOverTwoEdges(x:Int) = Set(x^3,x^5,x^6)
	def getPlaneNeighbours(x:Int,y:Int) = {val axis = x ^ y; (Set(Set(x^1,y^1),Set(x^2,y^2),Set(x^4,y^4)) - Set(x,y)).toIndexedSeq} //Input: zwei benachbarte, output: die umliegenden
	def getOppositesOnPlane(a:Int,b:Int) = { //Input zwei nicht benachbarte auf selbem Plane
		val axis = ~(a^b)&7
		val dir=a&axis
		val vertices = Set(0 to 7:_*)
		((
			if( dir > 0 )
				vertices.map( _|axis )
			else
				vertices.map( _&(7-axis) )
		)
		 - a - b).toIndexedSeq
	}
	
	def interpolate(a:Float,b:Float,flip:Boolean):Float = {
		if( a == b )
			if( flip )
				0f
			else
				1f
		else{
			val l = a/(a-b)
			if( flip )
				1f-l
			else l
		}
	}

	val offset = for(i <- 0 until 8) yield Vec3i(i&1,(i&2)>>1,(i&4)>>2)
	def dataToCase(data: IndexedSeq[Float]) = {
		var result = 0
		for( i <- 0 until data.size){
			result |= (if(data(i) > 0) (1 << i) else 0)
		}
		result
	}
	
	val stableCases = Set(allPositive,allNegative,onePositive,twoPositivesConnected,fourPositivesConnectedOnPlane,broken)
	def isStableCase(caseType:CaseType) = stableCases contains caseType

	// TODO scheiß name der Funktion uns scheiß Variablenamen
	def transformToStable(data:IndexedSeq[Float],oldCase:Int):(IndexedSeq[Float],Int) = {
		// Wenn instabiler Fall: Werte auf 0 setzen (Dichtefunktion verzerren), je nach Fall um auf stabilen fall zu kommen
		
		// dies sind die Ausgangsdaten der Dichtefunktion, die partiell auf null gesetzt werden, um sie mit Hexaedern besser zu approximieren.
		val newData = scala.collection.mutable.IndexedSeq(data:_*)
		
		// Falsch: dataToCase darf nicht hier bestimmt werden, da data schon mit manipulierten eingabedaten befüllt wird.
		// bestimmt welche Vertices als Positiv zu interpretieren sind, und welche als negativ interpretiert werden sollen.
		//var newCase = dataToCase(data)
		var newCase = oldCase
		// hier ist der Typ des Falls gespeichert.
		val caseType = caseTypeLookup(newCase)

		// setzt genau einen rasterpunkt aus den AusgangsDaten auf 0
		def setZero(poslist:Int*){
			for(pos <- poslist){
				newData(pos) = 0
				// Wenn Bit in Casetype positiv war auf negativ setzen, sonst auf positiv, also flippen, d.h. XOR mit 1 << pos
				newCase ^= 1 << pos
			}
		}

		def setZeroPreserveSign(poslist:Int*){
			for(pos <- poslist){
				newData(pos) = 0
			}
		}

		def error(poslist:Seq[Int]) = poslist.map(data(_).abs).sum
		
		// TODO zeroNegativeZero behandeln
		// es ist nur eine Ecke eingedrückt, aber man muss wissen welche 
		// Ecke in welche Richtung eingedrückt ist.
		// zudem ist ein oneNegative nicht automatisch ein StableCase, 
		// denn zwei seiner Nachbarn müssen 0 Sein damit die Polygone 
		// zusammen passen. Darum würde ich vorschlagen diesem Fall 
		// einen seperaten Namen zu geben: zeroPositiveZero
		// beim auf Null Setzten muss auch noch aufgepasst werden, 
		// denn in diesem Fall darf nicht immer das newCase bit 
		// geflippt werden
		
		if( !isStableCase(caseType) ){
			caseType match{
				case `oneNegative` => 
					
					// alle Nachbarn der Negativen Ecke ermitteln
					// das paar der Nachbarn mit der geringsten Gesamtsumme ermitteln
					// ist die Summe kleiner als der Betrag der negativen Ecke
					//    die beiden Nachbarn auf Null setzten
					// sonst
					//    die negative Ecke auf Null setzen
					// Achtung: in diesem Fall funktioniert das Flippen des bits im newCase nicht mehr wie vorher
					
					val Seq(n) = extractNegativeEdges(newCase)
					val Seq(a,b,c) = getNeighbours(n).toSeq
					
					setZero(Seq( Seq(a,b), Seq(a,c), Seq(b,c), Seq(n) ).minBy( error _ ):_*)
					
				case `threePositivesConnected` =>
					// Fehlende Plane-Plane-Ecke auf null setzen => fourPositivesOnPlaneConnected
					// oder: Äußere Eckpunkte rausfinden, kleinsten auf Null setzen => twoPositivesConnected
					val Seq(a,b,c) = extractPositiveEdges(newCase)
					val leftplaneedge = a^b^c
					val (outa,outb) =
						if( sameEdge(a,b) )
						{
							if( sameEdge(a,c) )
								(b,c)
							else
								(a,c)
						}
						else
							(a,b)
						
					// minimum auf null setzen	
					setZero(Seq( Seq(outa),Seq(outb),Seq(leftplaneedge) ).minBy( error _ ):_*)
					
					
				case `twoNegativesConnected` =>
					//     +-------e
					//    /|      /|
					//   / |     / |
					//  c--+----a  |
					//  |  +----+--f     
					//  | /     | /
					//  |/      |/
					//  d-------b					
					val Seq(a,b) = extractNegativeEdges(newCase)
					
					val otherAxis = Set(1,2,4) - (a^b)
					val a0 = otherAxis.head
					val a1 = otherAxis.tail.head
					
					val c:Int = a ^ a0
					val d:Int = b ^ a0
					val e:Int = a ^ a1
					val f:Int = b ^ a1
					
					val toPlane = Seq( Seq(c,d),Seq(a,b),Seq(e,f) ).minBy( error _ )
					val toZeroNegZero = Seq( Seq(a,d),Seq(b,c),Seq(a,f),Seq(b,e) ).minBy( error _ )
					
					if( error(toPlane) < error(toZeroNegZero) )
						setZero(toPlane:_*)
					else{
						setZeroPreserveSign(toZeroNegZero(0))
						setZero(toZeroNegZero(1))
					}
				
				case `diagonalHalf` =>
					// Um Mittelere Ecke liegende Ecken ermitteln
					// Betrag einer Ecke + ihrer gegenüberliegenden => kleinstes Paar finden und auf Null setzen
					// => fourConnectedPositivesOnPlane

					val Seq(a,b,c,d) = extractPositiveEdges(newCase)
					val  sides = if( sameEdge(a, b) )
						if( sameEdge(a, c) )
							Seq(b,c,d)
						else
							Seq(a,c,d)
					else
						if( sameEdge(a,c) )
							Seq(a,b,d)
						else
							Seq(a,b,c)
					
					val negatives = sides map (7 - _)
					//TODO: setLeastErrorToZero
					val z = ((sides map (x => Seq(x,7-x))) ++ Seq(sides,negatives)).minBy( error _ )
					
					if(z == negatives){
						setZeroPreserveSign(negatives.tail:_*)
						setZero(negatives.head)
					}
					else{
						setZero(z:_*)
					}
					
						
					
				case `threeNegativesConnected` =>
					
					val negatives = extractNegativeEdges(newCase)
					val p0 = negatives.reduceLeft( _ ^ _ )
					
					
					val Seq(n0,n2) = negatives.filter( p => (negatives map (q => sameEdge(p,q)) ) reduceLeft (_^_) )
					val n1 = n0 ^ p0 ^ n2
					
					// n0--n1
					// |    |
					// p0--n2
					
					if( error(Seq(p0)) < error(Seq(n0,n2)) )
						//wird zu Plane, indem der vierte in dieser negativen Ebene Auf Null gesetzt wird
						setZero(p0)
					else
						//wird zu ZeroNegativeZero, indem die beiden ausen liegenden Vertices auf Null gesetzt werden
						setZeroPreserveSign(n0,n2)
					
				case `twoPositivesOverThreeEdges` =>
					// => onePositive, indem man die betragskleinste Ecke auf Null setzt
					val Seq(a,b) = extractPositiveEdges(newCase)
					if( data(a) < data(b) )
						setZero(a)
					else
						setZero(b)

				case `threeNegativesUnconnected` =>
					
					// TODO: oder: einen auf Null setzen und die Ecken zwischen den beiden anderen Negativen auf null setzten => Plane
					setZero( extractNegativeEdges(newCase):_* )
				
				case `twoNegativesOverTwoEdges` =>
					// Beide auf Null setzen => allPositive
					// oder: Beide dazwischen auf Null setzen => Plane
					val Seq(a,b) = extractNegativeEdges(newCase)
					//TODO: hier fliegt exception, dass a und b nicht auf einem plane liegen
					val Seq(c,d) = getOppositesOnPlane(a,b)
					if( error(Seq(c,d)) > error(Seq(a,b)) )
						setZero(a,b)
					else
						setZero(c,d)

				case `twoPositivesOverTwoEdges` =>
					// einen auf Null setzen => onePositive
					// oder: Beide dazwischen auf Null setzen => Plane
					val Seq(a,b) = extractPositiveEdges(newCase)
					val Seq(c,d) = getOppositesOnPlane(a,b)
					val lightest = if( data(a) > data(b) ) b else a
					if( error(Seq(lightest)) > error(Seq(c,d)) )
						setZero(c,d)
					else
						setZero(lightest)
				
				case `fourPositivesInLine` =>
					val positive = extractPositiveEdges(newCase)
					
					// jeweils mit nur einer ecke verbunden
					val Seq(p0,p3) = positive.filter( p => (positive map (q => sameEdge(p,q)) ) reduceLeft (_^_) )
					// mit dem Anfang verbunden
					val p1 = positive.find( p => sameEdge(p0,p) ).get
					// mit dem Ende verbunden
					val p2 = positive.find( p => sameEdge(p3,p) ).get
					
					// p0,p1,p2,p3 ist jetzt die Linie
					//     n1-----p3
					//    /|      /|
					//   / |     / |
					//  *--+----*  |
					//  |  p1---+-p2     
					//  | /     | /
					//  |/      |/
					//  p0-----n0
					
					// die negativen vertices
					val n0 = p0^p1^p2
					val n1 = p1^p2^p3
					
					//das Paar das auf Null gesetzt werden soll
					val Seq(sz0,sz1) = Vector( Seq(p0,n1),Seq(n0,p3),Seq(p0,p1),Seq(p2,p3) ).minBy( error _ )
					
					setZero(sz0,sz1)
				case `threeNegativesOneUnconnected` =>
					val Seq(a,b,c) = extractNegativeEdges(newCase)
					
					// n0 ist mit allen unverbunden
					val (n0,d,e) = 
					if( sameEdge(a,b) )
						(c,b,a)
					else if( sameEdge(b,c) )
						(a,b,c)
					else
						(b,a,c)
					
					val (n1,n2) =
					if(over2edges(n0,d))
						(d,e)
					else
						(e,d)
					
					val (a0,a1) = (n1^n2) match {
						case 1 => (2,4)
						case 2 => (1,4)
						case 4 => (1,2)
					}
					
					val p0 = n1 ^ a0
					val p1 = n2 ^ a0
					val p2 = n1 ^ a1
					val p3 = n2 ^ a1
					
					// n1,n2 sind verbunden n0 liegt ausserhalb
					//     n0------*
					//    /|      /|
					//   / |     / |
					//  p0-+---p1  |
					//  |  p2---+-p3     
					//  | /     | /
					//  |/      |/
					//  n1-----n2
					
					setZero(Seq(Seq(n0,p0,p1),Seq(n0,p2,p3),Seq(n0,n1,n2),Seq(n2,p0,p2)).minBy( error _ ):_*)
				case `threePositivesOneUnconnected` =>
					val Seq(a,b,c) = extractPositiveEdges(newCase)
					
					val (p0,d,e) =
					if( sameEdge(a,b) )
						(c,b,a)
					else if( sameEdge(b,c) )
						(a,b,c)
					else
						(b,a,c)
					
					val (p1,p2) =
					if(over2edges(p0,d))
						(d,e)
					else
						(e,d)
					
					val (a0,a1) = (p1^p2) match {
						case 1 => (2,4)
						case 2 => (1,4)
						case 4 => (1,2)
					}
					
					val n0 = p1 ^ a0
					val n1 = p2 ^ a0
					val n2 = p1 ^ a1
					val n3 = p2 ^ a1
					
					setZero(Seq(Seq(p0),Seq(p1,p2),Seq(n0,n2,p2),Seq(p0,n0,n1),Seq(p0,n2,n3)).minBy( error _ ):_*)
					
				case `fourPositivesOneUnconnected` =>
					val positives = extractPositiveEdges(newCase)
					// die Achse, um die der unverbundene positive aus ebene herausragt.
					val axis = positives reduceLeft (_ ^ _)
					assert( axis == 1 || axis == 2 || axis == 4)
					// die beiden unverbundenen Gruppen
					val (g1,g2) = positives partition (v => (axis & v) == axis)
					
					assert((g1.size == 1 && g2.size == 3) || (g1.size == 3 && g2.size == 1))
					
					
					val single = 
					if (g1.size == 1)
						g1.head
					else
						g2.head
					
					setZero(single,single^axis)
				// Note: Diese Fälle sind alle komplett ungetestet
				case `threePositivesUnconnected` =>
					val Seq(a,b,c) = extractPositiveEdges(newCase)
					setZero(Seq(Seq(a,b),Seq(a,c),Seq(b,c)).minBy( error _ ):_*)
				case `fourPositivesUnconnected` =>
					val Seq(a,b,c,d) = extractPositiveEdges(newCase)
					val negatives = extractNegativeEdges(newCase)
					setZero(Seq(Seq(a,b,c),Seq(a,b,d),Seq(a,c,d),Seq(b,c,d),negatives).minBy( error _ ):_*)
				case `fourPositivesTwoTimesTwoConnected` =>
					val Seq(a,b,c,d) = extractPositiveEdges(newCase)
					val groups =
					if(sameEdge(a,b))
						Seq(Seq(a,b),Seq(c,d))
					else if(sameEdge(a,c))
						Seq(Seq(a,c),Seq(b,d))
					else
						Seq(Seq(a,d),Seq(b,c))
					
					setZero(groups.minBy( error _ ):_*)
					
				case `twoNegativesOverThreeEdges` =>
					setZero(extractNegativeEdges(newCase):_*)
				case left =>
					println("not handled: " + left)
			}
		}
		
		(newData, newCase)
	}
	

	def data2hexaeder(data: IndexedSeq[Float], exactCase:Int): Hexaeder = {
		caseTypeLookup(exactCase) match{
			case `allNegative` =>
				EmptyHexaeder

			case `allPositive` =>
				FullHexaeder
				
			case `onePositive` =>
				val h = new PartialHexaeder
				val p0 = log2(exactCase)
				
				val neighbours = Seq(p0^1,p0^2,p0^4)
				
				// An die positive Ecke angrenzende Ecken
				for( v <- neighbours ){ // getNeighbours gibt eine Liste der angrenzenden ecken zurück
						// jeder vertex wird in richtung der positiven Ecke interpoliert
						h(v,log2(v ^ p0)) = interpolate(data(p0),data(v),v - p0 < 0)
				}
				
				val Seq(a1,a2,a3) = ( neighbours sortBy data ) map ( _ ^ p0 )
				
				// An die positive Ecke über 2 Kanten angrenzenden Ecken auf eine direkt angrenzende setzen
				h(p0 ^ a1 ^ a2) = h(p0 ^ a2)
				h(p0 ^ a1 ^ a3) = h(p0 ^ a3)
				h(p0 ^ a2 ^ a3) = h(p0 ^ a3)
				h(7-p0) = h(p0 ^ a3)
				
				h
				

			case `twoPositivesConnected` =>
				val h = new PartialHexaeder
				val Seq(p0, p1) = extractPositiveEdges(exactCase)
				
				
				val axis = p0 ^ p1
				val (axisa,axisb) = axis match {
					case 1 => (2,4)
					case 2 => (1,4)
					case 4 => (1,2)
				}
				
				val a2 =  if( data(p0^axisa) + data(p1^axisa) < data(p0^axisb) + data(p1^axisb) )
						axisb
					else
						axisa
				
				//direkt anliegende nachbarn
				h(p0^axisa,log2(axisa)) = interpolate(data(p0),data(p0^axisa), (p0 & axisa) != 0 )
				h(p1^axisa,log2(axisa)) = interpolate(data(p1),data(p1^axisa), (p1 & axisa) != 0 )
				h(p0^axisb,log2(axisb)) = interpolate(data(p0),data(p0^axisb), (p0 & axisb) != 0 )
				h(p1^axisb,log2(axisb)) = interpolate(data(p1),data(p1^axisb), (p1 & axisb) != 0 )
			
				// gegenüberliegenden Vertices
				h(7-p1) = h(p0 ^ a2)
				h(7-p0) = h(p1 ^ a2)

				h

			case `fourPositivesConnectedOnPlane` =>
				val h = new PartialHexaeder
				
				val negatives = extractNegativeEdges(exactCase)
				// get axis
				var axis = 0
				var direction = false
				for( a <- 0 until 3 )
				{
					val ashift = (1 << a)
					if( negatives.map( _ & ashift ).toSet.size == 1 )
					{
						axis = a
						direction = (negatives(0) & ashift) > 0
					}
				}
				
				for( n <- negatives )
				{
					h(n,axis) = interpolate(data(n),data(n^(1<<axis)), direction)
				}
				
				h
				
			// es ist nicht wirklich dieser Fall. Wird als zeroPositiveZero behandelt.
			case `threeNegativesConnected` =>
				
				
				/* Sicht von oben:
				n0--n1
				|    |
				++--n2
				
				n0 und n2 sind 0, und nur der vertex bei n1 wird eingedrückt
				*/
				
				val Seq(a,b,c) = extractNegativeEdges(exactCase)
				val axis = 7 - ((a & b & c) ^ (a | b | c))
				val n1 = (7 - axis) ^ (a^b^c)
				val h = new PartialHexaeder
				
				//Hexaeder an n1 eindrücken
				//TODO: Axen immer mit 1,2,4 rechnen
				h(n1,log2(axis)) = interpolate(data(n1),data(n1^axis),n1 - (n1^axis) >= 0)
				
				h
			case x =>
				throw NotHandledYetException("not handled yet:\ndata: %s\nexactCase: %s".format(data,exactCase.toBinaryString))
		}
	}
	
	val cases = new collection.mutable.HashMap[CaseType,Set[Int]]
	
	cases( allNegative ) = Set(0x00)
	cases( onePositive ) = Set(0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80);
	cases( twoPositivesConnected ) = Set(0x03, 0x0C, 0x30, 0xC0,  0x05, 0x0A, 0x50, 0xA0,  0x11, 0x22, 0x44, 0x88);
	cases( fourPositivesConnectedOnPlane ) = Set(0x0F, 0xF0, 0x33, 0xCC, 0x55, 0xAA);
	cases( twoPositivesOverTwoEdges ) = Set(0x09, 0x21, 0x41, 0x06, 0x12, 0x82, 0x14, 0x84, 0x28, 0x48, 0x90, 0x60);
	cases( twoPositivesOverThreeEdges ) = Set(0x81, 0x42, 0x24, 0x18);
	cases( threePositivesUnconnected ) = Set(0x29, 0x49, 0x61, 0x16, 0x86, 0x92, 0x94, 0x68);
	cases( fourPositivesUnconnected ) = Set(0x69, 0x96);
	cases( threePositivesConnected ) = Set(0xC4, 0x70, 0xC8, 0xD0, 0x2A, 0x15, 0x4C, 0x8C, 0x0D, 0x51, 0xA8, 0x07, 0x54, 0xE0, 0xB0, 0x0E, 0x0B, 0x8A, 0x13, 0x31, 0x23, 0xA2, 0x45, 0x32);
	cases( diagonalHalf ) = Set(0x17, 0xD4, 0xB2, 0x2B, 0x71, 0xE8, 0x4D, 0x8E);
	cases( threePositivesOneUnconnected ) = Set(0x52, 0x58, 0x62, 0xA1, 0x26, 0x98, 0xA4, 0x25, 0x1A, 0xC1, 0x89, 0x1C, 0x19, 0x91, 0x2C, 0x83, 0x64, 0x46, 0x38, 0x4A, 0xC2, 0x34, 0x43, 0x85);
	cases( fourPositivesInLine ) = Set(0xE2, 0xB1, 0x2E, 0xCA, 0x1D, 0xD8, 0x47, 0xD1, 0xE4, 0x5C, 0x8B, 0xB8, 0xA3, 0x35, 0x72, 0x4E, 0x1B, 0x27, 0x53, 0xC5, 0xAC, 0x3A, 0x74, 0x8D);
	cases( fourPositivesOneUnconnected ) = Set(0xA9, 0x1E, 0x39, 0x95, 0xE1, 0x59, 0x87, 0x36, 0x65, 0x2D, 0x56, 0x6A, 0x78, 0xD2, 0x9C, 0x4B, 0xC6, 0xB4, 0x6C, 0x93, 0xA6, 0x63, 0xC9, 0x9A);
	cases( threeNegativesConnected ) = Set(0x4F, 0x73, 0xD5, 0xEC, 0xCE, 0xAB, 0x3B, 0x37, 0xCD, 0x8F, 0x5D, 0xF2, 0xAE, 0xEA, 0xDC, 0x75, 0xF8, 0xF1, 0xBA, 0xF4, 0x57, 0xB3, 0x2F, 0x1F);
	cases( fourPositivesTwoTimesTwoConnected ) = Set(0x99, 0xC3, 0x66, 0xA5, 0x5A, 0x3C);
	cases( threeNegativesOneUnconnected ) = Set(0x5E, 0x6E, 0xDA, 0xBC, 0x9D, 0xE5, 0xC7, 0x76, 0xB5, 0xE6, 0x3E, 0xB9, 0x67, 0xA7, 0xAD, 0xCB, 0x7A, 0x3D, 0x5B, 0x7C, 0x9B, 0xE3, 0xD9, 0xD3);
	cases( twoNegativesConnected ) = Set(0xF3, 0xAF, 0x3F, 0x77, 0xFC, 0xF5, 0xFA, 0xEE, 0x5F, 0xCF, 0xDD, 0xBB);
	cases( threeNegativesUnconnected ) = Set(158, 182, 121, 107, 233, 151, 214, 109);
	cases( twoNegativesOverTwoEdges ) = Set(0xF9, 0xF6, 0xDE, 0xEB, 0x7B, 0xD7, 0x7D, 0xED, 0xBE, 0xB7, 0x6F, 0x9F);
	cases( twoNegativesOverThreeEdges ) = Set(0x7E, 0xBD, 0xDB, 0xE7);
	cases( oneNegative ) = Set(0xDF, 0x7F, 0xEF, 0xFB, 0xF7, 0xBF, 0xFE, 0xFD);
	cases( allPositive ) = Set(0xFF);
	
	val caseTypeLookup = {
		for(i <- 0 until 256) yield {
			var id = broken
			for((name,c) <- cases){
				if(c contains i)
				id = name
			}
			id
		}
	}
}


