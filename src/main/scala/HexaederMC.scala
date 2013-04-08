package openworld
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.math._
import simplex3d.math.double._

import Util._

// Alle Marching-Cubes-Fälle
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

object HexaederMC {
	
	// testet ob zwei Vertices auf der selben Kante liegen
	def sameEdge(a:Int, b:Int) = isPowerOfTwo(a ^ b)
	
	// testet ob zwei Vertices zwei Kantenlängen voneinander entfernt sind
	def over2edges(a:Int, b:Int) = isPowerOfTwo((~(a^b)) & 7)
	
	// extrahiert aus den binär kodierten Vorzeichen aller Vertices ihre Indizes
	// gibt also die Indizes aller gesetzten Bits zurück
	def extractPositiveEdges(x:Int) = for(i <- (0 until 8) if(((1 << i) & x) == (1 << i))) yield i
	
	// Komplement zu extractPositiveEdges
	def extractNegativeEdges(x:Int) = extractPositiveEdges(~x & 0xFF)
	
	// gibt die drei Nachbarn eines Vertex zurück
	def getNeighbours(x:Int) = (x^1, x^2, x^4)
	
	
	// Findet für eine Achse die anderen zwei Achsen (1,2,4) heraus:
	// 1 ->   2,4
	// 2 ->   1,4
	// 3 -> (2),(4)
	// 4 ->   1,2
	def otherAxisA(axis:Int) = ((axis) & 1) + 1
	def otherAxisB(axis:Int) = 4 - (((axis)-1) & 2)
	
	//Input: zwei nicht benachbarte auf selbem Plane
	//Output: die beiden Vertex-Paare auf den zwei angrenzenden Seitenflächen
	def getOppositesOnPlane(a:Int, b:Int) = { 
		//     +-------+
		//    /|      /|
		//   / |     / |
		//  a--+----c  |
		//  |  +----+--+
		//  | /     | /
		//  |/      |/
		//  d-------b

		var m1 = (a ^ b) & 1
		var m2 = (a ^ b) & 2
		val m3 = (a ^ b) & 4
		
		if(m3 != 0){
			if(m1 == 0)
				m1 = m3
			else
				m2 = m3
		}
		
		val dir = a & (7 - (a^b))
		val c = dir | (m1 & a) | (m2 & b)
		val d = dir | (m2 & a) | (m1 & b)
		
		(c,d)
	}
	
	// Die Nullstelle zwischen der Gerade, die durch (0,a) und (1,b) verläuft
	// Wenn flip = true, dann durch (1,a) und (0,b)
	def interpolate(a:Float,b:Float,flip:Boolean):Float = {
		if( a == b )
			if( flip )
				0f
			else
				1f
		else {
			val l = a/(a-b)
			if( flip )
				1f-l
			else l
		}
	}
	
	// wandelt die Noise-Daten in einen der 256 MC-Fälle um
	def dataToCase(data: IndexedSeq[Float]) = {
		var result = 0
		for( i <- 0 until data.size){
			result |= (if(data(i) > 0) (1 << i) else 0)
		}
		result
	}
	
	// alle Fälle, die abgebildet werden können vom HexaederMC
	val stableCases = Set(allPositive,allNegative,onePositive,
		twoPositivesConnected,fourPositivesConnectedOnPlane)
	
	def isStableCase(caseType:CaseType) = stableCases contains caseType

	def transformToStable(data:IndexedSeq[Float], oldCase:Int):(IndexedSeq[Float],Int) = {
		// Wenn instabiler Fall: Werte auf 0 setzen (Dichtefunktion verzerren), je nach Fall, um auf stabilen fall zu kommen
		
		// dies sind die Ausgangsdaten der Dichtefunktion, die auf null gesetzt werden können, um sie mit Hexaedern darstellen zu können.
		val newData = scala.collection.mutable.IndexedSeq(data:_*)
		
		var newCase = oldCase
		// hier ist der Typ des Falls gespeichert.
		val caseType = caseTypeLookup(newCase)

		// setzt genau einen rasterpunkt aus den AusgangsDaten auf 0
		def setZero(poslist:Int*) {
			for(pos <- poslist){
				newData(pos) = 0
				// Wenn Bit in CaseType positiv war, auf negativ setzen,
				// sonst auf positiv, also flippen, d.h. XOR mit 1 << pos
				newCase ^= 1 << pos
			}
		}

		def setZeroPreserveSign(poslist:Int*){
			for(pos <- poslist){
				newData(pos) = 0
			}
		}

		// Berechnet die absolute Abweichung von den Originaldaten,
		// wenn alle Vertices auf Null gesetzt werden würden
		val error:(Int => Float) = pos => data(pos).abs
		val errorSum:(Seq[Int] => Float) =  poslist => {
			var sum = 0f
			for( p <- poslist )
				sum += error(p)
			sum
		}
		val errorTupleSum: ((Seq[Int],Seq[Int])) => Float = poslist => {
			errorSum(poslist._1) + errorSum(poslist._2)
		}
		
		if( !isStableCase(caseType) ) {
			caseType match {

				// Bei einigen Fällen macht es Sinn, einen weiteren stabilen Fall
				// zeroNegativeZero einzuführen. Er übernimmt die Kodierung von
				// dem Fall threeNegativesConnected, ist dabei allerdings nur
				// ein Spezialfall, bei dem die beiden äußeren Vertices den Wert
				// null haben. Abgebildet welden kann dieser Fall relativ einfach,
				// indem nur ein einziger Vertex des Hexaeders eingedrückt wird.
				// Wenn man auf ihn abbildet, muss oft darauf geachtet werden,
				// dass die Kodierung als Fall mit drei Negativen Ecken stimmt.
				// Dies bedeutet, dass in einigen Fällen kein Vorzeichenwechsel 
				// stattfindet.

				case `oneNegative` => 
					// Alle Nachbarn der Negativen Ecke ermitteln.
					// Das paar der Nachbarn mit der geringsten Gesamtsumme ermitteln.
					// Ist die Summe kleiner als der Betrag der negativen Ecke:
					//    die beiden Nachbarn auf Null setzen
					// sonst:
					//    die negative Ecke auf Null setzen

					// wenn zwei Nachbarn auf Null gesetzt werden, entsteht der
					// Fall theeNegativesConnected. Da diese Nachbarn auf 0 
					// gesetzt werden kann der Fall allerdings als 
					// ZeroNegativeZero weiterverarbeitet werden.
					
					val Seq(n) = extractNegativeEdges(newCase)
					val (a,b,c) = getNeighbours(n)
					
					setZero(Seq( Seq(a,b), Seq(a,c), Seq(b,c), Seq(n) ).minBy( errorSum ):_*)

					
				case `threePositivesConnected` =>
					// Fehlende Plane-Plane-Ecke auf null setzen => fourPositivesOnPlaneConnected
					// oder: Äußere Eckpunkte rausfinden, kleinsten auf Null setzen => twoPositivesConnected
					val Seq(a,b,c) = extractPositiveEdges(newCase)
					val leftplaneedge = a^b^c
					// Die äußeren Eckpunkte herausfinden:
					val (outa,outb) =
						if( sameEdge(a,b) )
							if( sameEdge(a,c) )
								(b,c)
							else
								(a,c)
						else
							(a,b)
						
					// minimum auf null setzen
					setZero(Seq( Seq(outa),Seq(outb),Seq(leftplaneedge) ).minBy( errorSum ):_*)
					
					
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
					
					val a0 = otherAxisA(a^b)
					val a1 = otherAxisB(a^b)
					
					val c:Int = a ^ a0
					val d:Int = b ^ a0
					val e:Int = a ^ a1
					val f:Int = b ^ a1
					
					// Es wird je nach kleinstem Fehler abgebildet auf:
					// - fourPositivesConnectedOnPlane
					// - allPositive
					// - zeroNegativeZero
					
					// (a,b) -> 0 entspricht allPositive
					val toPlane = Seq( Seq(c,d),Seq(e,f),Seq(a,b) ).minBy( errorSum )
					val toZeroNegZero = Seq( Seq(a,d),Seq(b,c),Seq(a,f),Seq(b,e) ).minBy( errorSum )
					
					if( errorSum(toPlane) < errorSum(toZeroNegZero) )
						setZero(toPlane:_*)
					else{
						// bei ZeroNegativeZero findet hier bei einem Vertex kein
						// Vorzeichenwechsel statt.
						setZeroPreserveSign(toZeroNegZero(0))
						setZero(toZeroNegZero(1))
					}
				

				case `diagonalHalf` =>
					//     +-------X
					//    /|      /|
					//   / |     / |
					//  X--+----X  |
					//  |  +----+--+
					//  | /     | /
					//  |/      |/
					//  +-------X
					// X: {a,b,c,d}

					// Um mittlere Ecke liegende Ecken ermitteln
					// Betrag einer Ecke + ihrer gegenüberliegenden
					// => kleinstes Paar finden und auf Null setzen
					// wird zu fourPositivesConnectedOnPlane
					// oder zeroNegativeZero

					val Seq(a,b,c,d) = extractPositiveEdges(newCase)
					val  sides = if( sameEdge(a, b) )
						if( sameEdge(a, c) )
							Seq(b,c,d) // a ist in der Mitte
						else
							Seq(a,c,d) // b ist in der Mitte
					else
						if( sameEdge(a,c) )
							Seq(a,b,d) // c ist in der Mitte
						else
							Seq(a,b,c) // d ist in der Mitte
					
					// die drei Negativen, an die Positiven angrenzen.
					val negatives = sides map (7 - _)

					val z = (
						(sides map (x => Seq(x,7-x))) // Gegenüberliegende Ecken
						++ Seq(sides,negatives)
					).minBy( errorSum )
					
					// die drei Negativen können auf null gesetzet werden,
					// wodurch der Fall zeroNegativeZero entsteht. Allerdings 
					// darf hier nur bei einem von ihnen das Vorzeichen 
					// gewechselt werden.
					if(z == negatives) {
						setZeroPreserveSign(negatives.tail:_*)
						setZero(negatives.head)
					}
					else {
						setZero(z:_*)
					}
					
						
				case `threeNegativesConnected` =>
					
					//     +-------+
					//    /|      /|
					//   / |     / |
					//  n0-+----n1 |
					//  |  +----+--+
					//  | /     | /
					//  |/      |/
					//  p0------n2
					
					// n1 ist immer benachbart zu n0 und n2
					val Seq(a,b,c) = extractNegativeEdges(newCase)
					val (n0,n1,n2) =
						if( sameEdge(a,b) )
							if( sameEdge(a,c) )
								(b,a,c) // n1 = a
							else
								(a,b,c) // n1 = b
						else
							(a,c,b) // n1 = c

					val p0 = a ^ b ^ c

					if( errorSum(Seq(p0)) < errorSum(Seq(n0,n2)) ) {
						// wird zu fourPositivesConnectedOnPlane, indem der vierte
						// in dieser negativen Ebene auf Null gesetzt wird
						setZero(p0)
					}
					else {
						// wird zu ZeroNegativeZero, indem die beiden außen
						// liegenden Vertices auf Null gesetzt werden
						setZeroPreserveSign(n0,n2)
					}
					
					
				case `twoPositivesOverThreeEdges` =>
					//     +-------b
					//    /|      /|
					//   / |     / |
					//  +--+----+  |
					//  |  +----+--+
					//  | /     | /
					//  |/      |/
					//  a-------+

					// wird zu onePositive, indem man die betragskleinste Ecke auf Null setzt
					val Seq(a,b) = extractPositiveEdges(newCase)
					if( data(a) < data(b) )
						setZero(a)
					else
						setZero(b)


				case `threeNegativesUnconnected` =>
					//     +-------n3
					//    /|      /|
					//   / |     / |
					//  n1-+----+  |
					//  |  +----+--+
					//  | /     | /
					//  |/      |/
					//  +-------n2
					
					val Seq(n1,n2,n3) = extractNegativeEdges(newCase).sortBy( error )
					// Die zwei Ecken mit dem geringsten Fehler auf 0 setzen
					setZero(n1,n2)
					// ab hier wie oneNegative behandeln:
					// Code von oneNegative:
					val (a,b,c) = getNeighbours(n3)
					setZero(Seq(Seq(a,b), Seq(a,c), Seq(b,c), Seq(n3) ).minBy( errorSum ):_*)

					// TODO oder: einen auf Null setzen und die Ecken zwischen den beiden anderen Negativen auf null setzten => Plane


				case `twoNegativesOverTwoEdges` =>
					//     +-------+
					//    /|      /|
					//   / |     / |
					//  n0-+----p0 |
					//  |  +----+--+
					//  | /     | /
					//  |/      |/
					//  p1------n1
					// Beide auf Null setzen => allPositive
					// oder: Beide dazwischen auf Null setzen => Plane
					// oder: drei auf Null setzten => zeroNegativeZero
					
					val Seq(n0,n1) = extractNegativeEdges(newCase)
					val (p0,p1) = getOppositesOnPlane(n0,n1)
					
					setZero(Seq(
						Seq(p0,p1,n0), 	Seq(p0,p1,n1), // zeroNegativeZero
						Seq(n0,n1), Seq(p0,p1) // allPositive, Plane
					).minBy( errorSum ):_*)


				case `twoPositivesOverTwoEdges` =>
					//     +-------+
					//    /|      /|
					//   / |     / |
					//  n0-+----p0 |
					//  |  +----+--+
					//  | /     | /
					//  |/      |/
					//  p1------n1
					
					// einen auf Null setzen => onePositive
					// Beide dazwischen auf Null setzen => Plane
					
					val Seq(p0,p1) = extractPositiveEdges(newCase)
					val (n0,n1) = getOppositesOnPlane(p0,p1)
					
					setZero(Seq(Seq(p0), Seq(p1), Seq(n0,n1)).minBy( errorSum ):_*)


				case `fourPositivesInLine` =>
					// p0,p1,p2,p3 bilden eine Linie
					//     n3-----p3
					//    /|      /#
					//   / |     / #
					//  *--+----*  #
					//  |  p1###+#p2
					//  | #     | /
					//  |#      |/
					//  p0-----n0
					
					
					// Planes: p0,p1,p2,n0; p1,p2,p3,n3
					// twoPositives: p0,p1; p1,p2; p2,p3
					
					
					val positives = extractPositiveEdges(newCase)
					
					//TODO: schneller implementieren ohne lambda
					// jeweils mit nur einer ecke verbunden
					val Seq(p0,p3) = positives.filter( p => (positives map (q => sameEdge(p,q)) ) reduceLeft (_^_) )
					
					// mit dem Anfang verbunden
					val p1 = positives.find( p => sameEdge(p0,p) ).get
					// mit dem Ende verbunden
					val p2 = positives.find( p => sameEdge(p3,p) ).get
					
					// die negativen vertices
					val n0 = p0^p1^p2
					val n3 = p1^p2^p3
					
					//das Paar das auf Null gesetzt werden soll
					setZero( Seq(
						Seq(p0,n3),Seq(n0,p3), // Plane
						Seq(p0,p1),Seq(p2,p3), Seq(p0,p3) // twoPositive
					).minBy( errorSum ):_* )
					

				case `threeNegativesOneUnconnected` =>
					// n1,n2 sind verbunden n0 liegt ausserhalb
					//     n0------p4
					//    /|      /|
					//   / |     / |
					//  p0-+---p1  |
					//  |  p2---+-p3
					//  | /     | /
					//  |/      |/
					//  n1-----n2
					// Vertices in Klammern werden auf Null gesetzt
					// Plane: n1,n2,p2,p3 (n0,p2,p3); n1,n2,p0,p1 (n0,p0,p1); p0,n0,p2,n1 (p0,p2,n2)
					// zeroNegativeZero: ([x] bedeutet "preserve sign")
					//	n1,n2,p1 (n0,[n1],p1); p0,n1,n2 (n0,p0,[n2]); p2,n1,n2 (n0,p2,[n2]); n1,n2,p3 (n0, [n1], p3)
					//  p0,n0,p4 (n1,n2,p0,p4); p0,n0,p2 (n1,n2,p0,p2); p2,n0,p4 (n1,n2,p2,p4)
					// allPositive (n0,n1,n2), allNegative (p0,p1,p2,p3,p4)

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
					
					val a0 = otherAxisA(n1^n2)
					val a1 = otherAxisB(n1^n2)
					
					val p0 = n1 ^ a0
					val p1 = n2 ^ a0
					val p2 = n1 ^ a1
					val p3 = n2 ^ a1
					val p4 = 7 - n1
					
					// in den Tupeln werden zwei Listen von Vertices gespeichert.
					// beide müssen auf null gesetzt werden, aber bei der ersten 
					// Liste muss dabei das Vorzeichen beibehalten werden.
					val cases = 
					Seq(
						// Plane
						(Nil,Seq(n0,p2,p3)), (Nil,Seq(n0,p0,p1)), (Nil, Seq(p0,p2,n2)),
						// zeroNegativeZero
						(Seq(n1),Seq(n0,p1)), (Seq(n2),Seq(n0,p0)), (Seq(n2),Seq(n0,p2)), (Seq(n1),Seq(n0,p3)) ,
						(Nil, Seq(n1,n2,p0,p4)), (Nil, Seq(n1,n2,p0,p2)), (Nil, Seq(n1,n2,p2,p4)),
						// allPositive, allNegative
						(Nil, Seq(n0,n1,n2)), (Nil, Seq(p0,p1,p2,p3,p4))
					)
					
					val leastError = cases.minBy( errorTupleSum )
					
					setZeroPreserveSign(leastError._1:_*)
					setZero(leastError._2:_*)
					

				case `threePositivesOneUnconnected` =>
					//     p0------+
					//    /|      /|
					//   / |     / |
					//  n0-+----n1 |
					//  |  n2---+--n3
					//  | /     | /
					//  |/      |/
					//  p1------p2
					// onePositive: p0 (p1,p2)
					// twoPositive: p1,p2 (p0)
					// Plane: p0,p1,n0,n2 (p2,n0,n2)
					
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
					
					val a0 = otherAxisA(p1^p2)
					val a1 = otherAxisB(p1^p2)
					
					// p1 und p2 in jeweils beide Achsenrichtungen verschoben
					val n0 = p1 ^ a0
					val n1 = p2 ^ a0
					val n2 = p1 ^ a1
					val n3 = p2 ^ a1
					
					setZero(Seq(Seq(p0),Seq(p1,p2),Seq(n0,n2,p2)).minBy( errorSum ):_*)

					
				case `fourPositivesOneUnconnected` =>
					//     p0------n3
					//    /|      /|
					//   / |     / |
					//  n1-+----n2 |
					//  |  n0---+--p3
					//  | /     | /
					//  |/      |/
					//  p1------p2
					// onePositive: p0 (p1,p2,p3)
					// twoPositive: p1,p2 (p3,p0); p2,p3 (p0,p1)
					// Plane: p1,p2,p3,n0 (p0,n0); p0,p1,n0,n1 (n0,n1,p2,p3); p0,p3,n0,n3 (n0,n3,p1,p2)
					
					val positives = extractPositiveEdges(newCase)
					
					// die Achse, um die der unverbundene positive aus ebene herausragt.
					val axis = positives(0) ^ positives(1) ^ positives(2) ^ positives(3)

					// die beiden unverbundenen Gruppen
					val (g1,g2) = positives partition (v => (axis & v) == axis)
					
					val (p0,p1,p2,p3) =
						if(g1.size == 1)
							(g1.head, g2(0), g2(1), g2(2))
						else
							(g2.head, g1(0), g1(1), g1(2))
					
					val n0 = p0 ^ axis
					val n1 = p1 ^ axis
					val n2 = p2 ^ axis
					val n3 = p3 ^ axis

					setZero(Seq(
						Seq(p1,p2,p3), // onePositive
						Seq(p3,p0),Seq(p0,p1), // twoPositive
						Seq(p0,n0), Seq(n0,n1,p2,p3), Seq(n0,n3,p1,p2) // Plane
					).minBy( errorSum ):_*)
					
					
				case `threePositivesUnconnected` =>
					//     c-------+
					//    /|      /|
					//   / |     / |
					//  +--+----b  |
					//  |  +----+--+
					//  | /     | /
					//  |/      |/
					//  a-------+
					// onePositive: a (b,c); b (a,c); c (a,b)
					// Plane: a,b (opab,c); a,c (opac,b); b,c (opbc,a)
					// allPositive: (allnegatives)
					
					
					val Seq(a,b,c) = extractPositiveEdges(newCase)
					val opab = getOppositesOnPlane(a,b)
					val opac = getOppositesOnPlane(a,c)
					val opbc = getOppositesOnPlane(b,c)
					
					setZero(Seq(
						Seq(a,b),Seq(a,c),Seq(b,c), // onePositive
						Seq(opab._1,opab._2,c),Seq(opac._1,opac._2,b), // Plane
						Seq(opbc._1,opbc._2,a),
						extractNegativeEdges(newCase) // allPositive
					).minBy( errorSum ):_*)
					
					
				case `fourPositivesUnconnected` =>
					//     c-------+
					//    /|      /|
					//   / |     / |
					//  +--+----b  |
					//  |  +----+--d
					//  | /     | /
					//  |/      |/
					//  a-------+
					// onePositive: a (b,c,d); b (a,c,d); c (a,b,d); d (a,b,c)
					// Plane: a,b (opab,c,d); a,c (opac,b,d); b,c (opbc,a,d); a,d (opad,b,c); b,d (opbd,a,c) ; c,d (opcd,a,b)
					// allPositive: (negatives)
					val Seq(a,b,c,d) = extractPositiveEdges(newCase)
					val opab = getOppositesOnPlane(a,b)
					val opac = getOppositesOnPlane(a,c)
					val opbc = getOppositesOnPlane(b,c)
					val opad = getOppositesOnPlane(a,d)
					val opbd = getOppositesOnPlane(b,d)
					val opcd = getOppositesOnPlane(c,d)
					
					setZero(Seq(
						Seq(b,c,d),Seq(a,c,d),Seq(a,b,d),Seq(a,b,c), // onePositive
						Seq(opab._1,opab._2,c,d),Seq(opac._1,opac._2,b,d), // Plane
						Seq(opbc._1,opbc._2,a,d),Seq(opad._1,opad._2,b,c),
						Seq(opbd._1,opbd._2,a,c),Seq(opcd._1,opcd._2,a,b),
						extractNegativeEdges(newCase) // allPositive
					).minBy( errorSum ):_*)
					

				case `fourPositivesTwoTimesTwoConnected` =>
					//     p0------p1
					//    /|      /|
					//   / |     / |
					//  +--+----+  |
					//  |  +----+--+
					//  | /     | /
					//  |/      |/
					//  p2------p3
					// twoPositive: p0,p1 (p2,p3); p2,p3 (p0,p1)
					// allPositive: (negatives)
					val Seq(a,b,c,d) = extractPositiveEdges(newCase)
					val (p0,p1,p2,p3) =
					if(sameEdge(a,b))
						(a,b,c,d)
					else if(sameEdge(a,c))
						(a,c,b,d)
					else
						(a,d,b,c)
					
					setZero(Seq(
						Seq(p0,p1),Seq(p2,p3), // twoPositive
						extractNegativeEdges(newCase)
					).minBy( errorSum ):_*)
					
					
				case `twoNegativesOverThreeEdges` =>
					//     d-------n1
					//    /|      /|
					//   / |     / |
					//  a--+----e  |
					//  |  b----+--f
					//  | /     | /
					//  |/      |/
					//  n0------c
					// zeroNegativeZero: a,n0,b (a,n1,b); a,n0,c (a,n1,c); b,n0,c (b,n1,c)
					//                   d,n1,e (d,n0,e); d,n1,f (d,n0,f); e,n1,f (e,n0,f)
					// allPositive: (n0,n1)
					
					val Seq(n0,n1) = extractNegativeEdges(newCase)
					val (a,b,c) = getNeighbours(n0)
					val (d,e,f) = getNeighbours(n1)
					
					setZero(Seq(
						Seq(n1,a,b), Seq(n1,a,c), Seq(n1,b,c), // zeroNegativeZero
						Seq(n0,d,e), Seq(n0,d,f), Seq(n0,e,f),
						Seq(n0,n1) // allPositive
					).minBy( errorSum ):_*)


				case left =>
					println("not handled: " + left)
			}
		}
		
		(newData, newCase)
	}
	
	// Erzeugt aus den Daten für Stabile Fälle die eigentlichen Hexaeder
	def data2hexaeder(data: IndexedSeq[Float], exactCase:Int): Polyeder = {
		caseTypeLookup(exactCase) match{
			case `allNegative` =>
				return EmptyHexaeder
				

			case `allPositive` =>
				return FullHexaeder
				
				
			case `onePositive` =>
				val hexaeder = new Hexaeder
				
				//Index der Positiven Ecke
				val p0 = log2(exactCase)
				
				// Alle Nachbarn von p0 ermitteln
				val n1 = p0 ^ 1
				val n2 = p0 ^ 2
				val n3 = p0 ^ 4
				
				// An die positive Ecke angrenzende Ecken n1,n2,n3:
				// jeder vertex wird in richtung der positiven Ecke interpoliert
				hexaeder(n1, log2(n1 ^ p0)) = interpolate(data(p0), data(n1), n1 - p0 < 0)
				hexaeder(n2, log2(n2 ^ p0)) = interpolate(data(p0), data(n2), n2 - p0 < 0)
				hexaeder(n3, log2(n3 ^ p0)) = interpolate(data(p0), data(n3), n3 - p0 < 0)
				
				
				// in Diesem Fall müssen Vertices aufeinander gelegt werden. 
				// Je nachdem welche vertices übereinender gelegt werden, ist 
				// eine andere Seitenfläche oben. Es gilt die Vertices so
				// übereinander zu legen, dass die übrig gebliebene Seitenfläche
				// mit ihrer richtung möglichst mit ihrer ausgangsrichtung im 
				// vollen Hexaeder übereinstimmt. Dies ist wichtig, damit die
				// texturen, die Auf dem Hexaeder aufliegen sollen, auch oben 
				// aufliegen und nicht an steilen Wänden sind.
				
				// val Seq(a1,a2,a3) = ( Seq(n1,n2,n3) sortBy data ) map ( _ ^ p0 )
				// Hardcoded aus Performance-Gründen:
				var a1 = n1
				var a2 = n2
				var a3 = n3
				
				if( data(a2) < data(a1) ) {
					val tmp = a2
					a2 = a1
					a1 = tmp
				}
				
				if( data(a3) < data(a2) ) {
					val tmp = a3
					a3 = a2
					a2 = tmp
				}
				
				if( data(a2) < data(a1) ) {
					val tmp = a2
					a2 = a1
					a1 = tmp
				}
				
				a1 ^= p0
				a2 ^= p0
				a3 ^= p0
				
				// An die positive Ecke über 2 Kanten angrenzenden Ecken auf eine direkt angrenzende setzen
				hexaeder(p0 ^ a1 ^ a2) = hexaeder(p0 ^ a2)
				hexaeder(p0 ^ a1 ^ a3) = hexaeder(p0 ^ a3)
				hexaeder(p0 ^ a2 ^ a3) = hexaeder(p0 ^ a3)
				hexaeder(7-p0) = hexaeder(p0 ^ a3)
				
				return hexaeder
				
				
			case `twoPositivesConnected` =>
				val hexaeder = new Hexaeder
				val Seq(p0, p1) = extractPositiveEdges(exactCase)

				val axisa = otherAxisA(p0 ^ p1)
				val axisb = otherAxisB(p0 ^ p1)
				
				// Sortierung wegen: siehe onePositive
				val a2 = if( data(p0^axisa) + data(p1^axisa) < data(p0^axisb) + data(p1^axisb) )
						axisb
					else
						axisa
				
				//direkt anliegende nachbarn
				hexaeder(p0^axisa,log2(axisa)) = interpolate(data(p0),data(p0^axisa), (p0 & axisa) != 0 )
				hexaeder(p1^axisa,log2(axisa)) = interpolate(data(p1),data(p1^axisa), (p1 & axisa) != 0 )
				hexaeder(p0^axisb,log2(axisb)) = interpolate(data(p0),data(p0^axisb), (p0 & axisb) != 0 )
				hexaeder(p1^axisb,log2(axisb)) = interpolate(data(p1),data(p1^axisb), (p1 & axisb) != 0 )
			
				// gegenüberliegenden Vertices
				hexaeder(7-p1) = hexaeder(p0 ^ a2)
				hexaeder(7-p0) = hexaeder(p1 ^ a2)

				return hexaeder
				

			case `fourPositivesConnectedOnPlane` =>
				val hexaeder = new Hexaeder
				
				val Seq(n1,n2,n3,n4) = extractNegativeEdges(exactCase)
				
				// Ermittle die Achse, die normal zur Plane steht
				val axis = (n1 & n2 & n3 & n4) | ((7-n1) & (7-n2) & (7-n3) & (7-n4))
				val direction = (n1 & axis) > 0
				val log2axis = log2(axis)
				
				hexaeder(n1, log2axis) = interpolate(data(n1), data(n1^axis), direction)
				hexaeder(n2, log2axis) = interpolate(data(n2), data(n2^axis), direction)
				hexaeder(n3, log2axis) = interpolate(data(n3), data(n3^axis), direction)
				hexaeder(n4, log2axis) = interpolate(data(n4), data(n4^axis), direction)
				
				return hexaeder
				

			case `threeNegativesConnected` => // = zeroPositiveZero
				
				/* Sicht von oben:
				n0--n1
				|    |
				++--n2
				
				n0 und n2 sind 0, und nur der vertex bei n1 wird eingedrückt
				*/
				
				val hexaeder = new Hexaeder

				val Seq(a,b,c) = extractNegativeEdges(exactCase)
				val axis = 7 - ((a & b & c) ^ (a | b | c))
				val n1 = (7 - axis) ^ (a^b^c)
				
				//Hexaeder an n1 eindrücken
				hexaeder(n1,log2(axis)) = interpolate(data(n1),data(n1^axis),n1 - (n1^axis) >= 0)
				//TODO: immer axis 1,2,4 verwenden!
				return hexaeder
			case _ =>
				// dieser Hexaeder sollte nicht mehr auftreten, wird aber
				// dennoch erzeugt falls vorher ein Fehler in der
				// transformToStable aufgetreten ist.
				return BrokenHexaeder
		}
	}
	
	val cases = Map(
	allNegative -> Set(0x00),
	onePositive -> Set(0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80),
	twoPositivesConnected -> Set(0x03, 0x0C, 0x30, 0xC0,  0x05, 0x0A, 0x50, 0xA0,  0x11, 0x22, 0x44, 0x88),
	fourPositivesConnectedOnPlane -> Set(0x0F, 0xF0, 0x33, 0xCC, 0x55, 0xAA),
	twoPositivesOverTwoEdges -> Set(0x09, 0x21, 0x41, 0x06, 0x12, 0x82, 0x14, 0x84, 0x28, 0x48, 0x90, 0x60),
	twoPositivesOverThreeEdges -> Set(0x81, 0x42, 0x24, 0x18),
	threePositivesUnconnected -> Set(0x29, 0x49, 0x61, 0x16, 0x86, 0x92, 0x94, 0x68),
	fourPositivesUnconnected -> Set(0x69, 0x96),
	threePositivesConnected -> Set(0xC4, 0x70, 0xC8, 0xD0, 0x2A, 0x15, 0x4C, 0x8C, 0x0D, 0x51, 0xA8, 0x07, 0x54, 0xE0, 0xB0, 0x0E, 0x0B, 0x8A, 0x13, 0x31, 0x23, 0xA2, 0x45, 0x32),
	diagonalHalf -> Set(0x17, 0xD4, 0xB2, 0x2B, 0x71, 0xE8, 0x4D, 0x8E),
	threePositivesOneUnconnected -> Set(0x52, 0x58, 0x62, 0xA1, 0x26, 0x98, 0xA4, 0x25, 0x1A, 0xC1, 0x89, 0x1C, 0x19, 0x91, 0x2C, 0x83, 0x64, 0x46, 0x38, 0x4A, 0xC2, 0x34, 0x43, 0x85),
	fourPositivesInLine -> Set(0xE2, 0xB1, 0x2E, 0xCA, 0x1D, 0xD8, 0x47, 0xD1, 0xE4, 0x5C, 0x8B, 0xB8, 0xA3, 0x35, 0x72, 0x4E, 0x1B, 0x27, 0x53, 0xC5, 0xAC, 0x3A, 0x74, 0x8D),
	fourPositivesOneUnconnected -> Set(0xA9, 0x1E, 0x39, 0x95, 0xE1, 0x59, 0x87, 0x36, 0x65, 0x2D, 0x56, 0x6A, 0x78, 0xD2, 0x9C, 0x4B, 0xC6, 0xB4, 0x6C, 0x93, 0xA6, 0x63, 0xC9, 0x9A),
	threeNegativesConnected -> Set(0x4F, 0x73, 0xD5, 0xEC, 0xCE, 0xAB, 0x3B, 0x37, 0xCD, 0x8F, 0x5D, 0xF2, 0xAE, 0xEA, 0xDC, 0x75, 0xF8, 0xF1, 0xBA, 0xF4, 0x57, 0xB3, 0x2F, 0x1F),
	fourPositivesTwoTimesTwoConnected -> Set(0x99, 0xC3, 0x66, 0xA5, 0x5A, 0x3C),
	threeNegativesOneUnconnected -> Set(0x5E, 0x6E, 0xDA, 0xBC, 0x9D, 0xE5, 0xC7, 0x76, 0xB5, 0xE6, 0x3E, 0xB9, 0x67, 0xA7, 0xAD, 0xCB, 0x7A, 0x3D, 0x5B, 0x7C, 0x9B, 0xE3, 0xD9, 0xD3),
	twoNegativesConnected -> Set(0xF3, 0xAF, 0x3F, 0x77, 0xFC, 0xF5, 0xFA, 0xEE, 0x5F, 0xCF, 0xDD, 0xBB),
	threeNegativesUnconnected -> Set(158, 182, 121, 107, 233, 151, 214, 109),
	twoNegativesOverTwoEdges -> Set(0xF9, 0xF6, 0xDE, 0xEB, 0x7B, 0xD7, 0x7D, 0xED, 0xBE, 0xB7, 0x6F, 0x9F),
	twoNegativesOverThreeEdges -> Set(0x7E, 0xBD, 0xDB, 0xE7),
	oneNegative -> Set(0xDF, 0x7F, 0xEF, 0xFB, 0xF7, 0xBF, 0xFE, 0xFD),
	allPositive -> Set(0xFF)
	)
	
	// Umkehrabbildung von "cases"
	val caseTypeLookup = {
		for(i <- 0 until 256) yield {
			var id = broken
			for( (name, caseset) <- cases )
				if(caseset contains i)
					id = name

			id
		}
	}
}


