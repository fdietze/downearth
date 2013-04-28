package downearth

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import Util._

object World {
	
	val octree = {
		WorldSerializer.load match {
			case Some(s) => s
			case None => WorldGenerator.genWorld
		}
	}

	//raytracer zum anklicken von Zellen
	def raytracer(from:Vec3,direction:Vec3,top:Boolean,distance:Double):Option[Vec3i] = {
		// der raytracer ist fehlerhaft falls die startposition genau auf einen Integer fällt ganzzahling
		for(i <- 0 until 3) {
			if(from(i) == floor(from(i)))
				from(i) += 0.000001
		}
	
		val t = direction
		
		val pos = Vec3i(floor(from))
		val step = Vec3i(sign(direction))
		val tMax = Vec3(0)
		val tDelta = Vec3(0)
		
		tMax.x = if(step.x == 1) (ceil(from.x)-from.x)/abs(t.x) else (from.x-floor(from.x))/abs(t.x)
		tMax.y = if(step.y == 1) (ceil(from.y)-from.y)/abs(t.y) else (from.y-floor(from.y))/abs(t.y)
		tMax.z = if(step.z == 1) (ceil(from.z)-from.z)/abs(t.z) else (from.z-floor(from.z))/abs(t.z)
		
		tDelta.x = 1/abs(t.x)
		tDelta.y = 1/abs(t.y)
		tDelta.z = 1/abs(t.z)
		
		var h:Polyeder = apply(pos).h
		if(!Util.rayPolyederIntersect(from-pos,direction,h))
			h = null
		var i = 0
		
		// todo octreeoptimierung
		var axis = 0
		
		while(h == null && i < distance) {
			if(tMax.x < tMax.y) {
				if(tMax.x < tMax.z) {
					axis = 0
					pos.x += step.x
					tMax.x += tDelta.x
				} else {
					axis = 2
					pos.z += step.z;
					tMax.z += tDelta.z;
				}
			} else {
				if(tMax.y < tMax.z) {
					axis = 1
					pos.y += step.y;
					tMax.y+= tDelta.y;
				} else {
					axis = 2
					pos.z += step.z;
					tMax.z += tDelta.z;
				}
			}

			h = apply(pos).h

			if(!Util.rayPolyederIntersect(from-pos,direction,h))
				h = null
			
			i += 1
		}
		
		val prepos = pos.clone
		prepos(axis) -= step(axis)
		
		if(h != null){
			if(top && rayCellTest(from-pos,direction,h.asInstanceOf[Hexaeder]))
				Some(prepos)
			else
				Some(pos)
		}
		else
			None
	}
	
	def update(pos:Vec3i, l:Leaf) {
		octree(pos) = l
		BulletPhysics.worldChange(pos)
	}
	
	def apply(pos:Vec3i) = octree(pos)

}
