package xöpäx

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import org.lwjgl.opengl.GL11._
import Util._

import actors.Futures

object World{
	
	/*
	val octree = time("genworld: "){
		WorldGenerator.genWorld
	}
	time("genmesh: ")(octree.genMesh)
	*/
	
	val octree = {
		WorldSerializer.load match {
		case Some(s) => s
		case None => 
			val gen = time("genworld: ")(WorldGenerator.genWorld)
			          time("genmesh: ")(gen.genMesh)
			gen
		}
	}
	

	Runtime.getRuntime.gc
	
	//raytracer zum anclicken von Zellen
	def raytracer(from:Vec3,direction:Vec3,top:Boolean,distance:Float):Option[Vec3i] = {
		// der raytracer ist fehlerhaft falls die startposition genau auf einen Integer fällt ganzzahling
		for(i <- 0 until 3){
			if(from(i) == floor(from(i)))
				from(i) += 0.000001f
		}
	
		val t = direction
		
		Draw addText direction
		
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
		
		var h:Hexaeder = null
		var i = 0
		
		// todo octreeoptimierung
		var axis = 0
		
		while(h == null && i < distance){
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

			h = apply(pos)

			if(!Util.lineHexaederIntersect(from-pos,direction,h))
				h = null
			
			i += 1
		}
		
		val prepos = pos.clone
		prepos(axis) -= step(axis)

		if(h != null)
			if(top)
				Some(prepos)
			else
				Some(pos)
		else
			None
	}
	
	def update(pos:Vec3i,h:Hexaeder){
		octree(pos) = h
		BulletPhysics.worldChange(pos)
	}
	
	def apply(pos:Vec3i) = 
		if(octree != null)
			octree(pos)
		else
			UndefHexaeder
		
	def draw{
		octree.draw
		
		val selection = raytracer(Controller.current.position,Controller.current.direction,false,100)
		selection match {
		case Some(v) =>
			Draw.addText("Position: (" + v.x.toInt + ", " + v.y.toInt + ", " + v.z.toInt + ")")
			// malt die Markierung der angewählten Zelle
			glDisable(GL_LIGHTING)
			glPushMatrix
			glTranslatef(v.x,v.y,v.z)
			val h = octree(v)
			glColor3f(1,1,1)
			Draw.renderHexaeder(h)
			glPopMatrix
			glEnable(GL_LIGHTING)
		case None =>
		}
		
		octree stream Controller.current.position
	}
}
