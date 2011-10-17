
import simplex3d.math.float._
import simplex3d.math.float.functions._
import scala.collection.mutable._

case class Triangle(v1:Vec3,v2:Vec3,v3:Vec3){
  val normal = cross(v2-v1, v3-v1)
  val h = dot(normal,v1) // n^t*v = h
  def isOver(v:Vec3) = {
    dot(normal,v)-h > 0
  }
  var vertices = Seq[Vec3]()
}

case class Edge(v1:Vec3,v2:Vec3)

object QuickHull {
  def apply(vertices: Buffer[Vec3]) {
    val es = new Stack[Edge]
    val faces = new ArrayBuffer[Triangle](20)
    
    def swap(i:Int,j:Int){
      val temp = vertices(i)
      vertices(i) = vertices(j)
      vertices(j) = temp
    }
    
    
    for(i ← Range(0,vertices.size) ) {
      if ( vertices(i).x > vertices(0).x )
        swap(0,i)
      if ( vertices(i).x < vertices(1).x )
        swap(1,i)
    }
    
    def distanceToLine(l1:Vec3,l2:Vec3,p:Vec3) = {
      length( cross( p - l1, p - l2 ) ) / length( l2 - l1 )
    }
    
    var distance = distanceToLine(vertices(0),vertices(1),vertices(2))
    for( i ← Range(3,vertices.size) ) {
      val newDistance = distanceToLine(vertices(0),vertices(1),vertices(i))
      if( newDistance > distance ) {
        swap(2,i)
      }
    }
    
    val face1 = new Triangle(vertices(0),vertices(1),vertices(2))
    val face2 = new Triangle(vertices(0),vertices(2),vertices(1))
    
    val (v1,v2) = vertices.view(0,vertices.size).partition( v => face1.isOver(v) )
    face1.vertices ++= v1
    face2.vertices ++= v2
    
    
    // TODO noch lange nich fertig implementiert
  }
}

/*

public Object3dList build () {
    Triangle3dPlus face1,face2; //first two faces created
    int frameNo = 1;
    EdgeStack es = new EdgeStack(); //used to find boundary of hole
    Object3dList faces = new Object3dList(20);
    findmaxmin();
    //make p[3] the furthest from p[0]p[1]
    HalfSpace h = new HalfSpace(pts[0],pts[1]);
    for (int i = 3; i < pts.length; i++) {
      if (h.normal.dot(pts[i])>h.normal.dot(pts[2])){
        Point3dObject3d temp = pts[2];
        pts[2] = pts[i];
        pts[i] = temp;
      }
    }      
    faces.addElement(face1 = new Triangle3dPlus(pts[0],pts[1],pts[2],frameNo++));
    faces.addElement(face2 = new Triangle3dPlus(pts[0],pts[2],pts[1],frameNo++));

    /* associate remaining points with one of these two faces */    
    for (int i = 3; i < pts.length; i++) {
      if (!face1.add(pts[i])) {
        face2.add(pts[i]);
      } 
    }
    
    /* Each time around the main loop we process one face */
    for (int i = 0; i < faces.size(); i++){
      Object3dList ps = new Object3dList(20); //pts associated with deleted faces
      Object3d o = faces.elementAt(i);
      if (!(o instanceof Triangle3dPlus)) {
        continue;
      }
      Triangle3dPlus selected = (Triangle3dPlus)o;
      if (selected.lastFrame <= frameNo) {
        continue;
      }
      Point3dObject3d newp = selected.extreme();
      if (newp == null) continue;
      /* delete faces that this vertex can see*/
      for (int j = 0; j < faces.size(); j++){
              o = faces.elementAt(j);
              if (o instanceof Triangle3dPlus) {
          Triangle3dPlus t = (Triangle3dPlus)o;
          if (t.lastFrame>frameNo && t.inside(newp)) {
            t.lastFrame = frameNo;
            /* update boundary of hole */
            es.putp(t.tri[0],t.tri[1]);
            es.putp(t.tri[1],t.tri[2]);
            es.putp(t.tri[2],t.tri[0]);
            /*add the points associated with this face to ps */
            ps.append(t.getPoints());
          }
        }
      }
      selected.select(frameNo++);
      
      while (!es.isEmpty()){
        Edge3d e = es.get();
        Triangle3dPlus t =new Triangle3dPlus(e.start,e.end,newp,frameNo++);
        Object3dList ps2 = new Object3dList(ps.size());
        for (int j = ps.size() -1; j >= 0; j--){
          Point3dObject3d p = (Point3dObject3d)ps.elementAt(j); 
          if ((p!=newp) && !t.add(p)) {
            ps2.addElement(p);
          }
        }
        ps = ps2;
        faces.addElement(t);
      }
    }
    faces.lastFrame = frameNo;
    return faces;
  }

*/

