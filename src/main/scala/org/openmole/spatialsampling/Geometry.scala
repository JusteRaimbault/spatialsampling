package org.openmole.spatialsampling

import org.openmole.spatialsampling.OSM.OSMObject

object Geometry {

  /**
    *
    * @param vertices array of vertices - last always equals to the first so that vertices[i]vertices[i+1] are all polygon edges
    */
  case class Polygon(
                    vertices: Array[Point]
                    ) {

  }

  object Polygon {

    /**
      * polygon from OSM way (rq: no projection)
      * @param way
      * @return
      */
    def apply(way: OSMObject.Way): Polygon = {
      Polygon(
        way.nodes.map(n => Point(n.getX,n.getY)).toArray
      )
    }

  }

  case class Point(x: Double, y: Double) {

    /**
      * winding number algorithm
      * @return
      */
    def inPolygon(polygon: Polygon): Boolean = {
      !(polygon.vertices.dropRight(1).zip(polygon.vertices.drop(1)).map{
        case (pi,pj) =>
          if(pi.y==pj.y) 0
          else if (pi.y < pj.y&&pi.y<=y&&pj.y>y&&((y - pi.y)/(pj.y - pi.y)*(pj.x - pi.x)+pi.x - x)>0) 1
          else if (pi.y>pj.y&&pi.y>y&&pj.y<=y&&((y - pi.y)/(pj.y - pi.y)*(pj.x - pi.x)+pi.x - x)>0) -1
          else 0
      }.sum==0)
    }

  }


}
