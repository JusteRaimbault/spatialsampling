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

    def project(projection: (Double,Double) => (Double,Double)): Polygon = this.copy(vertices.map(_.project(projection)))

  }

  object Polygon {

    /**
      * polygon from OSM way (rq: no projection)
      * @param way way
      * @return
      */
    def apply(way: OSMObject.Way): Polygon = {
      if (way==null) return null
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

    def project(projection: (Double,Double) => (Double,Double)): Point = {
      val proj = projection(x,y)
      Point(proj._1, proj._2)
    }

  }


}
