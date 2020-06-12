package org.openmole.spatialsampling



object Test extends App {

  //TestGeometry.testInPolygon()
  TestOSM.testOSM()

  object TestOSM {

    def testOSM(): Unit = {
      println(gridToString(
        Generation.osmBuildingsGrid(51.5248763, -0.1329176, 500, 50)
      ))
    }

  }


  object TestGeometry {

    def testInPolygon(): Unit = {

      import org.openmole.spatialsampling.Geometry.{Point, Polygon}

      //val p = Polygon(Array(Point(1.0,0.0), Point(2.0,0.0), Point(2.0,1.0), Point(1.0,1.0), Point(1.0, 0.0)))
      val p = Polygon(Array(Point(1.0,0.0), Point(2.0,1.0), Point(1.0, 2.0), Point(0.0, 1.0), Point(1.0, 0.0)))
      println(Point(0.1,0.1).inPolygon(p))
      println(Point(1.5,0.1).inPolygon(p))
      println(Point(2.25,0.25).inPolygon(p))
      println(Point(1.25,1.25).inPolygon(p))
    }

  }

}
