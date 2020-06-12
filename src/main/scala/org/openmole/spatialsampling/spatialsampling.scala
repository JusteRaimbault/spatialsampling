package org.openmole

package object spatialsampling {

  val DEBUG = true

  type RasterLayerData[N] = Array[Array[N]]
  type RasterData[N] = Seq[RasterLayerData[N]]
  type RasterDim = Either[Int,(Int,Int)]
  type Point = (Double,Double)
  type SpatialField[N] = Map[Point,Array[N]]
  type ShortestPaths = Map[(Network.Node,Network.Node), (Seq[Network.Node],Seq[Network.Link],Double)]

  def log(msg: String): Unit = if(DEBUG) println(msg)

  def gridToString(a: RasterLayerData[Double]): String = {
    Array.fill(a(0).length)("-").mkString("")+"\n"+
      a.map(row => "|"+row.map(d => if (d>0.0) "0" else " ").mkString("")+"|").mkString("\n")+"\n"+
      Array.fill(a(0).length)("-").mkString("")
  }

}
