package org.openmole

package object spatialsampling {

  type RasterLayerData[N] = Array[Array[N]]
  type RasterData[N] = Seq[RasterLayerData[N]]
  type RasterDim = Either[Int,(Int,Int)]
  type Point = (Double,Double)
  type SpatialField[N] = Map[Point,Array[N]]

}
