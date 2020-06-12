package org.openmole.spatialsampling

object GIS {


  def WGS84ToPseudoMercator(lon: Double, lat: Double): (Double, Double) = {
    val x = lon * 20037508.34 / 180
    val y = (math.log(math.tan((90 + lat) * math.Pi / 360)) / (math.Pi / 180) ) * 20037508.34 / 180
    (x, y)
  }

  def PseudoMercatorToWGS84Mercator(x: Double, y: Double): (Double, Double) = {
    val lon = (x / 20037508.34) * 180
    val lat = (y / 20037508.34) * 180
    (lon, 180 / math.Pi * (2 * math.atan(math.exp(lat * math.Pi / 180)) - math.Pi / 2))
  }

  def wgs84window(lon: Double,lat: Double,windowSize: Double): (Double,Double,Double,Double) = {
    val (x, y) = WGS84ToPseudoMercator(lon, lat)
    val (west, south) = PseudoMercatorToWGS84Mercator(x - windowSize / 2, y - windowSize / 2)
    val (east, north) = PseudoMercatorToWGS84Mercator(x + windowSize / 2, y + windowSize / 2)
    (west,south,east,north)
  }

}
