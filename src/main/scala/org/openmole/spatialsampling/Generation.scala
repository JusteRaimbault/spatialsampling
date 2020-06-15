package org.openmole.spatialsampling


import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Generation {


  def expMixtureGrid(size: RasterDim,
                     centers: Int,
                     maxValue: Double,
                     kernelRadius: Double,
                     normalized: Boolean = false,
                     centerCoordinates: Seq[Point] = Seq.empty
                    )(implicit rng: Random): RasterLayerData[Double] = {
    def expKernel(x: Double, y: Double): Double = maxValue*math.exp(-math.sqrt(math.pow(x,2.0)+math.pow(y,2.0))/kernelRadius)
    val eithcenters = centerCoordinates.size match {
      case 0 => Left(centers)
      case _ => Right(centerCoordinates.map(c => (c._1.toInt,c._2.toInt)))
    }
    Math.kernelMixture(size,eithcenters,expKernel,rng)
  }


  def percolationGrid(size: Int,
                      percolationProba: Double,
                      bordPoints: Int,
                      linkwidth: Double,
                      maxIterations: Int,
                      percolateBuildings: Boolean = false
                     )(implicit rng: Random): RasterLayerData[Double] = {
    val percolatedGrid = Network.networkToGrid(percolationNetwork(size,percolationProba,bordPoints,linkwidth,maxIterations)(rng),linkwidth=linkwidth)
    if (percolateBuildings) percolatedGrid else percolatedGrid.map{_.map{1.0 - _}}
  }


  def blocksGrid(size: RasterDim,blocks: Int,blockMinSize: Int, blockMaxSize: Int)(implicit rng: Random): RasterLayerData[Double] = {
    val maxsize = math.max(blockMinSize,blockMaxSize)
    val minsize = math.min(blockMinSize,blockMaxSize)
    val w = size match {case Left(l) => l; case Right((ww,_)) => ww}
    val h = size match {case Left(l) => l; case Right((_,hh)) => hh}
    val vals = Array.fill(w,h)(0.0)
    for(_ <- 0 until blocks){
      val (i,j) = (rng.nextInt(w),rng.nextInt(h))
      val (ww,hh) = (minsize + rng.nextInt(maxsize-minsize + 1),minsize + rng.nextInt(maxsize-minsize + 1))
      // convention : if even, center on bottom right corner
      for(di <- 0 until ww){
        for(dj <- 0 until hh){
          val (k,l) = (i - ww / 2 + di,j - hh / 2 + dj)
          if(k>=0&l>=0&k<w&l<h){vals(k)(l) = vals(k)(l) + 1.0}
        }
      }

    }
    vals
  }

  def randomGrid(size: RasterDim)(implicit rng: Random) : RasterLayerData[Double] = {
    size match {
      case Left(s)=>Array.fill(s, s){ rng.nextDouble() }
      case Right((w,h))=>Array.fill(w, h){ rng.nextDouble() }
    }
  }


  def reactionDiffusionGrid(size: RasterDim,
                            growthRate: Double,
                            totalPopulation: Double,
                            alphaAtt: Double,
                            diffusion: Double,
                            diffusionSteps: Int,
                            initialConfiguration: Option[Seq[Seq[Double]]] = None,
                            iterImpl: Boolean = true
                           )(implicit rng: Random): Array[Array[Double]] = {
    val (width,height)= if(initialConfiguration.isDefined) (initialConfiguration.get.head.length,initialConfiguration.get.length) else size match {case Left(s)=>(s,s);case Right(c)=> c}
    var arrayVals: Array[Array[Double]] = if (initialConfiguration.isDefined) {
      val copyconfig = initialConfiguration.get.map{_.toVector}.toVector
      Array.tabulate(width,height){ (i,j) => copyconfig(i)(j)}
    } else Array.fill(width, height) { 0.0 }
    var population: Double = arrayVals.flatten.filter(!_.isNaN).sum
    var steps = 0
    var stepdeltapop = 0.0
    while (population < totalPopulation && stepdeltapop>=0.0) {

      val prevpop = population

      // add new population following pref att rule
      if (population == 0) {
        //choose random patch - if Nan a few will be lost
        for (_ <- 1 to growthRate.toInt) { val i = rng.nextInt(width); val j = rng.nextInt(height); arrayVals(i)(j) = arrayVals(i)(j) + 1 }
      }
      else {

        val oldPop: Array[Array[Double]] = arrayVals.map { _.map { x => math.pow(x / population, alphaAtt) } }
        val ptot = oldPop.flatten.filter(!_.isNaN).sum

        if (iterImpl) {
          for (_ <- 1 to growthRate.toInt) {
            var s = 0.0; val r = rng.nextDouble(); var i = 0; var j = 0
            //draw the cell from cumulative distrib
            while (s < r) {
              val d = oldPop(i)(j)
              if (!d.isNaN) s = s + (d / ptot)
              j = j + 1
              if (j == height) {
                j = 0; i = i + 1
              }
            }
            if (j == 0) {
              j = height - 1; i = i - 1
            } else {
              j = j - 1
            }
            arrayVals(i)(j) = arrayVals(i)(j) + 1
          }
        }else {

          val probas = Array.fill(growthRate.toInt)(rng.nextDouble()).sorted

          var s = 0.0
          var i = 0
          var j = 0
          var k = 0
          //draw the cell from cumulative distrib
          while (k < probas.length) {
            if (s <= probas(k)) {
              val d = oldPop(i)(j)
              if (!d.isNaN) s = s + (d / ptot)
              j = j + 1
              if (j == height) {
                j = 0
                i = i + 1
              }
            } else {
              k = k + 1
              val (ii,jj) = if (j == 0) (i - 1,height - 1) else (i,j - 1)

              arrayVals(ii)(jj) = arrayVals(ii)(jj) + 1
            }
          }

        }

      }

      // diffuse
      for (_ <- 1 to diffusionSteps) {
        arrayVals = Math.diffuse(arrayVals, diffusion)
      }

      // update total population
      population = arrayVals.flatten.filter(!_.isNaN).sum

      stepdeltapop = population - prevpop
      steps = steps + 1

    }
    arrayVals
  }


  def osmBuildingsGrid(lon: Double, lat: Double, windowSize: Double, worldWidth: Int): RasterLayerData[Double] = {
    val (west,south,east,north) = GIS.wgs84window(lon, lat, windowSize)
    val polygons = OSM.Buildings.getBuildings(west, south, east, north).map(_.project(GIS.WGS84ToPseudoMercator))
    println(polygons.map(_.vertices.map(_.x).min))
    println(polygons.map(_.vertices.map(_.y).min))
    val (x0, y0) = GIS.WGS84ToPseudoMercator(lon, lat)
    val (xmi, xma, ymi, yma) = (x0 - windowSize/2, x0 + windowSize/2, y0 - windowSize / 2, y0 + windowSize/2)
    println(xmi+" - "+ymi)
    val step = (windowSize  - 1) / worldWidth
    (for {
      x <- BigDecimal(xmi) to BigDecimal(xma) by step
      y <- BigDecimal(ymi) to BigDecimal(yma) by step
    } yield Geometry.Point(x.toDouble,y.toDouble)).map{p=>
      if (polygons.exists(p.inPolygon)) 1.0 else 0.0
    }.toArray.grouped(worldWidth).toArray
  }


  def percolationNetwork(worldSize: Int,
                         percolationProba: Double,
                         bordPoints: Int,
                         linkwidth: Double,
                         maxIterations:    Int
                        )(implicit rng: Random): Network = {
    var nw = gridNetwork(worldSize)
    var bordConnected = 0
    val xmin = nw.nodes.map{_.x}.min;val xmax = nw.nodes.map{_.x}.max
    val ymin = nw.nodes.map{_.y}.min;val ymax = nw.nodes.map{_.y}.max
    var iteration = 0
    while(bordConnected<bordPoints&&iteration<maxIterations){
      nw = nw.percolate(percolationProba,linkFilter={
        l: Network.Link => l.weight==0.0&&(
          (((l.e1.x!=xmin)&&(l.e2.x!=xmin))||((l.e1.x==xmin)&&(l.e2.x!=xmin))||((l.e2.x==xmin)&&(l.e1.x!=xmin)))&&
            (((l.e1.x!=xmax)&&(l.e2.x!=xmax))||((l.e1.x==xmax)&&(l.e2.x!=xmax))||((l.e2.x==xmax)&&(l.e1.x!=xmax)))&&
            (((l.e1.y!=ymin)&&(l.e2.y!=ymin))||((l.e1.y==ymin)&&(l.e2.y!=ymin))||((l.e2.y==ymin)&&(l.e1.y!=ymin)))&&
            (((l.e1.y!=ymax)&&(l.e2.y!=ymax))||((l.e1.y==ymax)&&(l.e2.y!=ymax))||((l.e2.y==ymax)&&(l.e1.y!=ymax)))
          )
      })
      val giantcomp =  Network.largestConnectedComponent(Network(nw.nodes,nw.links.filter{_.weight>0}))
      val nodesOnBord = giantcomp.nodes.filter{ n => n.x==xmin||n.x==xmax||n.y==ymin||n.y==ymax}
      bordConnected =nodesOnBord.size
      iteration = iteration + 1
    }
    nw
  }

  def gridNetwork(size: Int,diagLinks: Boolean = false): Network = {
    val (xstep,ystep)=(size/10,size/10)
    val ycoords = 0 to size by ystep
    val xcoords = 0 to size by xstep
    val coords: Seq[(Double,Double)] = xcoords.flatMap{xx: Int => ycoords.map{yy: Int => (xx.toDouble,yy.toDouble)}}
    val nodes: Seq[Seq[Network.Node]] = coords.zipWithIndex.map{ c=>  Network.Node(c._2,c._1._1,c._1._2)}.sliding(ycoords.size,ycoords.size).toSeq
    val edges = ArrayBuffer[Network.Link]()
    for (i <- nodes.indices ; j <- nodes.head.indices) {
      if(i-1>0){
        if(diagLinks&&j-1>0){edges.append(Network.Link(nodes(i)(j),nodes(i-1)(j-1),0.0))}
        edges.append(Network.Link(nodes(i)(j),nodes(i-1)(j),0.0))
        if(diagLinks&&j+1<nodes.head.size){edges.append(Network.Link(nodes(i)(j),nodes(i-1)(j+1),0.0))}
      }
      if(j-1>0){
        edges.append(Network.Link(nodes(i)(j),nodes(i)(j-1),0.0))
      }
      if(j+1<nodes.head.size){
        edges.append(Network.Link(nodes(i)(j),nodes(i)(j+1),0.0))
      }
      if(i+1<nodes.size){
        if(diagLinks&&j-1>0){edges.append(Network.Link(nodes(i)(j),nodes(i+1)(j-1),0.0))}
        edges.append(Network.Link(nodes(i)(j),nodes(i+1)(j),0.0))
        if(diagLinks&&j+1<nodes.head.size){edges.append(Network.Link(nodes(i)(j),nodes(i+1)(j+1),0.0))}
      }
    }
    Network(nodes.flatten.toSet,edges.toSet)
  }


}
