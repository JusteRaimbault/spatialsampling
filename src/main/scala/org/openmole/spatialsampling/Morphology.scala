package org.openmole.spatialsampling

import scala.util.Random

case class Morphology(
                           height: Double,
                           width: Double,
                           area: Double,
                           moran: Double,
                           avgDistance: Double,
                           entropy: Double,
                           slope: (Double,Double),
                           density: Double,
                           components: Double,
                           avgDetour: Double,
                           avgBlockArea: Double,
                           avgComponentArea: Double,
                           fullDilationSteps: Double,
                           fullErosionSteps: Double,
                         )

object Morphology {

  def apply(grid: RasterLayerData[Double])(implicit rng: Random): Morphology = {
    val cachedNetwork = Network.gridToNetwork(grid)
    Morphology(
      grid.length,grid(0).length,
      grid.flatten.sum,
      moranDirect(grid),
      distanceMeanDirect(grid),
      Math.entropy(grid.flatten),
      Math.slope(grid.flatten),
      density(grid),
      components(grid,Some(cachedNetwork)),
      avgDetour(grid,Some(cachedNetwork)),
      avgBlockArea(grid,Some(cachedNetwork)),
      avgComponentArea(grid),
      fullDilationSteps(grid),
      fullErosionSteps(grid)
    )
  }

  def density(world: Array[Array[Double]]): Double = world.flatten.filter(_ > 0.0).map{_ => 1.0}.sum / world.flatten.length

  def avgBlockArea(world: Array[Array[Double]],cachedNetwork: Option[Network] = None): Double = {
    val nw = cachedNetwork match {case None => Network.gridToNetwork(world);case n => n.get}
    val components = Network.connectedComponentsTraverse(nw)
    val avgblockarea = components.size match {case n if n == 0 => 0.0;case _ => components.map{_.nodes.size}.sum/components.size.toDouble}
    avgblockarea
  }

  /**
    * avg component area
    * @param world world
    * @return
    */
  def avgComponentArea(world: Array[Array[Double]]): Double = {
    val inversedNetwork = Network.gridToNetwork(world.map{_.map{x => 1.0 - x}})
    val components = Network.connectedComponentsTraverse(inversedNetwork)
    if(components.nonEmpty){
      components.map{_.nodes.size}.sum/components.size
    }else 0.0
  }

  def distanceMeanDirect(matrix: Array[Array[Double]]): Double = {

    def totalQuantity = matrix.flatten.sum

    def numerator =
      (for {
        (c1, p1) <- zipWithPosition(matrix)
        (c2, p2) <- zipWithPosition(matrix)
      } yield distance(p1, p2) * c1 * c2).sum

    def normalisation = math.sqrt(matrix.flatten.length / math.Pi)

    if(totalQuantity==0.0||normalisation==0.0) return 0.0

    (numerator / (totalQuantity * totalQuantity)) / normalisation
  }

  def distance(p1: (Int,Int), p2: (Int,Int)): Double = {
    val (i1, j1) = p1
    val (i2, j2) = p2
    val a = i2 - i1
    val b = j2 - j1
    math.sqrt(a * a + b * b)
  }

  def zipWithPosition(m :Array[Array[Double]]): Seq[(Double, (Int,Int))] = {
    m.zipWithIndex.flatMap{
      case (row,i) =>
        row.zipWithIndex.map{
          case (content,j) => (content,(i,j))
        }
    }.toSeq
  }

  def moranDirect(matrix: Array[Array[Double]]): Double = {
    def decay(p1:(Int,Int),p2:(Int,Int)): Double = {
      if (p1==p2) 0.0
      else 1/distance(p1,p2)
    }
    def flatCells = matrix.flatten
    val totalPop = flatCells.sum
    val averagePop = totalPop / matrix.flatten.length


    def vals =
      for {
        (c1, p1) <- zipWithPosition(matrix)
        (c2, p2) <- zipWithPosition(matrix)
      } yield (decay(p1, p2) * (c1 - averagePop) * (c2 - averagePop),decay(p1, p2))



    def numerator : Double = vals.map{case (n,_)=>n}.sum
    def totalWeight : Double = vals.map{case(_,w)=>w}.sum

    def denominator =
      flatCells.map {
        p =>
          if (p == 0) 0
          else math.pow(p - averagePop.toDouble, 2)
      }.sum

    if (denominator == 0) 0.0
    else (matrix.flatten.length / totalWeight) * (numerator / denominator)
  }

  def components(world: Array[Array[Double]],cachedNetwork: Option[Network] = None): Double = {
    val nw = cachedNetwork match {case None => Network.gridToNetwork(world);case n => n.get}
    val components = Network.connectedComponentsTraverse(nw)
    components.size
  }

  def dilation(matrix: Array[Array[Double]],
               convol: (Array[Array[Double]],Array[Array[Double]],Double=> Double)=> Array[Array[Double]] = Math.convolution2dDirect
              ): Array[Array[Double]] =
    convol(matrix,Array(Array(0.0,1.0,0.0),Array(1.0,1.0,1.0),Array(0.0,1.0,0.0)),{d => if(d > 0.0)1.0 else 0.0})

  def erosion(matrix: Array[Array[Double]],
              convol: (Array[Array[Double]],Array[Array[Double]],Double=> Double)=> Array[Array[Double]] = Math.convolution2dDirect
             ): Array[Array[Double]] = {
    val mask = Array(Array(0.0, 1.0, 0.0), Array(1.0, 1.0, 1.0), Array(0.0, 1.0, 0.0))
    convol(matrix,
      mask,
      {d => if (d == mask.flatten.sum) 1.0 else 0.0 }
    )
  }

  def fullDilationSteps(matrix: Array[Array[Double]],
                        convol: (Array[Array[Double]],Array[Array[Double]],Double=> Double)=> Array[Array[Double]] = Math.convolution2dDirect
                       ): Double = {
    var steps = 0
    var complete = false
    var currentworld = matrix
    if(matrix.flatten.sum==0){return 0.0}
    while(!complete){
      currentworld = dilation(currentworld,convol)
      complete = currentworld.flatten.sum == currentworld.flatten.length
      steps = steps + 1
    }
    steps
  }

  /**
    * Number of steps to fully erode the image
    * @param matrix matrix
    * @return
    */
  def fullErosionSteps(matrix: Array[Array[Double]],
                       convol: (Array[Array[Double]],Array[Array[Double]],Double=> Double)=> Array[Array[Double]] = Math.convolution2dDirect
                      ): Double = {
    var steps = 0
    var complete = false
    var currentworld = matrix
    if(matrix.flatten.sum==matrix.flatten.length){return 0.0}
    while(!complete){
      currentworld = erosion(currentworld,convol)
      complete = currentworld.flatten.sum == 0
      steps = steps + 1
    }
    steps
  }

  def avgDetour(world: Array[Array[Double]],
                cachedNetwork: Option[Network] = None,
                sampledPoints: Int=50
               )(implicit rng: Random): Double = {
    if(world.flatten.sum==world.map{_.length}.sum) return 0.0
    val nw = cachedNetwork match {case None => Network.gridToNetwork(world);case n => n.get}
    val sampled = Math.sampleWithoutReplacement(nw.nodes.toSeq,sampledPoints)(rng)
    val paths = Network.shortestPaths(nw, sampled, sampled)
    val avgdetour = paths.toSeq.filter{!_._2._3.isInfinite}.map{
      case (_,(nodes,_,d))=>
        val (n1,n2) = (nodes.head,nodes.last)
        val de = math.sqrt((n1.x-n2.x)*(n1.x-n2.x)+(n1.y-n2.y)*(n1.y-n2.y))
        d/de
    }.filter{!_.isNaN}.filter{!_.isInfinite}.sum / paths.size
    avgdetour
  }



}
