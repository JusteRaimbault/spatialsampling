package org.openmole.spatialsampling

import scala.util.Random

object Math {

  def kernelMixture(worldSize: RasterDim,
                    centers: Either[Int,Seq[(Int,Int)]],
                    kernel: (Double,Double)=>Double,
                    rng: Random
                   ): Array[Array[Double]] = {
    val w = worldSize match {case Left(l) => l; case Right((ww,_)) => ww}
    val h = worldSize match {case Left(l) => l; case Right((_,hh)) => hh}
    val vals = Array.fill(w,h)(0.0)
    val coords = centers match {
      case Left(i) => Seq.fill(i){(rng.nextInt(w),rng.nextInt(h))}
      case Right(c) => c
    }
    for(i<- 0 until w; j<- 0 until h){
      for(c <- coords){
        vals(i)(j) = vals(i)(j) + kernel(i - c._1,j - c._2)
      }
    }
    vals
  }

  def diffuse(a: Array[Array[Double]], alpha: Double): Array[Array[Double]] = {
    val newVals = a.clone()
    val (height,width) = (a.length,a(0).length)

    for (i <- a.indices; j <- a(0).indices) {
      // diffuse in neigh cells
      val d = a(i)(j)
      if (!d.isNaN) {
        if (i >= 1) {
          newVals(i - 1)(j) = newVals(i - 1)(j) + (alpha / 8) * d
        }
        if (i < height - 1) {
          newVals(i + 1)(j) = newVals(i + 1)(j) + (alpha / 8) * d
        }
        if (j >= 1) {
          newVals(i)(j - 1) = newVals(i)(j - 1) + (alpha / 8) * d
        }
        if (j < width - 1) {
          newVals(i)(j + 1) = newVals(i)(j + 1) + (alpha / 8) * d
        }
        if (i >= 1 && j >= 1) {
          newVals(i - 1)(j - 1) = newVals(i - 1)(j - 1) + (alpha / 8) * d
        }
        if (i >= 1 && j < width - 1) {
          newVals(i - 1)(j + 1) = newVals(i - 1)(j + 1) + (alpha / 8) * d
        }
        if (i < height - 1 && j >= 1) {
          newVals(i + 1)(j - 1) = newVals(i + 1)(j - 1) + (alpha / 8) * d
        }
        if (i < height - 1 && j < width - 1) {
          newVals(i + 1)(j + 1) = newVals(i + 1)(j + 1) + (alpha / 8) * d
        }
        //delete in the cell (¡ bord effect : lost portion is the same even for bord cells !)
        // to implement diffuse as in NL, put deletion inside boundary conditions checking
        newVals(i)(j) = newVals(i)(j) - alpha * d
      }
    }
    newVals
  }

  def convolution2dDirect(matrix: Array[Array[Double]],mask: Array[Array[Double]],
                          //operator: Array[Double]=>Double = {case a => if(a.filter(_>0.0).size>0)1.0 else 0.0})
                          filter: Double=>Double = { d => if(d > 0.0)1.0 else 0.0}
                         )
  : Array[Array[Double]] = {
    assert(mask.length%2==1&&mask(0).length%2==1,"mask should be of uneven size")
    val sizes = matrix.map(_.length);assert(sizes.max==sizes.min,"array should be rectangular")
    val masksizes = mask.map(_.length);assert(masksizes.max==masksizes.min,"mask should be rectangular")
    val (paddingx,paddingy) = ((mask.length-1)/2,(mask(0).length-1)/2)
    val padded = Array.tabulate(matrix.length+2*paddingx,matrix(0).length+2*paddingy){
      case (i,j) if i<paddingx||i>=(matrix.length+paddingx)||j<paddingy||j>=(matrix(0).length+paddingy) => 0.0
      case (i,j) => matrix(i-paddingx)(j-paddingy)
    }
    val res = Array.fill(matrix.length+2*paddingx,matrix(0).length+2*paddingy)(0.0)
    for(i <- paddingx until (res.length - paddingx);j <- paddingy until (res(0).length-paddingy)){
      val masked = Array.fill(mask.length,mask(0).length)(0.0)
      for(k <- - paddingx to paddingx;l <- - paddingy to paddingy){
        //assert(i+k<matrix.length&j+l<matrix(0).length,"size : "+i+" "+j+" "+k+" "+" "+l+" for a matrix of size "+matrix.length+";"+matrix(0).length)
        masked(k+paddingx)(l+paddingy)=padded(i+k)(j+l)*mask(k+paddingx)(l+paddingy)
      }
      res(i)(j) = filter(masked.flatten.sum)
    }
    //res.zip(matrix).map{case (row,initrow) => row.take(initrow.length + paddingy).takeRight(initrow.length)}.take(matrix.length+paddingx).takeRight(matrix.length)
    res.map{ row => row.slice(paddingy,row.length-paddingy)}.slice(paddingx,res.length-paddingx)
  }

}
