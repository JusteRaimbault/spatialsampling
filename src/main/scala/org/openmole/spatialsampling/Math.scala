package org.openmole.spatialsampling

import org.apache.commons.math3.stat.regression.SimpleRegression

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
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

  def entropy(values: Array[Double]): Double = {
    val x = values.map{d => if (d.isNaN) 0.0 else d}
    val totalQuantity = x.sum
    //assert(totalQuantity > 0)

    totalQuantity match {
      case 0.0 => 0.0
      case _ =>
        x.map {p =>
          val quantityRatio = p / totalQuantity
          val localEntropy = if (quantityRatio == 0.0) 0.0 else quantityRatio * math.log (quantityRatio)
          localEntropy
        }.sum * (- 1 / math.log (x.length) )
    }
  }

  def slope(values: Array[Double]): (Double,Double) = {
    def distribution: Array[Double] = values.filter(_ > 0).sorted
    def distributionLog: Array[Array[Double]] = distribution.zipWithIndex.map { case (q, i) => Array(math.log(i + 1), math.log(q)) }
    val simpleRegression = new SimpleRegression(true)
    simpleRegression.addData(distributionLog)
    (simpleRegression.getSlope, simpleRegression.getRSquare)
  }

  def sampleWithoutReplacement[T](sampled: Iterable[T], samples: Int)(implicit rng: Random): Vector[T] =
    sampleWithoutReplacementBy[T](sampled,_ => 1.0 / sampled.size.toDouble, samples)

  def sampleWithoutReplacementBy[T](sampled: Iterable[T], probability: T => Double, samples: Int)(implicit rng: Random): Vector[T] = {
    assert(samples <= sampled.size,"Can not sample more than vector size : "+samples+" / "+sampled.size)
    Iterator.iterate((sampled, Vector.empty[T])) { case (rest, res) =>
      val totproba = rest.map(probability(_)).sum
      val normalizedProba = rest.toSeq.map(probability(_) / totproba)
      val sample = sampleOneBy[((T, Int), Double)](rest.toSeq.zipWithIndex.zip(normalizedProba), _._2)
      (rest.toSeq.zipWithIndex.filter(_._2 != sample._1._2).map(_._1), Vector(sample._1._1) ++ res)
    }.take(samples).toSeq.last._2
  }

  def sampleOneBy[T](sampled: Iterable[T], probability: T => Double)(implicit rng: Random): T = {
    def f(s: (Iterable[T],T,Double,Double)): (Iterable[T], T, Double, Double) = (s._1.tail, s._1.head, s._3 + probability (s._1.head), s._4)
    f(Iterator.iterate((sampled,sampled.head,0.0,rng.nextDouble()))(f).takeWhile(s => s._3 < s._4&&s._1.nonEmpty).toSeq.last)._2
  }

  /**
    * part of JGrapht to avoid embedding the whole library
    */
  object GraphAlgorithms {

    class PairingNode {
      var key: Double = 0.0
      var heap: PairingHeap = _
      var value: (Network.Node, Network.Link) = _
      var oc: PairingNode = _
      var ys: PairingNode = _
      var os: PairingNode = _

      def decreaseKey(newKey: Double): Unit = {
        getOwner.decreaseKey(this, newKey)
      }

      def getOwner: PairingHeap = {
        if (heap.other != heap) {
          var root: PairingHeap = heap
          while (root != root.other) {
            root = root.other
          }
          var cur: PairingHeap = heap
          while (cur.other != root) {
            val next: PairingHeap = cur.other
            cur.other = root
            cur = next
          }
          heap = root
        }
        heap
      }
    }
    object PairingNode {
      def apply(k: Double): PairingNode = {
        val res = new PairingNode
        res.key = k
        res
      }
      def apply(h: PairingHeap, k: Double, v: (Network.Node, Network.Link)): PairingNode = {
        val res = new PairingNode
        res.key = k; res.value = v; res.heap = h
        res
      }
    }

    class PairingHeap {
      var root: PairingNode = _
      var size: Int = 0
      var other: PairingHeap = this

      def decreaseKey(n: PairingNode, newKey: Double): Unit = {
        val c: Int = Ordering[Double].compare(newKey, n.key)
        n.key = newKey
        if (c == 0 || root == n) return
        if (n.ys != null) n.ys.os = n.os
        if (n.os.oc == n) n.os.oc = n.ys else n.os.ys = n.ys
        n.ys = null
        n.os = null
        root = link(root, n)
      }

      def link(f: PairingNode, s: PairingNode): PairingNode = {
        if (s == null) f
        else if (f == null) s
        else if (Ordering[Double].compare(f.key, s.key) <= 0) {
          s.ys = f.oc
          s.os = f
          if (f.oc != null) f.oc.os = s
          f.oc = s
          f
        } else {
          link(s, f)
        }
      }

      def insert(key: Double, value: (Network.Node, Network.Link)):PairingNode = {
        val n: PairingNode = PairingNode(this, key, value)
        root = link(root, n)
        size=size + 1
        n
      }

      def isEmpty: Boolean = size == 0

      def findMin: PairingNode = root

      def clear(): Unit= {
        root = null
        size = 0
      }

      def deleteMin(): PairingNode = {
        if (size == 0) throw new NoSuchElementException()
        val oldRoot: PairingNode = root
        root = combine(cutChildren(root))
        size = size - 1
        oldRoot
      }

      def cutChildren(n: PairingNode): PairingNode = {
        val child: PairingNode = n.oc
        n.oc = null
        if (child != null) child.os = null
        child
      }

      def combine(l: PairingNode): PairingNode = {
        if (l == null) return null
        assert(l.os == null)
        var pairs: PairingNode = null
        var it: PairingNode = l
        var p_it: PairingNode = null
        while (it != null) {
          p_it = it
          it = it.ys
          if (it == null) {
            p_it.ys = pairs
            p_it.os = null
            pairs = p_it
          } else {
            val n_it: PairingNode = it.ys
            p_it.ys = null
            p_it.os = null
            it.ys = null
            it.os = null
            p_it = link(p_it, it)
            p_it.ys = pairs
            pairs = p_it
            it = n_it
          }
        }
        it = pairs
        var f: PairingNode = null
        while (it != null) {
          val nextIt: PairingNode = it.ys
          it.ys = null
          f = link(f, it)
          it = nextIt
        }
        f
      }

    }

    class DijkstraClosestFirstIterator(
                                      graph: Network,
                                      source: Network.Node,
                                      radius: Double,
                                      seen: mutable.HashMap[Network.Node, PairingNode],
                                      heap: PairingHeap
                                      ) extends Iterator[Network.Node] {
      override def hasNext: Boolean = {
        if (heap.isEmpty) return false
        val vNode: PairingNode = heap.findMin
        val vDistance: Double = vNode.key
        if (radius < vDistance) {
          heap.clear()
          return false
        }
        true
      }
      override def next(): Network.Node = {
        if (!hasNext) throw new NoSuchElementException()
        val vNode: PairingNode = heap.deleteMin()
        val v: Network.Node = vNode.value._1
        val vDistance: Double = vNode.key
        for (e <- graph.outgoingLinksOf(v)) {
          val u: Network.Node =  e.oppositeOf(v)
          val eWeight: Double = e.weight
          if (eWeight < 0.0) {throw new IllegalArgumentException("Negative edge weight not allowed")}
          updateDistance(u, e, vDistance + eWeight)
        }
        v
      }

      def updateDistance(v: Network.Node , e: Network.Link, distance: Double): Unit = {
        var node: PairingNode = seen.getOrElse(v, null)
        if (node == null) {
          node = heap.insert(distance, (v, e))
          seen.put(v, node)
        } else if (distance < node.key) {
          node.decreaseKey(distance)
          node.value = (node.value._1,e)
        }
      }

      def distanceAndPredecessorMap: Map[Network.Node, (Double, Network.Link)] = {
        val distanceAndPredecessorMap = new mutable.HashMap[Network.Node, (Double, Network.Link)]
        for (vNode : PairingNode <- seen.values) {
        val vDistance: Double = vNode.key
        if (radius >= vDistance) {
            val v = vNode.value._1
            distanceAndPredecessorMap.put(v, (vDistance, vNode.value._2))
          }
        }
        distanceAndPredecessorMap.toMap
      }

      def getPaths(to: Seq[Network.Node]): ShortestPaths = {
        val map: Map[Network.Node, (Double, Network.Link)] = distanceAndPredecessorMap
        val paths: mutable.HashMap[(Network.Node,Network.Node),(Seq[Network.Node],Seq[Network.Link],Double)] = new mutable.HashMap
        for (target <- to) {
          if (source == target) paths.put((source, source), (Seq(source), Seq.empty, 0.0))
          val edgeList: ArrayBuffer[Network.Link] = new ArrayBuffer
          val nodeList: ArrayBuffer[Network.Node] = new ArrayBuffer
          var cur: Network.Node = target
          var p = map.getOrElse(cur, null)
          if (p == null || p._1.isInfinity) paths.put((source, target),(Seq.empty,Seq.empty,Double.PositiveInfinity))
          var weight = 0.0
          nodeList.append(cur)
          while (p != null && !cur.equals(source)) {
            val e = p._2
            if (e == null) p = null
            else {
              edgeList.append(e)
              weight += e.weight
              cur = e.oppositeOf(cur)
              nodeList.append(cur)
              p = map.getOrElse(cur, null)
            }
          }
          paths.put((source, target), (nodeList.reverse,edgeList.reverse,weight))
        }
        paths.toMap
      }

    }
    object DijkstraClosestFirstIterator {
      def apply(g: Network, s: Network.Node, r: Double, h: PairingHeap): DijkstraClosestFirstIterator = {
        val map = new mutable.HashMap[Network.Node, PairingNode]
        val res = new DijkstraClosestFirstIterator(g,s,r,map,h)
        res.updateDistance(s, null, 0.0)
        res
      }
    }

    class DijkstraShortestPaths(
                               graph: Network,
                               from: Seq[Network.Node],
                               to: Seq[Network.Node],
                               radius: Double = Double.PositiveInfinity,
                               heap: PairingHeap = new PairingHeap
                               ) {
      def concat(s1: ShortestPaths, s2: ShortestPaths): ShortestPaths = s1++s2

      def getPaths: ShortestPaths = {
        (for{
          source <- from
          it = DijkstraClosestFirstIterator(graph, source, radius, heap)
          _ = while (it.hasNext) {it.next()}
        } yield it.getPaths(from)).reduce(concat)
      }
    }



  }

}
