package org.openmole.spatialsampling

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


case class Network(
                    nodes: Set[Network.Node],
                    links: Set[Network.Link],
                    outLinkMap: Map[Network.Node,Vector[Network.Link]],
                    inLinkMap: Map[Network.Node,Vector[Network.Link]],
                    directed: Boolean = false,
                    cachedShortestPaths: Option[Map[(Network.Node,Network.Node),(Seq[Network.Node],Seq[Network.Link],Double)]] = None
                  ){
  def percolate(percolationProba: Double,linkFilter: Network.Link=>Boolean= {l =>l.weight==0.0})(implicit rng: Random): Network = {
    val emptyLinks = links.toSeq.filter(linkFilter)
    val fullLinks = links.toSeq.filter{l => !linkFilter(l)}
    val percolated = emptyLinks.map{l => if(rng.nextDouble()<percolationProba){Network.Link(l.e1,l.e2,1.0)}else{Network.Link(l.e1,l.e2,0.0)}}
    val newlinks=fullLinks++percolated
    val newLinksSet = newlinks.toSet
    this.copy(links = newLinksSet)
  }

  def outgoingLinksOf(n: Network.Node): Vector[Network.Link] = outLinkMap(n)
}


object Network {

  case class Node(id: Int,
                  position: Point
                 ) {
    def x: Double = position._1
    def y: Double = position._2
    def <=(n2: Node): Boolean = x<n2.x||(x==n2.x&&y<=n2.y)
  }

  object Node {
    def apply(id: Int,x: Double,y: Double): Node = Node(id,(x,y))
  }

  case class Link(e1: Node,e2: Node,weight: Double,length: Double) {
    def oppositeOf(n: Node): Node = if(n==e1) e2 else if (n==e2) e1 else null
  }

  object Link {
    def apply(e1: Node,e2: Node,weight: Double): Link = {
      val d = math.sqrt((e1.x-e2.x)*(e1.x-e2.x)+(e1.y-e2.y)*(e1.y-e2.y))
      if (e1<=e2) Link(e1,e2,weight,d) else Link(e2,e1,weight,d)
    }
    def apply(e1: Node, e2: Node):Link = apply(e1,e2,1.0)
  }

  def apply(nodes: Set[Network.Node], links: Set[Network.Link]): Network = {
    val out = links.map(l => (l.e1,l)).groupBy(_._1).map(e => (e._1,e._2.toVector.map(_._2)))
    val in = links.map(l => (l.e2,l)).groupBy(_._1).map(e => (e._1,e._2.toVector.map(_._2)))
    Network(nodes, links, out, in)
  }

  def networkToGrid(network: Network,footPrintResolution: Double = 1.0,linkwidth: Double = 1.0): RasterLayerData[Double] = {
    val xmin = network.nodes.map{_.x}.min;val xmax = network.nodes.map{_.x}.max
    val ymin = network.nodes.map{_.y}.min;val ymax = network.nodes.map{_.y}.max
    def xcor(x: Double): Int = math.max(xmin.toDouble,math.min(xmax.toDouble,math.round(x))).toInt
    def ycor(y: Double): Int = math.max(ymin.toDouble,math.min(ymax.toDouble,math.round(y))).toInt
    val res: Array[Array[Double]] = (BigDecimal(xmin) to xmax by 1.0).toArray.map{ _ => (BigDecimal(ymin) to ymax by 1.0).toArray.map{ _ =>0.0}}
    network.links.toSeq.filter{_.weight>0.0}.foreach{ l =>
      val i1 = l.e1.x - xmin;val j1 = l.e1.y - ymin
      val i2 = l.e2.x - xmin;val j2 = l.e2.y - ymin
      val istep = i1 - i2 match {case xx if math.abs(xx) < 1e-10 => 0.0 ;case _ => math.cos(math.atan((j2 - j1)/(i2 - i1)))*footPrintResolution}
      val jstep = j1 - j2 match {case xx if math.abs(xx) < 1e-10 => 0.0 ;case _ => math.sin(math.atan((j2 - j1)/(i2 - i1)))*footPrintResolution}
      val nsteps = i1 - i2 match {case xx if math.abs(xx) < 1e-10 => (j2 - j1)/jstep;case _ => (i2 - i1)/istep}
      var x = l.e1.x;var y = l.e1.y
      (BigDecimal(0.0) to nsteps by 1.0).foreach{_ =>
        for {
          k1 <- - BigDecimal((linkwidth-1)/2) to (linkwidth-1)/2 by 1.0
          k2 <-  - BigDecimal((linkwidth-1)/2) to (linkwidth-1)/2 by 1.0
        } yield {
          res(xcor(x+k1.toDouble))(ycor(y+k2.toDouble)) = 1.0
        }

        x = x + istep;y = y+ jstep
      }
    }
    res
  }

  def gridToNetwork(world: Array[Array[Double]]): Network = {
    val links = new ArrayBuffer[Link]()
    var nodeid = 0
    for(i <- world.indices; j <- world.indices) {
      if(world(i)(j)>0.0){
        val currentnode = Node(nodeid,i,j);nodeid=nodeid+1
        if(i-1>0){if(world(i-1)(j)>0.0){nodeid=nodeid+1;links.append(Link(currentnode,Node(nodeid,i-1,j)))}}
        if(i+1<world.length){if(world(i+1)(j)>0.0){nodeid=nodeid+1;links.append(Link(currentnode,Node(nodeid,i+1,j)))}}
        if(j-1>0){if(world(i)(j-1)>0.0){nodeid=nodeid+1;links.append(Link(currentnode,Node(nodeid,i,j-1)))}}
        if(j+1<world(0).length){if(world(i)(j+1)>0.0){nodeid=nodeid+1;links.append(Link(currentnode,Node(nodeid,i,j+1)))}}
      }
    }
    Network(links.map{_.e1}.toSet.union(links.map{_.e2}.toSet),links.toSet)
  }


  def connectedComponentsTraverse(network: Network): Seq[Network] = {
    val nlinks = new mutable.HashMap[Node, Seq[Link]]()
    network.links.foreach { l =>
      if (nlinks.contains(l.e1)) {
        nlinks(l.e1) = nlinks(l.e1) ++ Seq(l)
      } else {
        nlinks(l.e1) = Seq(l)
      }
      if (nlinks.contains(l.e2)) {
        nlinks(l.e2) = nlinks(l.e2) ++ Seq(l)
      } else {
        nlinks(l.e2) = Seq(l)
      }
    }
    network.nodes.foreach { n => if (!nlinks.contains(n)) {
      nlinks(n) = Seq.empty
    }
    }

    //traverse using the map, using hash consing
    val totraverse = new mutable.HashMap[Node, Node]()
    network.nodes.foreach { n => totraverse.put(n, n) }
    val res = new ArrayBuffer[Network]()

    def otherend(n: Node, l: Link): Node = {
      if (l.e1 == n) l.e2 else l.e1
    }

    def traversenode(n: Node): (Seq[Node], Seq[Link]) = {
      if (!totraverse.contains(n)) {
        return (Seq.empty, nlinks(n))
      } // note : a bit redundancy on links here as they are not colored
      totraverse.remove(n)
      val traversed = nlinks(n).map { l => traversenode(otherend(n, l)) }
      (Seq(n) ++ traversed.flatMap(_._1), traversed.flatMap(_._2))
    }

    while (totraverse.nonEmpty) {
      val entry = totraverse.values.head
      val currentcomponent = traversenode(entry)
      res.append(Network(currentcomponent._1.toSet, currentcomponent._2.toSet))
    }
    res
  }

  def largestConnectedComponent(network: Network): Network = {
    val components = connectedComponentsTraverse(network)
    val largestComp = components.sortWith { case (n1, n2) => n1.nodes.size > n2.nodes.size }.head
    largestComp
  }


  def shortestPaths(network: Network,
                    from: Seq[Node],
                    to: Seq[Node]
                   ): ShortestPaths = new Math.GraphAlgorithms.DijkstraShortestPaths(network, from, to).getPaths



}
