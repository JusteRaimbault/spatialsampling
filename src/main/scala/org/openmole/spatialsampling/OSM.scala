package org.openmole.spatialsampling

import java.io._
import java.net._
import java.text._
import java.util.Date

import javax.xml.stream.{XMLInputFactory, XMLStreamConstants, XMLStreamException, XMLStreamReader}
import org.openmole.spatialsampling.Geometry.Polygon

import scala.beans.BeanProperty
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import scala.util.control.Breaks._

object OSM {

  val APIURL = "https://www.openstreetmap.org/api/0.6/"

  def get(west: Double, south: Double, east: Double, north: Double): OSMRoot = {
    val otherSymbols = new DecimalFormatSymbols
    otherSymbols.setDecimalSeparator('.')
    val df = new DecimalFormat("#.#######", otherSymbols)
    val bottom = df.format(south)
    val left = df.format(west)
    val top = df.format(north)
    val right = df.format(east)
    val url = APIURL+"map?bbox=" + left + "," + bottom + "," + right + "," + top

    val conn: HttpURLConnection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
    conn.setRequestMethod("GET")
    val rd: BufferedReader = new BufferedReader(new InputStreamReader(conn.getInputStream))
    val content: StringBuilder = new StringBuilder()
    try {
      var line = rd.readLine
      while (line != null) {
        content.append(line)
        line = rd.readLine
      }
    } finally rd.close()

    val root = new OSMRoot
    val parser = OSMXmlParser(root)
    parser.parse(content.toString())
    root
  }



  object Buildings {

    def asPolygonSeq(e: OSMRoot.Enumerator[OSMObject.Way]): Seq[Polygon] = {
      /*var result = scala.collection.mutable.Buffer[Polygon]()
      var way: OSMObject.Way = e.next
      while (way != null) {
        val building = way.getTag("building")
        if (building != null /* && building.equals("yes")*/ ) {
          val potentialPolygon = Try(Polygon(way))
          if (potentialPolygon.isSuccess) {
            result += potentialPolygon.get
          }
        }
        way = e.next
      }*/
      val result = Iterator.continually(Polygon(e.next)).takeWhile(_!=null).toSeq
      println("polygons: "+result.size)
      result
    }

    def getBuildings(west: Double, south: Double, east: Double, north: Double): Seq[Polygon] =
      asPolygonSeq(get(west, south, east, north).enumerateWays)

  }







  class OSMRoot extends Serializable {
    private var nodes:mutable.Map[Long, OSMObject.Node] = new mutable.HashMap[Long, OSMObject.Node]()
    private var ways:mutable.Map[Long, OSMObject.Way] = new mutable.HashMap[Long, OSMObject.Way]()
    private var relations:mutable.Map[Long, OSMObject.Relation] = new mutable.HashMap[Long, OSMObject.Relation]()

    def removeNode(identity: Long): OSMObject.Node = {
      val node = getNode(identity)
      remove(node)
      node
    }

    def removeWay(identity: Long):OSMObject.Way = {
      val way = getWay(identity)
      remove(way)
      way
    }

    def removeRelation(identity: Long): OSMObject.Relation = {
      val relation = getRelation(identity)
      remove(relation)
      relation
    }

    def enumerateNodes: OSMRoot.Enumerator[OSMObject.Node] = new OSMRoot.Enumerator[OSMObject.Node]() {
      val iterator: Iterator[(Long,OSMObject.Node)] = getNodes.iterator
      override def next: OSMObject.Node = if (iterator.hasNext) iterator.next._2 else null
    }

    def enumerateWays: OSMRoot.Enumerator[OSMObject.Way] =
      new OSMRoot.Enumerator[OSMObject.Way]() {
        val iterator: Iterator[(Long,OSMObject.Way)] = getWays.iterator
        override def next: OSMObject.Way = if (iterator.hasNext) iterator.next._2 else null
      }

    def enumerateRelations: OSMRoot.Enumerator[OSMObject.Relation] = new OSMRoot.Enumerator[OSMObject.Relation]() {
      val iterator: Iterator[(Long,OSMObject.Relation)] = getRelations.iterator
      override def next: OSMObject.Relation = if (iterator.hasNext) iterator.next._2 else null
    }

    def getNode(identity: Long): OSMObject.Node = getNodes.getOrElse(identity,null)

    def getWay(identity: Long): OSMObject.Way = getWays.getOrElse(identity,null)

    def getRelation(identity: Long): OSMObject.Relation = getRelations.getOrElse(identity,null)

    def remove(`object`: OSMObject): java.util.Set[OSMObject] = {
      val affectedRelations = `object`.accept(removeVisitor)
      affectedRelations
    }

    private val removeVisitor = new RemoveVisitor

    class RemoveVisitor extends OSMObject.OSMObjectVisitor[java.util.Set[OSMObject]] with Serializable {
      override def visit(node: OSMObject.Node): java.util.HashSet[OSMObject] = {
        val affectedRelations = new java.util.HashSet[OSMObject](1024)
        if (node.getWaysMemberships != null) {
          for (way <- node.getWaysMemberships) { // need to loop in case we visit this node multiple times, eg a polygon where this is start and stop
            while ( {
              way.getNodes.contains(node)
            }) way.getNodes.remove(way.getNodes.indexOf(node))
            affectedRelations.add(way)
          }
        }
        node.setWaysMemberships(null)
        if (node.getRelationMemberships != null) {
          for (member <- node.getRelationMemberships) {
            member.getRelation.getMembers.remove(member.getRelation.getMembers.indexOf(member))
            affectedRelations.add(member.getRelation)
          }
        }
        node.setRelationMemberships(null)
        OSMRoot.this.getNodes.remove(node.getId)
        affectedRelations
      }

      override def visit(way: OSMObject.Way): java.util.Set[OSMObject] = {
        val affectedRelations = new java.util.HashSet[OSMObject](1024)
        if (way.getNodes != null) {
          for (node <- way.getNodes) {
            node.getWaysMemberships.remove(node.getWaysMemberships.indexOf(way))
            affectedRelations.add(node)
          }
          way.setNodes(null)
        }
        if (way.getRelationMemberships != null) {
          for (member <- way.getRelationMemberships) {
            member.getRelation.getMembers.remove(member.getRelation.getMembers.indexOf(member))
            affectedRelations.add(member.getRelation)
          }
          way.setRelationMemberships(null)
        }
        affectedRelations
      }

      override def visit(relation: OSMObject.Relation): java.util.Set[OSMObject] = {
        val affectedRelations = new java.util.HashSet[OSMObject](1024)
        if (relation.getMembers != null) {
          for (member <- relation.getMembers) {
            member.getObject.getRelationMemberships.remove(member.getObject.getRelationMemberships.indexOf(member))
            if (member.getObject.getRelationMemberships.isEmpty) {
              member.getObject.setRelationMemberships(null)
              affectedRelations.add(member.getObject)
            }
          }
          relation.setMembers(null)
        }
        affectedRelations
      }
    }

    @SerialVersionUID(1L)
    class AddVisitor extends OSMObject.OSMObjectVisitor[Void] with Serializable {
      override def visit(node: OSMObject.Node): Null = {
        getNodes.put(node.getId, node)
        null
      }

      override def visit(way: OSMObject.Way): Null = {
        getWays.put(way.getId, way)
        null
      }

      override def visit(relation: OSMObject.Relation): Null = {
        getRelations.put(relation.getId, relation)
        null
      }
    }

    private val addVisitor = new AddVisitor

    def add(osmObject: OSMObject): Unit = {
      osmObject.accept(addVisitor)
    }

    def getNodes: mutable.Map[Long, OSMObject.Node] = nodes

    def setNodes(nodes: mutable.Map[Long, OSMObject.Node]): Unit = {
      this.nodes = nodes
    }

    def getWays: mutable.Map[Long, OSMObject.Way] = ways

    def setWays(ways: mutable.Map[Long, OSMObject.Way]): Unit = {
      this.ways = ways
    }

    def getRelations: mutable.Map[Long, OSMObject.Relation] = relations

    def setRelations(relations: mutable.Map[Long, OSMObject.Relation]): Unit = {
      this.relations = relations
    }

    /**
      * @param f returns true if instance is to be removed from results
      * @return
      */
    def filter(f: OSMObject.OSMObjectVisitor[Boolean]): Iterable[OSMObject] = filter(gatherAllOsmObjects, f)

    def filter(input: Iterable[OSMObject], filter: OSMObject.OSMObjectVisitor[Boolean]): Iterable[OSMObject] =
      input.filter(!_.accept(filter))

    def gatherAllOsmObjects: mutable.HashSet[OSMObject] = {
      val objects = new mutable.HashSet[OSMObject]
      getWays.values.foreach(objects.add(_))
      getRelations.values.foreach(objects.add(_))
      getNodes.values.foreach(objects.add(_))
      objects
    }

    def findNodeByLatitudeAndLongitude(latitude: Double, longitude: Double): mutable.ArrayBuffer[OSMObject.Node] = {
      val nodes = new mutable.ArrayBuffer[OSMObject.Node](100)
      for (node <- getNodes.values) {
        if (node.getLatitude == latitude && node.getLongitude == longitude) nodes.append(node)
      }
      nodes
    }

  }

  object OSMRoot {
    abstract class Enumerator[T] {
      def next: T
    }
  }




  abstract class OSMObject extends Serializable {

    def accept[R](visitor: OSMObject.OSMObjectVisitor[R]): R

    private var loaded = false
    protected var id = 0L
    private var attributes: mutable.Map[String, String] = _
    private var version:Integer = _
    private var changeset = 0L
    private var uid = 0L
    private var user:String = _
    private var visible = false
    private var timestamp = 0L
    private var tags: mutable.Map[String, String] = _
    private var relationMemberships: mutable.ArrayBuffer[OSMObject.RelationMembership] = _

    def addRelationMembership(member: OSMObject.RelationMembership): Unit = {
      if (relationMemberships == null) relationMemberships = new ArrayBuffer[OSMObject.RelationMembership](5)
      else { // don't add membership to the same object twice
        for (relationMembership <- relationMemberships) {
          if (relationMembership.getRelation == member.getRelation) return
        }
      }
      relationMemberships.append(member)
    }

    def getAttributes: mutable.Map[String, String] = attributes

    def setAttributes(attributes: mutable.Map[String, String]): Unit = {
      this.attributes = attributes
    }

    def getRelationMemberships: mutable.ArrayBuffer[OSMObject.RelationMembership] = relationMemberships

    def setRelationMemberships(relationMemberships: mutable.ArrayBuffer[OSMObject.RelationMembership]): Unit = {
      this.relationMemberships = relationMemberships
    }

    def getAttribute(key: String): String = {
      if (attributes == null) return null
      attributes.getOrElse(key,null)
    }

    def setAttribute(key: String, value: String): Option[String] = {
      if (attributes == null) attributes = new mutable.HashMap[String, String]
      attributes.put(key, value)
    }

    def getTag(key: String): String = {
      if (tags == null) return null
      tags.getOrElse(key, null)
    }

    def setTag(key: String, value: String): Option[String] = {
      if (tags == null) {
        tags = new mutable.HashMap[String, String]
      }
      tags.put(key, value)
    }

    def getVersion: Integer = version

    def setVersion(version: Integer): Unit = {
      this.version = version
    }

    def getChangeset: Long = changeset

    def setChangeset(changeset: Long): Unit = {
      this.changeset = changeset
    }

    def getUser: String = user

    def setUser(user: String): Unit = {
      this.user = user
    }

    def isVisible: Boolean = visible

    def setVisible(visible: Boolean): Unit = {
      this.visible = visible
    }

    def getId: Long = id

    def setId(id: Long): Unit = {
      this.id = id
    }

    def getTags: mutable.Map[String, String] = tags

    def setTags(tags: mutable.Map[String, String]): Unit = {
      this.tags = tags
    }

    def getTimestamp: Long = timestamp

    def setTimestamp(timestamp: Long): Unit = {
      this.timestamp = timestamp
    }

    def getUid: Long = uid

    def setUid(uid: Long): Unit = {
      this.uid = uid
    }

    def isLoaded: Boolean = loaded

    def setLoaded(loaded: Boolean): Unit = {
      this.loaded = loaded
    }

    override def toString: String = "OsmObject{" + "loaded=" + loaded + ", id=" + id + ", attributes=" + attributes + ", version=" + version + ", changeset=" + changeset + ", uid=" + uid + ", user='" + user + '\'' + ", visible=" + visible + ", timestamp=" + timestamp + ", tags=" + tags + ", relationMemberships=" + relationMemberships + '}'
  }


  object OSMObject {

    trait OSMObjectVisitor[R] {
      def visit(node: Node): R

      def visit(way: Way): R

      def visit(relation: Relation): R
    }

    class Node() extends OSMObject with Serializable {
      def accept[R](visitor: OSMObject.OSMObjectVisitor[R]): R = visitor.visit(this)

      def this(id: Long, latitude: Double, longitude: Double, tags: String*) {
        this()
        this.latitude = latitude
        this.longitude = longitude
        setId(id)
        var i = 0
        while ( {
          i < tags.length
        }) {
          setTag(tags(i), tags(i + 1))

          i += 2
        }
      }

      private var latitude = .0
      private var longitude = .0
      private var waysMemberships: mutable.ArrayBuffer[Way] = _

      def addWayMembership(way: Way): Unit = {
        if (waysMemberships == null) waysMemberships = new mutable.ArrayBuffer[Way](5)
        else { // don't add membership to the same way twice
          // this happens for instance when this is the start and stop in a polygon.
          for (wayMembership <- waysMemberships) {
            if (way == wayMembership) return
          }
        }
        waysMemberships.append(way)
      }

      def getWaysMemberships: mutable.ArrayBuffer[Way] = waysMemberships

      def setWaysMemberships(waysMemberships: mutable.ArrayBuffer[Way]): Unit = {
        this.waysMemberships = waysMemberships
      }

      def getLatitude: Double = latitude

      def setLatitude(latitude: Double): Unit = {
        this.latitude = latitude
      }

      def getLongitude: Double = longitude

      def setLongitude(longitude: Double): Unit = {
        this.longitude = longitude
      }

      def getY: Double = getLatitude

      def setY(latitude: Double): Unit = {
        setLatitude(latitude)
      }

      def getX: Double = getLongitude

      def setX(longitude: Double): Unit = {
        setLongitude(longitude)
      }

      override def toString: String = "Node{" + super.toString + "latitude=" + latitude + ", longitude=" + longitude + ", waysMemberships.size=" + (if (waysMemberships == null) "null"
      else waysMemberships.size) + '}'
    }


    class Way extends OSMObject with Serializable {

      override def accept[R](visitor: OSMObject.OSMObjectVisitor[R]): R = visitor.visit(this)

      def Way(id: Long): Unit = {
        this.id = id
      }

      @BeanProperty
      var nodes: mutable.ArrayBuffer[Node] = _

      /**
        * @return true if an enclosed polygon
        */
      def isPolygon: Boolean = {
        if (!isLoaded) {
          throw new NotLoadedException(this)
        }
        getNodes.size > 2 && getNodes()(0) == getNodes()(getNodes.size - 1)
      }

      def addNode(node: Node): Way = {
        if (nodes == null) {
          nodes = new mutable.ArrayBuffer[Node](50)
        }
        nodes.append(node)
        this
      }

      override def toString: String = "Way{" + super.toString + "nodes.size=" + (if (nodes == null) "null" else nodes.size) + '}'
    }


    class Relation extends OSMObject with Serializable {
      override def accept[R](visitor: OSMObject.OSMObjectVisitor[R]): R = visitor.visit(this)

      private var members: mutable.ArrayBuffer[RelationMembership] = _

      def addMember(member: RelationMembership): mutable.ArrayBuffer[RelationMembership] = {
        if (members == null) members = new mutable.ArrayBuffer[RelationMembership](50)
        members.append(member)
        members
      }

      def getMembers: mutable.ArrayBuffer[RelationMembership] = members

      def setMembers(members: mutable.ArrayBuffer[RelationMembership]): Unit = {
        this.members = members
      }

      override def toString: String = "Relation{" + super.toString + "members=" + members + '}'
    }


    class RelationMembership extends Serializable {
      @BeanProperty
      var relation: Relation = _
      private var `object`: OSMObject = _

      private var role: String = _

      def getObject: OSMObject = `object`

      def setObject(`object`: OSMObject): Unit = {
        this.`object` = `object`
      }

      def getRole: String = role

      def setRole(role: String): Unit = {
        this.role = role
      }

      override def toString: String = "RelationMembership{" + "role='" + role + '\'' + ", relation.id=" + (if (relation != null) relation.getId
      else "null") + ", object.id=" + (if (`object` != null) `object`.getId
      else "null") + '}'
    }

    class NotLoadedException(detailMessage: String, throwable: Throwable) extends java.lang.RuntimeException(detailMessage, throwable) {
      def this(`object`: OSMObject) {
        this(`object`.getClass.getSimpleName + "#id " + `object`.getId + " is not loaded!", null)
      }
      def this(detailMessage: String) {
        this(detailMessage, null)
      }
    }


  }



  case class OSMXmlParser(
                           root: OSMRoot,
                           allowingMissingVersions: Boolean = true,
                           timestampFormat: OSMXmlParser.OsmXmlTimestampFormat = new OSMXmlParser.OsmXmlTimestampFormat,
                           tagKeyIntern: OSMXmlParser.HashConsing[String] = new OSMXmlParser.HashConsing[String],
                           tagValueIntern: OSMXmlParser.HashConsing[String] = new OSMXmlParser.HashConsing[String],
                           userIntern: OSMXmlParser.HashConsing[String] = new OSMXmlParser.HashConsing[String],
                           roleIntern: OSMXmlParser.HashConsing[String] = new OSMXmlParser.HashConsing[String]
                         ) {

    final def parse(xml: String):OSMXmlParser.OsmXmlParserDelta = parse(new StringReader(xml))

    def parse(xml: InputStream):OSMXmlParser.OsmXmlParserDelta = parse(new InputStreamReader(xml, "utf8"))

    def processParsedNode(node: OSMObject.Node, state: OSMXmlParser.State.Value): Unit = {}

    def processParsedWay(way: OSMObject.Way, state: OSMXmlParser.State.Value): Unit = {}

    def processParsedRelation(relation: OSMObject.Relation, state: OSMXmlParser.State.Value): Unit = {}


    def readerFactory(xml: InputStream):  OSMXmlParser.Stream  = readerFactory(new InputStreamReader(xml, "utf8"))

    def parse(xml: Reader): OSMXmlParser.OsmXmlParserDelta = {
      val started = System.currentTimeMillis
      val delta = OSMXmlParser.OsmXmlParserDelta()
      try {
        val xmlr:  OSMXmlParser.Stream  = readerFactory(xml)
        var current: OSMObject = null
        var currentNode: OSMObject.Node = null
        var currentRelation: OSMObject.Relation = null
        var currentWay: OSMObject.Way = null
        var skipCurrentObject = false
        var state: OSMXmlParser.State.Value = OSMXmlParser.State.none
        var eventType: Int = xmlr.next // START_DOCUMENT
        while (!xmlr.isEndDocument(eventType)) {
          breakable {
            if (xmlr.isStartElement(eventType)) {
              if ("create" == xmlr.getLocalName) state = OSMXmlParser.State.create
              else if ("modify" == xmlr.getLocalName) state = OSMXmlParser.State.modify
              else if ("delete" == xmlr.getLocalName) state = OSMXmlParser.State.delete
              else if ("node" == xmlr.getLocalName) {
                val identity = xmlr.getAttributeValue(null, "id").toLong
                if ((state == OSMXmlParser.State.none) || (state == OSMXmlParser.State.create)) {
                  currentNode = root.getNode(identity)
                  if (currentNode != null && currentNode.isLoaded && currentNode.getVersion != null) {
                    val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                    if (version <= currentNode.getVersion) {
                      skipCurrentObject = true
                      break //was:continue
                      //              } else if (version > currentNode.getVersion() + 1) {
                      //                throw new OsmXmlParserException("Inconsistency, too great version found during create node.");
                    }
                    else throw new OSMXmlParser.OsmXmlParserException("Inconsistency, node " + identity + " already exists.")
                  }
                  if (currentNode == null) {
                    currentNode = new OSMObject.Node
                    currentNode.setId(identity)
                  }
                  currentNode.setLatitude(xmlr.getAttributeValue(null, "lat").toDouble)
                  currentNode.setLongitude(xmlr.getAttributeValue(null, "lon").toDouble)
                  parseObjectAttributes(xmlr, currentNode, "id", "lat", "lon")
                  currentNode.setLoaded(true)
                  current = currentNode
                  delta.createdNodes.add(currentNode)
                  root.add(currentNode)
                }
                else if (state == OSMXmlParser.State.modify) {
                  currentNode = root.getNode(identity)
                  if (currentNode == null) throw new OSMXmlParser.OsmXmlParserException("Inconsistency, node " + identity + " does not exists.")
                  val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                  if (version <= currentNode.getVersion) {
                    log("Inconsistency, old version detected during modify node.")
                    skipCurrentObject = true
                    break //was:continue
                  }
                  else if (version > currentNode.getVersion + 1 && !allowingMissingVersions) throw new OSMXmlParser.OsmXmlParserException("Inconsistency, version " + version + " too great to modify node " + currentNode.getId + " with version " + currentNode.getVersion)
                  else if (version == currentNode.getVersion) throw new OSMXmlParser.OsmXmlParserException("Inconsistency, found same version in new data during modify node.")
                  currentNode.setTags(null)
                  currentNode.setAttributes(null)
                  currentNode.setLatitude(xmlr.getAttributeValue(null, "lat").toDouble)
                  currentNode.setLongitude(xmlr.getAttributeValue(null, "lon").toDouble)
                  parseObjectAttributes(xmlr, currentNode, "id", "lat", "lon")
                  current = currentNode
                  delta.modifiedNodes.add(currentNode)
                  root.add(currentNode)
                }
                else if (state == OSMXmlParser.State.delete) {
                  val nodeToRemove = root.getNode(identity)
                  if (nodeToRemove == null) {
                    log("Inconsistency, node " + identity + " does not exists.")
                    skipCurrentObject = true
                    break //was:continue
                  }
                  val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                  if (version < nodeToRemove.getVersion) {
                    log("Inconsistency, old version detected during delete node.")
                    skipCurrentObject = true
                    break //was:continue
                  }
                  else if (version > nodeToRemove.getVersion + 1 && !allowingMissingVersions) throw new OSMXmlParser.OsmXmlParserException("Inconsistency, too great version found during delete node.")
                  root.remove(nodeToRemove)
                  delta.deletedNodes.add(nodeToRemove)
                }
              }
              else if ("way" == xmlr.getLocalName) {
                val identity = xmlr.getAttributeValue(null, "id").toLong
                if ((state == OSMXmlParser.State.none) || (state == OSMXmlParser.State.create)) {
                  currentWay = root.getWay(identity)
                  if (currentWay != null && currentWay.isLoaded && currentWay.getVersion != null) {
                    val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                    if (version <= currentWay.getVersion) {
                      log("Inconsistency, old version detected during create way.")
                      skipCurrentObject = true
                      break //was:continue
                      //              } else if (version > currentWay.getVersion() + 1) {
                      //                throw new OsmXmlParserException("Inconsistency, too great version found during create way.");
                    }
                    else throw new OSMXmlParser.OsmXmlParserException("Inconsistency, way " + identity + " already exists.")
                  }
                  if (currentWay == null) {
                    currentWay = new OSMObject.Way
                    currentWay.setId(identity)
                  }
                  parseObjectAttributes(xmlr, currentWay, "id")
                  currentWay.setLoaded(true)
                  current = currentWay
                  delta.createdWays.add(currentWay)
                  root.add(currentWay)// added line
                }
                else if (state == OSMXmlParser.State.modify) {
                  currentWay = root.getWay(identity)
                  if (currentWay == null) throw new OSMXmlParser.OsmXmlParserException("Inconsistency, way " + identity + " does not exists.")
                  val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                  if (version <= currentWay.getVersion) {
                    println("Inconsistency, old version detected during modify way.")
                    skipCurrentObject = true
                    break //was:continue
                  }
                  else if (version > currentWay.getVersion + 1 && !allowingMissingVersions) throw new OSMXmlParser.OsmXmlParserException("Inconsistency, found too great version in new data during modify way.")
                  else if (version == currentWay.getVersion) throw new OSMXmlParser.OsmXmlParserException("Inconsistency, found same version in new data during modify way.")
                  //                currentWay.setTags(null)
                  currentWay.setAttributes(null)
                  if (currentWay.getNodes != null) {
                    for (node <- currentWay.getNodes) {
                      node.getWaysMemberships.remove(node.getWaysMemberships.indexOf(currentWay))
                      root.add(node)
                    }
                  }
                  currentWay.setNodes(null)
                  parseObjectAttributes(xmlr, currentWay, "id")
                  current = currentWay
                  delta.modifiedWays.add(currentWay)
                }
                else if (state == OSMXmlParser.State.delete) {
                  val wayToRemove = root.getWay(identity)
                  if (wayToRemove == null) {
                    println("Inconsistency, way \" + identity + \" does not exists.")
                    skipCurrentObject = true
                    break //was:continue
                  }
                  val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                  if (version < wayToRemove.getVersion) {
                    println("Inconsistency, old version detected during delete way.")
                    skipCurrentObject = true
                    break //was:continue
                  }
                  else if (version > wayToRemove.getVersion + 1 && !allowingMissingVersions) throw new OSMXmlParser.OsmXmlParserException("Inconsistency, too great way version found during delete way.")
                  root.remove(wayToRemove)
                  delta.deletedWays.add(wayToRemove)
                }
              }
              else if ("nd" == xmlr.getLocalName) { // a node reference inside of a way
                if (skipCurrentObject) break //was:continue
                //! continue is not supported
                val identity = xmlr.getAttributeValue(null, "ref").toLong
                if ((state == OSMXmlParser.State.none) || (state == OSMXmlParser.State.create) || (state == OSMXmlParser.State.modify)) {
                  var node = root.getNode(identity)
                  if (node == null) {
                    node = new OSMObject.Node
                    node.setId(identity)
                    root.add(node)
                  }
                  node.addWayMembership(currentWay)
                  currentWay.addNode(node)
                }
                else if (state == OSMXmlParser.State.delete) {
                  //throw new OsmXmlParserException("Lexical error, delete way should not contain <nd> elements.");
                }
              }
              else if ("relation" == xmlr.getLocalName) {
                // multi polygon, etc
                val identity = xmlr.getAttributeValue(null, "id").toLong
                if ((state == OSMXmlParser.State.none) || (state == OSMXmlParser.State.create)) {
                  currentRelation = root.getRelation(identity)
                  if (currentRelation != null && currentRelation.isLoaded && currentRelation.getVersion != null) {
                    val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                    if (version <= currentRelation.getVersion) {
                      println("Inconsistency, old version detected during create relation.")
                      skipCurrentObject = true
                      break //was:continue
                      //              } else if (version > currentRelation.getVersion() + 1) {
                      //                throw new OsmXmlParserException("Inconsistency, too great version found during create relation.");
                    }
                    else throw new OSMXmlParser.OsmXmlParserException("Inconsistency, relation " + identity + " already exists.")
                  }
                  if (currentRelation == null) {
                    currentRelation = new OSMObject.Relation
                    currentRelation.setId(identity)
                  }
                  parseObjectAttributes(xmlr, currentRelation, "id")
                  currentRelation.setLoaded(true)
                  current = currentRelation
                  delta.createdRelations.add(currentRelation)
                }
                else if (state == OSMXmlParser.State.modify) {
                  currentRelation = root.getRelation(identity)
                  if (currentRelation == null) throw new OSMXmlParser.OsmXmlParserException("Inconsistency, relation " + identity + " does not exists.")
                  val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                  if (version < currentRelation.getVersion) {
                    log("Inconsistency, old version detected during modify relation.")
                    skipCurrentObject = true
                    break //was:continue
                  }
                  else if (version > currentRelation.getVersion + 1 && !allowingMissingVersions) throw new OSMXmlParser.OsmXmlParserException("Inconsistency, too great version found during modify relation.")
                  else if (version == currentRelation.getVersion) throw new OSMXmlParser.OsmXmlParserException("Inconsistency, same version found during modify relation.")
                  if (currentRelation.getMembers != null) {

                    for (member <- currentRelation.getMembers) {
                      member.getObject.getRelationMemberships.remove(member.getObject.getRelationMemberships.indexOf(member))
                      if (member.getObject.getRelationMemberships.isEmpty) member.getObject.setRelationMemberships(null)
                    }
                    currentRelation.setMembers(null)
                  }
                  currentRelation.setAttributes(null)
                  currentRelation.setTags(null)
                  current = currentRelation
                  parseObjectAttributes(xmlr, currentRelation, "id")
                  delta.modifiedRelations.add(currentRelation)
                }
                else if (state == OSMXmlParser.State.delete) {
                  val relationToRemove = root.getRelation(identity)
                  if (relationToRemove == null) {
                    log("Inconsistency, relation \" + identity + \" does not exist.")
                    skipCurrentObject = true
                    break //was:continue
                  }
                  val version = Integer.valueOf(xmlr.getAttributeValue(null, "version"))
                  if (version < relationToRemove.getVersion) {
                    log("Inconsistency, old version detected during delete relation.")
                    skipCurrentObject = true
                    break //was:continue
                  }
                  else if (version > relationToRemove.getVersion + 1 && !allowingMissingVersions) throw new OSMXmlParser.OsmXmlParserException("Inconsistency, too great version found during delete relation.")
                  if (relationToRemove.getMembers != null) {
                    for (member <- relationToRemove.getMembers) {
                      member.getObject.getRelationMemberships.remove(member.getObject.getRelationMemberships.indexOf(member))
                      if (member.getObject.getRelationMemberships.isEmpty) member.getObject.setRelationMemberships(null)
                    }
                    relationToRemove.setMembers(null)
                  }
                  root.remove(relationToRemove)
                  delta.deletedRelations.add(relationToRemove)
                }
              }
              else if ("member" == xmlr.getLocalName) { // multi polygon member
                if (skipCurrentObject) break //was:continue
                if ((state == OSMXmlParser.State.none) || (state == OSMXmlParser.State.create) || (state == OSMXmlParser.State.modify)) {
                  val member = new OSMObject.RelationMembership
                  member.setRelation(currentRelation)
                  member.setRole(roleIntern.intern(xmlr.getAttributeValue(null, "role")))
                  val identity = xmlr.getAttributeValue(null, "ref").toLong
                  val `type` = xmlr.getAttributeValue(null, "type")
                  if ("way" == `type`) {
                    var way = root.getWay(identity)
                    if (way == null) {
                      way = new OSMObject.Way
                      way.setId(identity)
                      root.add(way)
                    }
                    member.setObject(way)
                  }
                  else if ("node" == `type`) {
                    var node = root.getNode(identity)
                    if (node == null) {
                      node = new OSMObject.Node
                      node.setId(identity)
                      root.add(node)
                    }
                    member.setObject(node)
                  }
                  else if ("relation" == `type`) {
                    var relation = root.getRelation(identity)
                    if (relation == null) {
                      relation = new OSMObject.Relation
                      relation.setId(identity)
                      root.add(relation)
                    }
                    member.setObject(relation)
                  }
                  else throw new RuntimeException("Unsupported relation member type: " + `type`)
                  member.getObject.addRelationMembership(member)
                  currentRelation.addMember(member)
                }
                else if (state == OSMXmlParser.State.delete) {
                  //throw new OsmXmlParserException("Lexical error, delete relation should not contain <member> elements.");
                }
              }
              else if ("tag" == xmlr.getLocalName) { // tag of any object type
                if (skipCurrentObject) break //was:continue
                if ((state == OSMXmlParser.State.none) || (state == OSMXmlParser.State.create) || (state == OSMXmlParser.State.modify)) {
                  val key = tagKeyIntern.intern(xmlr.getAttributeValue(null, "k"))
                  val value = tagValueIntern.intern(xmlr.getAttributeValue(null, "v"))
                  current.setTag(key, value)
                }
                else if (state == OSMXmlParser.State.delete) {
                  //throw new OsmXmlParserException("Lexical error, delete object should not contain <tag> elements.");
                }
              }
              else if (xmlr.isEndElement(eventType)) if ("create" == xmlr.getLocalName) state = OSMXmlParser.State.none
              else if ("modify" == xmlr.getLocalName) state = OSMXmlParser.State.none
              else if ("delete" == xmlr.getLocalName) state = OSMXmlParser.State.none
              else if ("node" == xmlr.getLocalName) {
                if ((state == OSMXmlParser.State.none) || (state == OSMXmlParser.State.create) || (state == OSMXmlParser.State.modify)) root.add(currentNode)
                processParsedNode(currentNode, state)
                currentNode = null
                current = null
                skipCurrentObject = false
              }
              else if ("way" == xmlr.getLocalName) {
                if ((state == OSMXmlParser.State.none) || (state == OSMXmlParser.State.create) || (state == OSMXmlParser.State.modify)) root.add(currentWay)
                processParsedWay(currentWay, state)
                currentWay = null
                current = null
                skipCurrentObject = false
              }
              else if ("relation" == xmlr.getLocalName) {
                if ((state == OSMXmlParser.State.none) || (state == OSMXmlParser.State.create) || (state == OSMXmlParser.State.modify)) root.add(currentRelation)
                processParsedRelation(currentRelation, state)
                currentRelation = null
                current = null
                skipCurrentObject = false
              }
              else {
                // what not
              }
            }
          }
          eventType = xmlr.next
        }
        xmlr.close()
      } catch {
        case ioe: OSMXmlParser.StreamException =>
          throw new OSMXmlParser.OsmXmlParserException(ioe)
      }
      val timespent = System.currentTimeMillis - started
      log(s"time spent = $timespent ms")
      delta
    }

    @throws[OSMXmlParser.StreamException]
    private def parseObjectAttributes(xmlr: OSMXmlParser.Stream, osmObject: OSMObject, parsedAttributes: String*): Unit = {
      var attributeIndex = 0
      while ( {
        attributeIndex < xmlr.getAttributeCount
      }) {
        val key = xmlr.getAttributeLocalName(attributeIndex)
        val value = xmlr.getAttributeValue(attributeIndex)
        if ("version" == key) osmObject.setVersion(Integer.valueOf(value))
        else if ("changeset" == key) osmObject.setChangeset(value.toLong)
        else if ("uid" == key) osmObject.setUid(value.toLong)
        else if ("user" == key) osmObject.setUser(userIntern.intern(value))
        else if ("visible" == key) osmObject.setVisible(value.toBoolean)
        else if ("timestamp" == key) try {
          osmObject.setTimestamp(timestampFormat.parse(value).getTime)
        } catch {
          case pe: Exception =>
            throw new RuntimeException(pe)
        }
        else {
          var parsed = false
          breakable {
            for (parsedAttribute <- parsedAttributes) {
              if (parsedAttribute == key) {
                parsed = true
                break
              }
            }
          }
          if (!parsed) {
            osmObject.setAttribute(key, value)
          }
        }
        {
          attributeIndex += 1; attributeIndex - 1
        }
      }
    }



    private val xmlif = XMLInputFactory.newInstance

    @throws[OSMXmlParser.StreamException]
    def readerFactory(xml: Reader):  OSMXmlParser.Stream = {
      var xmlr: XMLStreamReader = null
      try
        xmlr = xmlif.createXMLStreamReader(xml)
      catch {
        case e: XMLStreamException =>
          throw new OSMXmlParser.StreamException(e)
      }
      new  OSMXmlParser.Stream  {
        @throws[OSMXmlParser.StreamException]
        def getEventType: Int = xmlr.getEventType

        @throws[OSMXmlParser.StreamException]
        def isEndDocument(eventType: Int): Boolean = eventType == XMLStreamConstants.END_DOCUMENT

        @throws[OSMXmlParser.StreamException]
        def next: Int = try
          xmlr.next
        catch {
          case e: XMLStreamException =>
            throw new OSMXmlParser.StreamException(e)
        }

        @throws[OSMXmlParser.StreamException]
        def isStartElement(eventType: Int): Boolean = eventType == XMLStreamConstants.START_ELEMENT

        @throws[OSMXmlParser.StreamException]
        def isEndElement(eventType: Int): Boolean = eventType == XMLStreamConstants.END_ELEMENT

        @throws[OSMXmlParser.StreamException]
        def getLocalName: String = xmlr.getLocalName

        @throws[OSMXmlParser.StreamException]
        def getAttributeValue(what: String, key: String): String = xmlr.getAttributeValue(what, key)

        @throws[OSMXmlParser.StreamException]
        def getAttributeCount: Int = xmlr.getAttributeCount

        @throws[OSMXmlParser.StreamException]
        def getAttributeValue(index: Int): String = xmlr.getAttributeValue(index)

        @throws[OSMXmlParser.StreamException]
        def getAttributeLocalName(index: Int): String = xmlr.getAttributeLocalName(index)

        @throws[OSMXmlParser.StreamException]
        def close(): Unit = {
          try
            xmlr.close()
          catch {
            case e: XMLStreamException =>
              throw new OSMXmlParser.StreamException(e)
          }
        }
      }
    }



  }



  /**
    * OSM data parser
    */
  object OSMXmlParser {


    class HashConsing[T] extends Serializable {
      private val map = new mutable.HashMap[T, T]()

      def intern(obj: T): T = {
        map.get(obj) match {
          case None =>
            map.put(obj, obj)
            obj
          case Some(t) => t
        }
      }
    }

    object State extends Enumeration {
      type State = Value
      val none, create, modify, delete = Value
    }

    /**
      * formats for xml timestamp
      */
    class OsmXmlTimestampFormat extends DateFormat {

      private val format1 = "yyyy-MM-dd'T'HH:mm:ss'Z'"
      private val format2 = "yyyy-MM-dd'T'HH:mm:ss"

      private val implementation1 = new SimpleDateFormat(format1)
      private val implementation2 = new SimpleDateFormat(format2)

      override def format(date: Date, stringBuffer: StringBuffer, fieldPosition: FieldPosition): StringBuffer = implementation1.format(date, stringBuffer, fieldPosition)

      override def parse(s: String, parsePosition: ParsePosition): Date = {
        if (s.length - parsePosition.getIndex == format1.length)
          return implementation1.parse(s, parsePosition)
        implementation2.parse(s, parsePosition)
      }
    }

    case class OsmXmlParserDelta(
                                  createdNodes: mutable.Set[OSMObject.Node] = new mutable.HashSet[OSMObject.Node],
                                  modifiedNodes: mutable.Set[OSMObject.Node] = new mutable.HashSet[OSMObject.Node],
                                  deletedNodes: mutable.Set[OSMObject.Node] = new mutable.HashSet[OSMObject.Node],
                                  createdWays: mutable.Set[OSMObject.Way] = new mutable.HashSet[OSMObject.Way],
                                  modifiedWays: mutable.Set[OSMObject.Way] = new mutable.HashSet[OSMObject.Way],
                                  deletedWays: mutable.Set[OSMObject.Way] = new mutable.HashSet[OSMObject.Way],
                                  createdRelations: mutable.Set[OSMObject.Relation] = new mutable.HashSet[OSMObject.Relation],
                                  modifiedRelations: mutable.Set[OSMObject.Relation] = new mutable.HashSet[OSMObject.Relation],
                                  deletedRelations: mutable.Set[OSMObject.Relation] = new mutable.HashSet[OSMObject.Relation]
                                )

    /**
      * parsing exception
      * @param s message
      * @param throwable exception
      */
    class OsmXmlParserException(s: String, throwable: Throwable) extends Exception(s, throwable) {
      def this(s: String) {
        this(s, null)
      }
      def this(throwable: Throwable) {
        this("", throwable)
      }
    }



    class StreamException(message: String, cause: Throwable) extends Exception(message, cause) {
      def this(message: String) {
        this(message, null)
      }

      def this(cause: Throwable) {
        this("", cause)
      }
    }

    abstract class Stream {
      @throws[StreamException]
      def getEventType: Int

      @throws[StreamException]
      def isEndDocument(eventType: Int): Boolean

      @throws[StreamException]
      def next: Int

      @throws[StreamException]
      def isStartElement(eventType: Int): Boolean

      @throws[StreamException]
      def isEndElement(eventType: Int): Boolean

      @throws[StreamException]
      def getLocalName: String

      @throws[StreamException]
      def getAttributeValue(what: String, key: String): String

      @throws[StreamException]
      def getAttributeCount: Int

      @throws[StreamException]
      def getAttributeValue(index: Int): String

      @throws[StreamException]
      def getAttributeLocalName(index: Int): String

      @throws[StreamException]
      def close(): Unit
    }


  }





}
