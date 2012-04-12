import javax.swing.JFrame

import java.io.{FileInputStream, BufferedInputStream}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream

import scala.io.Source
import scala.xml.pull.{XMLEventReader, EvElemStart, EvElemEnd, EvText}


import edu.umd.cs.piccolo.PCanvas
import edu.umd.cs.piccolo.nodes.{PText, PPath}
import edu.umd.cs.piccolo.event.{PBasicInputEventHandler, PInputEvent}

import scala.util.Random.{nextInt, nextFloat}
import scala.collection.{mutable, immutable}

import org.jgrapht.graph._

import org.geotools.coverage.grid.{GridCoverageFactory, GridCoverage2D}
import org.opengis.referencing.crs._
import org.geotools.referencing.crs._
import org.geotools.geometry.{DirectPosition2D, Envelope2D}
import org.geotools.data.{DataUtilities}


object EntityType extends Enumeration
{
    import java.awt.Color
    
    class Element( val name : String, val closed : Boolean, val routeable : Boolean, val color : java.awt.Color, val dashPattern : Option[Array[Float]] ) extends Val(name)
    
    val unknown         = new Element("Unknown",        false, false, new Color( 0.7f, 0.4f, 0.7f ), None)
    val highway         = new Element("Highway",        false, true,  new Color( 0.6f, 0.0f, 0.0f ), None)
    val road            = new Element("Road",           false, true,  new Color( 0.3f, 0.0f, 0.0f ), None)
    val cycleway        = new Element("Cycleway",       false, true,  new Color( 0.0f, 1.0f, 0.0f ), Some( Array( 3.0f, 3.0f ) ))
    val footpath        = new Element("Footpath",       false, true,  new Color( 0.0f, 0.0f, 1.0f ), Some( Array( 3.0f, 3.0f ) ))
    val railway         = new Element("Railway",        false, false, new Color( 0.0f, 0.0f, 0.0f ), Some( Array( 4.0f, 4.0f ) ))
    val unknownLine     = new Element("Unknown line",   false, false, new Color( 0.7f, 0.7f, 0.7f ), None)
    
    val building        = new Element("Building",       true,  false, new Color( 0.9f, 0.9f, 0.9f ), None)
    val woodland        = new Element("Woodland",       true,  false, new Color( 0.0f, 0.6f, 0.0f ), None)
    val waterway        = new Element("Waterway",       true,  false, new Color( 0.5f, 0.5f, 1.0f ), None)
    val greenspace      = new Element("Greenspace",     true,  false, new Color( 0.0f, 1.0f, 0.0f ), None)
    val farmland        = new Element("Farmland",       true,  false, new Color( 0.5f, 0.3f, 0.3f ), None)
}

object TestRunner extends App
{
    class Taggable
    {
        var entityType = EntityType.unknown
        
        var tags = immutable.HashSet[(String, String)]()
        var keys = immutable.HashMap[String, String]()
        def add( k : String, v : String )
        {
            tags += ( (k, v) )
            keys += k -> v
        }
        
        def has( k : String, v : String ) = tags.contains( (k, v) )
        def has( k : String ) = keys.contains(k)
    }
    
    class Node( val lat : Double, val lon : Double ) extends Taggable
    {
        var wayMembership = 0
        def inWay = wayMembership > 0
    }
    
    class Way() extends Taggable
    {
        var nodes = mutable.ArrayBuffer[Node]()
    }
    
    class Canvas( val nodes : Iterable[Node], val ways : Iterable[Way], val routeGraph : RouteGraph ) extends JFrame
    {
        private def mouseClickHandler( fn : PInputEvent => Unit ) = new PBasicInputEventHandler()
        {
            override def mousePressed( event : PInputEvent ) = fn( event )
            override def mouseDragged( event : PInputEvent ) {}
            override def mouseReleased( event : PInputEvent ) {}
            override def keyPressed( event : PInputEvent ) {}
        }
    
        val jp = javax.swing.Box.createHorizontalBox()
        add(jp)
        val canvas = new PCanvas()        
        jp.add(canvas)
        
        val tm = new javax.swing.table.AbstractTableModel()
        {
            var data = Array.tabulate( 10, 2 )( (x, y) => (x * y).toString )
            override def getColumnCount() = if ( data.size == 0 ) 0 else data(0).size
            override def getRowCount() = data.size
            override def getValueAt( row : Int, col : Int ) = data(row)(col)
        }
        val tb = new javax.swing.JTable(tm)
        val sp = new javax.swing.JScrollPane(tb) 
        sp.setMaximumSize( new java.awt.Dimension( 250, 800 ) )
        sp.setMinimumSize( new java.awt.Dimension( 250, 800 ) )
        sp.setPreferredSize( new java.awt.Dimension( 250, 800 ) )
        jp.add( sp )
        
        var routeEndPoints = List[(PPath, Node)]()
        var nodeToPathMap = mutable.HashMap[Node, PPath]()
        
        val layered = mutable.ListBuffer[(Int, PPath, Taggable)]()
        for ( w <- ways )
        {
            if ( !w.nodes.isEmpty )
            {
                val xs = w.nodes.map( _.lon.toFloat ).toArray
                val ys = w.nodes.map( -_.lat.toFloat ).toArray
                
                val ctype = w.entityType
                val layer = if ( w.has("layer") ) w.keys("layer").filter( _ != '+' ).toInt
                else if ( ctype.closed ) -1
                else 0
                
                val node = new PPath()
                node.setPathToPolyline( xs, ys )
                ctype.dashPattern match
                {
                    case Some(pattern) =>
                    {
                        node.setStroke( new java.awt.BasicStroke( 1.0f, java.awt.BasicStroke.CAP_ROUND, java.awt.BasicStroke.JOIN_ROUND, 10, pattern, 0 ) )
                    }
                    case _ =>
                    {
                        node.setStroke( new java.awt.BasicStroke( 1.0f, java.awt.BasicStroke.CAP_ROUND, java.awt.BasicStroke.JOIN_ROUND ) )
                    }
                }
                
                if ( ctype.closed )
                {
                    node.setPaint( ctype.color )
                    node.closePath()
                }
                else
                {
                    node.setStrokePaint( ctype.color )
                }
                
                layered.append( (layer, node, w) )
            }
        }
        
        for ( (l, n, t) <- layered.sortWith( _._1 < _._1 ) )
        {
            canvas.getLayer().addChild(n)
            
            if ( false && t.has("name") )
            {   
                val label = new PText(t.keys("name"))
                label.setHorizontalAlignment( java.awt.Component.CENTER_ALIGNMENT )
                label.setBounds( n.getBounds() )
                label.setConstrainHeightToTextHeight(true)
                label.setConstrainWidthToTextWidth(true)
                canvas.getLayer().addChild(label)
            }
            
            
            
            n.addInputEventListener( mouseClickHandler( event =>
            {
                tm.data = t.keys.toArray.map( x => Array( x._1, x._2 ) )
                tm.fireTableDataChanged()
            } ) )
        }
        
        for ( node <- nodes.view.filter( !_.inWay ) )
        {
            val circ = PPath.createEllipse( node.lon.toFloat, -node.lat.toFloat, 3.0f, 3.0f )
            canvas.getLayer().addChild(circ)
            
            if ( false && node.has("name") )
            {
                val label = new PText()
                label.setBounds( node.lon.toFloat, -node.lat.toFloat, 0, 0 )
                label.setHorizontalAlignment( java.awt.Component.CENTER_ALIGNMENT )
                label.setConstrainHeightToTextHeight(true)
                label.setConstrainWidthToTextWidth(true)
                label.setText( node.keys("name") )
                canvas.getLayer().addChild(label)
            }
            
            circ.addInputEventListener( mouseClickHandler( event =>
            {
                tm.data = node.keys.toArray.map( x => Array( x._1, x._2 ) )
                tm.fireTableDataChanged()
            } ) )
        }
        
        for ( node <- routeGraph.nodes )
        {
            val circ = PPath.createEllipse( node.lon.toFloat-3.0f, (-node.lat.toFloat)-3.0f, 6.0f, 6.0f )
            circ.setPaint( new java.awt.Color( 0.0f, 0.0f, 1.0f ) )
            canvas.getLayer().addChild(circ)
            
            nodeToPathMap += node -> circ
            
            circ.addInputEventListener( mouseClickHandler( event =>
            {
                if ( routeEndPoints.size >= 2 )
                {
                    routeEndPoints.foreach( c => c._1.setPaint( new java.awt.Color( 0.0f, 0.0f, 1.0f ) ) )
                    routeEndPoints = Nil
                }
                routeEndPoints = (circ, node) :: routeEndPoints
                circ.setPaint( new java.awt.Color( 1.0f, 0.0f, 0.0f ) )
                
                if ( routeEndPoints.size == 2 )
                {
                    val List((c1, n1), (c2, n2)) = routeEndPoints
                    
                    val routeNodes = routeGraph.shortestRoute( n1, n2 )
                    val routeCircs = routeNodes.map( n => nodeToPathMap(n) )
                    (routeCircs zip routeNodes).foreach( x =>
                    {
                        x._1.setPaint( new java.awt.Color( 0.0f, 1.0f, 0.0f ) )
                        routeEndPoints = x :: routeEndPoints
                    } )
                }
            } ) )
        }

        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        setSize(1000, 700)
        setVisible(true)
    }
    
    class Bounds( val lon1 : Double, val lon2 : Double, val lat1 : Double, val lat2 : Double )
    {
        assert( lon2 >= lon1 )
        assert( lat2 >= lat1 )
        def within( lat : Double, lon : Double ) = lon >= lon1 && lon <= lon2 && lat >= lat1 && lat <= lat2
    }

    
    // Longitude W-E
    class XMLFilter( val fileName : String, bounds : Bounds )
    {           
        var nodes = mutable.HashMap[Long, Node]()
        val ways = mutable.ArrayBuffer[Way]()

        {
            val fin = new FileInputStream( fileName )
            val in = new BufferedInputStream(fin)
            val decompressor = new BZip2CompressorInputStream(in)
            
            val source = Source.fromInputStream( decompressor )
            val parser = new XMLEventReader(source)
            
            val (meanlon, meanlat) = ((bounds.lon1 + bounds.lon2)/2.0, (bounds.lat1 + bounds.lat2)/2.0)
            
            var currTaggable : Option[Taggable] = None
            var currWay = new Way()
            while (parser.hasNext)
            {
                parser.next match
                {
                    case EvElemStart(_, "node", attrs, _) =>
                    {
                        val id = attrs("id").text.toLong
                        val lat = attrs("lat").text.toDouble
                        val lon = attrs("lon").text.toDouble
                        
                        if ( bounds.within( lat, lon ) )
                        {
                            val nn = new Node( lat, lon )
                            nodes += id -> nn
                            currTaggable = Some(nn)
                        }
                    }
                    
                    case EvElemStart(_, "way", attrs, _) =>
                    {
                        currWay = new Way()
                        currTaggable = Some(currWay)
                    }
                    
                    case EvElemStart(_, "tag", attrs, _) =>
                    {
                        currTaggable match
                        {
                            case Some(taggable) => taggable.add( attrs("k").text, attrs("v").text )
                            case _ =>
                        }
                    }
                    
                    case EvElemStart(_, "nd", attrs, _) =>
                    {
                        val ref = attrs("ref").text.toLong
                        if ( nodes.contains(ref) )
                        {
                            val theNode = nodes(ref)
                            theNode.wayMembership += 1
                            currWay.nodes.append( theNode )
                        }
                    }
                                    
                    case EvElemEnd(_, "way" ) =>
                    {
                        currWay.entityType = wayType( currWay )
                        ways.append( currWay )
                        currWay = new Way()
                    }
                    

                    case _ =>
                }
            }
        }
        
        private def wayType( w : Way ) =
        {
            val wood = w.has( "natural", "wood" ) || w.has( "landuse", "forest" )
            val highway = w.has( "highway" )
            val building = w.has( "building" ) || w.has( "landuse", "residential" )
            val waterway = w.has( "waterway", "riverbank" ) || w.has("natural", "water") || w.has("natural", "coastline")
            val garden = w.has("residential", "garden" ) || w.has("leisure", "common") || w.has("leisure", "park") || w.has("landuse", "grass") || w.has("landuse", "meadow") || w.has("leisure", "pitch") || w.has( "leisure", "recreation_ground") || w.has( "landuse", "recreation_ground") || w.has( "landuse", "farmland") || w.has( "leisure", "nature_reserve") || w.has( "landuse", "orchard") || w.has( "landuse", "vineyard")
            val field = w.has("landuse", "field") || w.has("landuse", "farm")
            val railway = w.has("railway")
            val closed = wood || building || waterway || garden || field
         
            if ( wood ) EntityType.woodland
            else if ( building ) EntityType.building
            else if ( waterway ) EntityType.waterway
            else if ( garden ) EntityType.greenspace
            else if ( field ) EntityType.farmland
            else if ( railway ) EntityType.railway
            else if ( waterway ) EntityType.waterway
            else if ( highway )
            {
                w.keys("highway") match
                {
                    case ("path"|"track"|"footway"|"bridleway"|"pedestrian")    => EntityType.footpath
                    case "cycleway"                                             => EntityType.cycleway
                    case ("motorway"|"trunk"|"primary"|"secondary")             => EntityType.highway
                    case _ => EntityType.road
                }
               
            }
            else EntityType.unknown
        }            
    }
    
    
    class RouteGraph( xf : XMLFilter )
    {
        type GraphT = SimpleWeightedGraph[Node, DefaultWeightedEdge]
        
        val graph = new GraphT(classOf[DefaultWeightedEdge])
        
        private def nodeDist( n1 : Node, n2 : Node ) =
        {
            import scala.math._

            val theta = n1.lon - n2.lon
            var dist = sin(toRadians(n1.lat)) * sin(toRadians(n2.lat)) + cos(toRadians(n1.lat)) * cos(toRadians(n2.lat)) * cos(toRadians(theta))
            dist = acos(dist)
            dist = toDegrees(dist)
            dist = dist * 60 * 1.1515
            dist = dist * 1.609344
            
            dist
        }
        
        def nodes =
        {
            import scala.collection.JavaConverters._
            graph.vertexSet().asScala
        }
        
        def shortestRoute( n1 : Node, n2 : Node ) =
        {
            import scala.collection.JavaConverters._
            
            /*var edges = List[DefaultWeightedEdge]()
            for ( i <- 0 until 10000 )
            {
                if ( (i % 100) == 0 ) println(i)
                val dijk = new org.jgrapht.alg.DijkstraShortestPath( graph, n1, n2 )
                edges = dijk.getPath().getEdgeList().asScala.toList
            }*/
            
            val dijk = new org.jgrapht.alg.DijkstraShortestPath( graph, n1, n2 )
            val edges = dijk.getPath().getEdgeList().asScala.toList
            
            var allNodes = immutable.HashSet[Node]()
            for ( e <- edges )
            {
                allNodes += graph.getEdgeSource(e)
                allNodes += graph.getEdgeTarget(e)
            }
            
            allNodes
        }

        // When generating the routing graph, we are interested in keeping
        // nodes that are at the start or end of a way, or in more than one
        // way (way intersection points). All the rest can go, having
        // absorbed relevant distance information
        def processWay( way : Way )
        {
            val lastIndex = way.nodes.length-1
            var dist = 0.0
            var lastNode : Option[Node] = None
            var lastRouteNode : Option[Node] = None
            for ( (n, i) <- way.nodes.zipWithIndex )
            {
                // Update running distance between route nodes
                lastNode.foreach( ln => dist += (nodeDist(n, ln) * (if (way.entityType == EntityType.highway) 2.0 else 1.0)) )
                
                // Build route node if required
                val isRouteNode = n.wayMembership > 1 || i==0 || i==lastIndex
                if ( isRouteNode )
                {
                    graph.addVertex(n)
                    
                    // Step along this route sampling the heat map using LengthIndexedLine
                    lastRouteNode.foreach( lrn =>
                    {
                        if ( lrn != n )
                        {
                            val e = graph.addEdge( lrn, n )
                            if ( e != null ) graph.setEdgeWeight(e, dist)
                        }
                    } )
                    dist = 0.0
                    lastRouteNode = Some(n)
                }
                
                lastNode = Some(n)
            }
        }

        // Build the routing graph, way-by-way
        for ( w <- xf.ways if w.entityType.routeable && w.nodes.length > 1 ) processWay(w)
        println( "Num nodes: " + graph.vertexSet().size() + ", num edges: " + graph.edgeSet().size() )
    }
    
    //l = new LatLng(33.109283, -123.182312);

    object GISTypes
    {
        // 4326 is WGS84
        val line = DataUtilities.createType("line", "centerline:LineString:srid=4326,weight:Float" )
        val shape = DataUtilities.createType("shape", "geom:Polygon:srid=4326,weight:Float" )
        //val highway = DataUtilities.createType("shape", "centerline:LineString,name:String" )
    }

        
    override def main( args : Array[String] ) =
    {
        // Oxford
        //val b = new Bounds(-1.4558, -1.1949, 51.6554, 51.8916)
        val b = new Bounds( -1.3743, -1.216, 51.735, 51.82 )
        val f = new XMLFilter( args(0), b )
        
        
        import org.geotools.feature.{FeatureCollections}
        val featureCollection = FeatureCollections.newCollection("lines")
        
        {
            import org.geotools.geometry.jts.{JTSFactoryFinder}
            import com.vividsolutions.jts.geom.{GeometryFactory, LinearRing, Coordinate}
            import org.geotools.feature.simple.{SimpleFeatureBuilder}
            
            import org.geotools.{GML}
            import org.geotools.gml.producer.{FeatureTransformer}
            
         
            
               
            val geometryFactory = JTSFactoryFinder.getGeometryFactory( null )
            
            // Additionally, for nodes/ways: key=historic, amenity=bar/pub/cafe/restaurant
            // building=cathedral/chapel/church, craft=?, geological=?, mountain_pass=yes,
            // man_made=adit/lighthouse/pier/watermill/water_well/windmill
            // natural=?, railway=abandoned/disused/funicular
            // route=?, tourism=?, waterway=?(not ditch/drain)
            for ( w <- f.ways )
            {
                val coords = w.nodes.view.map( n => new Coordinate( n.lon, n.lat ) ).toList
                
                val weight = w.entityType match
                {
                    case EntityType.highway    => -100000.0
                    case EntityType.building   => -2000.0
                    case EntityType.woodland   => 5000.0
                    case EntityType.waterway   => 5000.0
                    case EntityType.greenspace => 3000.0
                    case EntityType.farmland   => 2000.0
                    case _ => 0.0
                }
                
                
                if ( weight != 0.0 )
                {
                    if ( w.entityType.closed )
                    {
                        val ring = geometryFactory.createLinearRing( (coords ++ List( coords.head )).toArray )
                        val holes : Array[LinearRing] = null
                        val polygon = geometryFactory.createPolygon( ring, holes )
                        val feature = SimpleFeatureBuilder.build(GISTypes.shape, Array[java.lang.Object](polygon, new java.lang.Float(weight)), null)
                        featureCollection.add( feature )
                    }
                    else
                    {   
                        val line = geometryFactory.createLineString( coords.toArray )
                        val feature = SimpleFeatureBuilder.build(GISTypes.line, Array[java.lang.Object](line, new java.lang.Float(weight)), null)
                        featureCollection.add( feature )
                    }
                }
            }
            
            // The following functionality in Geotools is a combination of badly documented and badly broken. Lovely.
            /*
            val schemaFile = (new java.io.File("myschema.xsd")).getCanonicalFile()
            schemaFile.createNewFile()
            val schemaURL = schemaFile.toURI().toURL()
            val baseURL = schemaFile.getParentFile().toURI().toURL()
            
            val xsd = new java.io.FileOutputStream(schemaFile)
            val encode = new GML(GML.Version.WFS1_1)
            
            encode.setBaseURL(baseURL)
            encode.setNamespace("awosm", schemaURL.toExternalForm())
            encode.encode(xsd, GISTypes.highway)
            xsd.close()


            val transform = new FeatureTransformer()
            //transform.setEncoding(charset)
            transform.setIndentation(4)
            transform.setGmlPrefixing(true)
            
            val schema = featureCollection.getSchema()
            val prefix = schema.getUserData().get("prefix").asInstanceOf[String]
            val namespace = schema.getName().getNamespaceURI()
            transform.getFeatureTypeNamespaces().declareDefaultNamespace(prefix, namespace)
            transform.addSchemaLocation(prefix, namespace)
            
            //String srsName = CRS.toSRS(schema.getCoordinateReferenceSystem());
            //if (srsName != null) {
            //    transform.setSrsName(srsName);
            //}
            
            // define feature collection
            transform.setCollectionPrefix(prefix);
            transform.setCollectionNamespace(namespace);
            
            // other configuration
            transform.setCollectionBounding(true); // include bbox info
            
            val op = new java.io.FileOutputStream( new java.io.File( "output2.gml" ) )
            transform.transform(featureCollection, op);
            
            //val op = new java.io.FileOutputStream( new java.io.File( "output2.gml" ) )
            //val encode2 = new GML(GML.Version.WFS1_1)
            //encode2.setBaseURL(baseURL)
            //encode2.setNamespace("awosm", "myschema.xsd")
            //encode2.encode( op, featureCollection )
            //op.close()
            */
            
            
        }            

        val gml =

            <ogr:FeatureCollection
                 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                 xmlns:ogr="http://ogr.maptools.org/"
                 xmlns:gml="http://www.opengis.net/gml">
            {
                for ( (w, i) <- f.ways.zipWithIndex ) yield
                {
                    <gml:featureMember>
                    {
                        <ogr:unnamed>
                            <ogr:geometryProperty>
                            {
                                if ( w.entityType.closed )
                                {
                                    <gml:Polygon>
                                        <gml:outerBoundaryIs>
                                            <gml:LinearRing>
                                                <gml:coordinates>
                                                {
                                                    w.nodes.map( n => n.lon + "," + n.lat ).mkString(" ")
                                                }
                                                </gml:coordinates>
                                            </gml:LinearRing>
                                        </gml:outerBoundaryIs>
                                    </gml:Polygon>
                                }
                                else
                                {
                                    <gml:LineString>
                                        <gml:coordinates>
                                        {
                                            w.nodes.map( n => n.lon + "," + n.lat ).mkString(" ")
                                        }
                                        </gml:coordinates>
                                    </gml:LineString>
                                }
                            }
                            </ogr:geometryProperty>
                            <ogr:cat>{i}</ogr:cat>
                            <ogr:name>
                            {
                                if ( w.keys.contains("name") ) w.keys("name") else ""
                            }
                            </ogr:name>
                        </ogr:unnamed>.copy(label=w.entityType.toString)
                    }
                    </gml:featureMember>
                }
            }
            </ogr:FeatureCollection>
        
        scala.xml.XML.save("output.gml", gml)
        
        {
            import javax.media.jai.{KernelJAI}
            
            def makeGaussianKernel( radius : Int ) : KernelJAI = 
            {
                val diameter = 2*radius + 1
                val invrsq = 1.0F/(radius*radius)

                val gaussianData = new Array[Float](diameter)

                var sum = 0.0F
                for ( i <- 0 until diameter )
                {
                    val d = i - radius;
                    val v = Math.exp(-d*d*invrsq).toFloat
                    gaussianData(i) = v
                    sum += v
                }

                // Normalize
                val invsum = 1.0F/sum;
                for ( i <- 0 until diameter )
                {
                    gaussianData(i) *= invsum;
                }

                new KernelJAI(diameter, diameter, radius, radius, gaussianData, gaussianData)
            }
            
            import org.geotools.gce.geotiff.GeoTiffFormat
            
            //val envelope = new Envelope2D(
            //    new DirectPosition2D( DefaultGeographicCRS.WGS84, b.lon1, b.lat1 ),
            //    new DirectPosition2D( DefaultGeographicCRS.WGS84, b.lon2, b.lat2 ) )
            val envelope = featureCollection.getBounds()
                
            
            import org.geotools.filter.{ConstantExpression, AttributeExpressionImpl}
            import org.geotools.process.raster.VectorToRasterProcess
            
            //val gridCoverage = VectorToRasterProcess.process( featureCollection, ConstantExpression.constant(20000.0), new java.awt.Dimension( 1000, 1000 ), envelope, "agrid", null )
            val gridCoverage = VectorToRasterProcess.process( featureCollection, new AttributeExpressionImpl("weight"), new java.awt.Dimension( 1000, 1000 ), envelope, "agrid", null )
            

            val df = new org.geotools.coverage.processing.CoverageProcessor()
            val convolver = new org.geotools.coverage.processing.operation.Convolve()
            
            val cparams = df.getOperation("Convolve").getParameters()
            cparams.parameter("Source").setValue(gridCoverage)
            cparams.parameter("kernel").setValue( makeGaussianKernel(40) )
            
            val convolved = convolver.doOperation(cparams, null).asInstanceOf[GridCoverage2D]
            
            val outputFile = new java.io.File( "test.tiff" )
            val format = new GeoTiffFormat()
            val writer = format.getWriter(outputFile)
            writer.write( convolved, null )
            
            // Now step along each way, one by one sampling equally spaced points
            // using LengthIndexedLine (for both weight and SRTM rasters)
        }
        
        // Stockholm
        //val f = new XMLFilter( args(0), new Bounds(17.638, 18.47, 59.165, 59.502) )
        
        // West Chilterns
        //val f = new XMLFilter( args(0), new Bounds(-1.0436, -0.8356, 51.5668, 51.6656) )
        
        //val rg = new RouteGraph( f )
        //val c = new Canvas( f.nodes.view.map( _._2 ), f.ways, rg )
    }
}


