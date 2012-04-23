package org.seacourt.osm

import scala.util.Random.{nextInt, nextFloat}
import scala.collection.{mutable, immutable}

import org.opengis.geometry.{Envelope}
import org.opengis.coverage.grid.{GridEnvelope}
import org.geotools.coverage.grid.{GridGeometry2D, GridEnvelope2D}
import org.geotools.coverage.grid.{GridCoverageFactory, GridCoverage2D, GridCoordinates2D}
import org.opengis.referencing.crs._
import org.geotools.referencing.crs._
import org.geotools.geometry.{DirectPosition2D, Envelope2D}
import org.geotools.data.{DataUtilities}
import org.geotools.filter.{ConstantExpression, AttributeExpressionImpl}
import org.geotools.process.raster.VectorToRasterProcess
import org.geotools.feature.{FeatureCollections}
import org.geotools.feature.simple.{SimpleFeatureBuilder}

import sbinary._
import sbinary.Operations._

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

class Node( val pos : DirectPosition2D ) extends Taggable
{
    var wayMembership = 0
    def inWay = wayMembership > 0
}

class Way() extends Taggable
{
    var nodes = mutable.ArrayBuffer[Node]()
}


object GISTypes
{
    // 4326 is WGS84, 3857 is Google spherical mercator
    val line = DataUtilities.createType("line", "centerline:LineString:srid=3857" )
    val shape = DataUtilities.createType("shape", "geom:Polygon:srid=3857" )
}


class GaussianMaxKernel( val radius : Int, val fn : (Int, Int) => Option[Double] )
{
    private val diam = (radius*2)-1
    
    // Gaussian quadrant. Not normalised as we'll be doing max
    private val arr = Array.tabulate( diam, diam )( (x, y) =>
    {
        val cx = x-radius
        val cy = y-radius
        
        val invrsq = 1.0F/((radius/2.0)*(radius/2.0))
        val d = scala.math.sqrt((cx*cx).toDouble + (cy*cy).toDouble)
        
        scala.math.exp(-d*d*invrsq).toDouble
    } )
    
    def apply( x : Int, y : Int ) =
    {
        var value = 0.0
        for ( sx <- 0 until diam; sy <- 0 until diam )
        {
            val rx = sx-radius
            val ry = sy-radius
         
            fn(x+rx, y+ry).foreach( pxval =>
            {
                value = (arr(sx)(sy) * pxval) max value
            } )
        }
        value.toFloat
    }
}


class MapMaker
{
    val routeNodeMap = mutable.HashMap[DirectPosition2D, RouteNode]()
    
    def weightToDistMultipler( weight : Double ) =
    {
        val maxWeight = 2.0f
        val minWeight = -2.0f;
        
        val clipped = (weight min maxWeight) max minWeight
        
        // Max weight - 1.0, min weight - 0.0
        val normalized = clipped / (maxWeight - minWeight)
        
        // Max weight, distance x 1.0; min weight, dist x 4.0
        1.0 + 3.0*(1.0 - normalized)
    }
    
    def weightWay( envelope : Envelope2D, way : Way, worldToGrid : DirectPosition2D => (Int, Int), heatMap : Array[Array[Float]], resultArray : Array[Array[Float]] )
    {
        import org.geotools.geometry.{DirectPosition2D}
        import org.geotools.geometry.jts.{JTSFactoryFinder}
        import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate}
        import com.vividsolutions.jts.linearref.LengthIndexedLine
        
        val lastIndex = way.nodes.length-1
        var currPoints = mutable.ArrayBuffer[DirectPosition2D]()
        
        val geometryFactory = JTSFactoryFinder.getGeometryFactory( null )
        for ( (n, i) <- way.nodes.zipWithIndex )
        {
            val currCoord = new DirectPosition2D( n.pos.x, n.pos.y )
            currPoints.append( currCoord )
            
            val isRouteNode = n.wayMembership > 1 || i==lastIndex
            if ( isRouteNode )
            {
                if ( currPoints.size > 2 )
                {
                    val line = geometryFactory.createLineString( currPoints.toArray.map( v => new Coordinate(v.x, v.y) ) )
                    
                    var routeLength = 0.0
                    
                    // Step along this route sampling the heat map using LengthIndexedLine
                    val lil = new LengthIndexedLine(line)
                    
                    var index = 0.0
                    val endIndex = lil.getEndIndex()
                    while (index < endIndex)
                    {
                        val coord = lil.extractPoint(index)
                        val dp = new DirectPosition2D( coord.x, coord.y )
                        if ( envelope.contains(dp) )
                        {
                            val (x, y) = worldToGrid( dp )
                            val hm = heatMap(y)(x)
                            if ( hm > 0.0 )
                            {
                                resultArray(y)(x) += hm
                            }
                        }
                        
                        // Increment of 50m
                        val distIncrement = 50.0f
                        index += distIncrement
                        routeLength += weightToDistMultipler( distIncrement )
                    }
                    
                    val first = currPoints.head
                    val last = currPoints.last
                    val startNode = routeNodeMap.getOrElseUpdate( first, new RouteNode( Pos(first) ) )
                    val endNode = routeNodeMap.getOrElseUpdate( last, new RouteNode( Pos(last) ) )
                    new RouteEdge( startNode, endNode, routeLength, currPoints.drop(1).dropRight(1).toArray.map( Pos(_) ) )
                }
                
                currPoints.clear()
                currPoints.append( currCoord )
            }
        }
    }
    
    // In rural areas with no forest/lakes/other big features labelled, upweight for
    // things like cattle grids, stiles, bridleway, regional walking route
    // archaeological
    
    // Additionally, for nodes/ways: key=historic, amenity=bar/pub/cafe/restaurant
    // building=cathedral/chapel/church, craft=?, geological=?, mountain_pass=yes,
    // man_made=adit/lighthouse/pier/watermill/water_well/windmill
    // natural=?, railway=abandoned/disused/funicular
    // route=?, tourism=?, waterway=?(not ditch/drain)
    def run( osmFile : String ) =
    {
        // Oxford
        val f = new OSMReader( osmFile )
        
        XMLUtils.saveToGML( "output.gml", f )
        
        // Radius is in grid cells (for now, metres would be better)
        case class OfInterest( et : EntityType.Value, weight : Double, radius : Int )
        
        val ofInterest = List(
            new OfInterest( EntityType.highway,     -2.0f,  10 ),
            new OfInterest( EntityType.building,    -1.0f,  10 ),
            new OfInterest( EntityType.woodland,    1.0f,   10 ),
            new OfInterest( EntityType.waterway,    1.0f,   10 ),
            new OfInterest( EntityType.greenspace,  1.0f,   10 ),
            new OfInterest( EntityType.farmland,    1.0f,   10 ) )

        def rasterDims = (1000, 1000)
        val envelope = f.bounds.envelope
        
        val gridGeoms = new GridGeometry2D(
            new GridEnvelope2D( 0, 0, rasterDims._1, rasterDims._2 ).asInstanceOf[GridEnvelope],
            envelope.asInstanceOf[Envelope] )
        
        val cellWeights = Array.tabulate( rasterDims._1, rasterDims._2 )( (x, y) => 0.0f )
        
        def smooth( weight : Double, radius : Int, into : Array[Array[Float]], fromFn : (Int, Int) => Option[Double] )
        {
            val gmk = new GaussianMaxKernel( radius, fromFn )
            
            for ( x <- 0 until rasterDims._1; y <- 0 until rasterDims._2 )
            {
                if ( weight >= 0.0 )
                {
                    into(x)(y) += gmk(x, y)
                }
                else
                {
                    into(x)(y) -= gmk(x, y)
                }
            }
        }
        
        // Write out all the single node points of interest and smooth
        {
            val weight = 1000.0f
            
            val nodePointWeights = Array.tabulate( rasterDims._1, rasterDims._2 )( (x, y) => 0.0f )
            for ( (i, n) <- f.nodes )
            {
                val goodNode =
                    n.has("historic") || n.has("building", "cathedral") || n.has("building", "chapel") || n.has("building", "church") ||
                    n.has("geological") || n.has("man_made", "adit") || n.has("man_made", "lighthouse") || n.has("man_made", "pier") ||
                    n.has("man_made", "watermill") || n.has("man_made", "water_well") || n.has("man_made", "windmill") ||
                    n.has("tourism") || n.has("craft") || n.has("archaeological") || n.has("barrier", "stile") || n.has("barrier", "gate") ||
                    n.has("barrier", "cattle_grid") || n.has("denomination", "anglican") | n.has("amenity", "pub") ||
                    n.has("waterway", "weir")

                if ( goodNode )
                {
                    val grid = gridGeoms.worldToGrid( n.pos )
                    nodePointWeights(grid.x)(grid.y) = weight
                }
            }
            smooth( weight, 10, cellWeights,
                (x, y) => if ( x >=0 && y >= 0 && x < rasterDims._1 && y < rasterDims._2 ) Some(nodePointWeights(y)(x)) else None
            )
        }
        
        // Write out all the vectors to raster
        if ( true )
        {
            for ( current <- ofInterest )
            {
                println( "Generating raster for %s".format(current.et) )
                val featureCollection = FeatureCollections.newCollection("lines")
            
                import org.geotools.geometry.jts.{JTSFactoryFinder}
                import com.vividsolutions.jts.geom.{GeometryFactory, LinearRing, Coordinate}
                import org.geotools.gml.producer.{FeatureTransformer}
                   
                // Build all the features for this entity type into the feature list
                val geometryFactory = JTSFactoryFinder.getGeometryFactory( null )
                for ( w <- f.ways if w.entityType == current.et )
                {
                    val coords = w.nodes.view.map( n => new Coordinate( n.pos.x, n.pos.y ) ).toList
                    
                    if ( w.entityType.closed && w.entityType != EntityType.waterway && coords.length > 3 )
                    {
                        val ring = geometryFactory.createLinearRing( (coords ++ List( coords.head )).toArray )
                        val holes : Array[LinearRing] = null
                        val polygon = geometryFactory.createPolygon( ring, holes )
                        val feature = SimpleFeatureBuilder.build(GISTypes.shape, Array[java.lang.Object](polygon), null)
                        featureCollection.add( feature )
                    }
                    else if ( coords.length > 1 )
                    {   
                        val line = geometryFactory.createLineString( coords.toArray )
                        val feature = SimpleFeatureBuilder.build(GISTypes.line, Array[java.lang.Object](line), null)
                        featureCollection.add( feature )
                    }
                }
           
                
                // Render the features directly onto a grid
                val gridCoverage = VectorToRasterProcess.process( featureCollection, ConstantExpression.constant(scala.math.abs(current.weight)), new java.awt.Dimension( rasterDims._1, rasterDims._2 ), envelope, "agrid", null )
                
                // Buffer the features out using max kernel and an appropriate radius into cellWeights
                val gcArr = Array.tabulate( rasterDims._1, rasterDims._2 )((x, y) =>
                {
                    val res = gridCoverage.evaluate( new GridCoordinates2D( x, y ), Array[Float](0.0f) )
                    res(0)
                } )//gridCoverage.getBackingArray
                
                smooth( current.weight, current.radius, cellWeights,
                    (x, y) => if ( x >=0 && y >= 0 && x < rasterDims._1 && y < rasterDims._2 ) Some(gcArr(y)(x)) else None
                )
            }
        }
        
        def writeTiff( fileName : String, coverage : GridCoverage2D )
        {
            import org.geotools.gce.geotiff.GeoTiffFormat
            
            val outputFile = new java.io.File( fileName )
            val format = new GeoTiffFormat()
            val writer = format.getWriter(outputFile)
            writer.write( coverage, null )
        }
        
        val gcf = new GridCoverageFactory()
        val heatMapCoverage = gcf.create("agrid", cellWeights, envelope)
        writeTiff("test.tiff", heatMapCoverage)
        
        {
            def worldToGrid( dp : DirectPosition2D ) =
            {
                val res : GridCoordinates2D = gridGeoms.worldToGrid(dp)
                (res.getCoordinateValue(0), res.getCoordinateValue(1))
            }
            
            // Now step along each way, one by one sampling equally spaced points
            // using LengthIndexedLine (for both weight and SRTM rasters)
            val resultArray = Array.tabulate( rasterDims._1, rasterDims._2 )( (x, y) => 0.0f )
            for ( w <- f.ways if w.entityType == EntityType.footpath || w.entityType == EntityType.cycleway || w.entityType == EntityType.road )
            {
                weightWay( envelope, w, worldToGrid, cellWeights, resultArray )
            }
            
            
            val resultGrid = gcf.create( "agrid2", resultArray, envelope )
            writeTiff( "test2.tiff", resultGrid )
            
            val rg = new RouteGraph( routeNodeMap.map( _._2 ).toArray )
            
            import SerializationProtocol._
            
            toFile(rg)( new java.io.File("routeGraph.bin") )
        }
    }
}

object GeoJSON
{
    abstract class JSONElement
    {
        def write( op : String => Unit )
    }
    
    class JSONString( val str : String ) extends JSONElement
    {
        def write( op : String => Unit ) = op( "\"" + str + "\"" )
    }
    
    class JSONInt( val v : Int ) extends JSONElement
    {
        def write( op : String => Unit ) = op( v.toString )
    }
    
    class JSONDouble( val v : Double ) extends JSONElement
    {
        def write( op : String => Unit ) = op( v.toString )
    }
    
    class JSONDictKey( val key : String )
    {   
        def ->( value : JSONElement ) =
        {
            (key, value)
        }        
    }
    
    implicit def strToKey( key : String ) = new JSONDictKey(key)
    implicit def strToJSON( v : String ) = new JSONString(v)
    implicit def intToJSON( v : Int ) = new JSONInt(v)
    implicit def doubleToJSON( v : Double ) = new JSONDouble(v)
    
    class JSONDict( val args : (String, JSONElement)* ) extends JSONElement
    {
        def write( op : String => Unit ) =
        {
            op( "{" )
            for ( (k, v) <- args )
            {
                op( "\"" + k + "\"" )
                op( ":" )
                v.write(op)
            }
            op( "}" )
        }

    }
    
    class JSONList( val els : JSONElement* ) extends JSONElement
    {
        def write( op : String => Unit ) =
        {
            op("[")
            els.foreach( e => e.write(op) )
            op("]")
        }
    }
    
    abstract class Geometry
    {
        def toJSON : JSONElement
    }
    
    class Polygon( val points : Seq[(Double, Double)], val closed : Boolean ) extends Geometry
    {
        def toJSON = new JSONDict(
            "type"          -> "LineString",
            "coordinates"   -> new JSONList( points.map( p => new JSONList( p._1, p._2 ) ) : _* )
        )
    }
    
    class Feature( val id : String, val properties : List[(String, String)], val geometry : Geometry )
    {
        def toJSON = new JSONDict(
            "type"          -> "Feature",
            "id"            -> id,
            "geometry"      -> geometry.toJSON/*,
            "properties"    -> new JSONDict( )*/
        )
    }
    
    class FeatureCollection( val crs : String )
    {
        val features = mutable.ArrayBuffer[Feature]()
        
        def toJSON = new JSONDict(
            "type"  -> "FeatureCollection",
            "crs"   ->  new JSONDict(
                "type"          -> "EPSG",
                "properties"    -> new JSONDict(
                    "code"              -> 4326,
                    "coordinate_order"  -> new JSONList(1, 0)
                )
            ),
            "features"  -> new JSONList( features.map( _.toJSON ) : _* )
        )
    }
}

class MapReader( graphFile : String )
{
    import SerializationProtocol._
    
    val graph = fromFile[RouteGraph]( new java.io.File(graphFile) )
    
    def run( outFile : String )
    {
        import GeoJSON._
        
        val uniqueEdges = (for ( n <- graph.nodes; e <- n.edges ) yield e).toSet
     
        val fc = new FeatureCollection( "3857" )
        for ( (e, i) <- uniqueEdges.zipWithIndex )
        {
            fc.features.append( new Feature( "id_%d".format(i), List(), new Polygon( e.points.map( v => (v.x, v.y) ), false ) ) )
        }
        
        // Dump fc out to disk @outFile
        val p = new java.io.PrintWriter( new java.io.File( outFile ) )
        try
        {
            val json = fc.toJSON
            json.write( s => p.write(s) )
        }
        finally
        {
            p.close()
        }
    }  
}

object TestRunner extends App
{
    override def main( args : Array[String] ) =
    {
        if ( true )
        {
            val mm = new MapMaker()
            mm.run(args(0))
        }
        else
        {
            val rg = new MapReader( args(0) )
            rg.run( args(1) )
        }
    }
}

