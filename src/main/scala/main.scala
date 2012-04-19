package org.seacourt.osm

import scala.util.Random.{nextInt, nextFloat}
import scala.collection.{mutable, immutable}


import org.geotools.coverage.grid.{GridCoverageFactory, GridCoverage2D, GridCoordinates2D}
import org.opengis.referencing.crs._
import org.geotools.referencing.crs._
import org.geotools.geometry.{DirectPosition2D, Envelope2D}
import org.geotools.data.{DataUtilities}
import org.geotools.filter.{ConstantExpression, AttributeExpressionImpl}
import org.geotools.process.raster.VectorToRasterProcess

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


object TestRunner extends App
{
    def weightWay( envelope : Envelope2D, way : Way, worldToGrid : DirectPosition2D => (Int, Int), heatMap : Array[Array[Float]], resultArray : Array[Array[Float]] )
    {
        import org.geotools.geometry.{DirectPosition2D}
        import org.geotools.geometry.jts.{JTSFactoryFinder}
        import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate}
        import com.vividsolutions.jts.linearref.LengthIndexedLine
        
        val lastIndex = way.nodes.length-1
        var currPoints = mutable.ArrayBuffer[Coordinate]()
        
        val geometryFactory = JTSFactoryFinder.getGeometryFactory( null )
        for ( (n, i) <- way.nodes.zipWithIndex )
        {
            val currCoord = new Coordinate( n.pos.x, n.pos.y )
            currPoints.append( currCoord )
            
            val isRouteNode = n.wayMembership > 1 || i==lastIndex
            if ( isRouteNode )
            {
                if ( currPoints.size > 2 )
                {
                    val line = geometryFactory.createLineString( currPoints.toArray )
                    
                    // Step along this route sampling the heat map using LengthIndexedLine
                    val lil = new LengthIndexedLine(line)
                    
                    var index = 0.0
                    val endIndex = lil.getEndIndex()
                    while (index < endIndex)
                    {
                        val coords = lil.extractPoint(index)
                        val dp = new DirectPosition2D( coords.x, coords.y )
                        if ( envelope.contains(dp) )
                        {
                            val (x, y) = worldToGrid( dp )
                            resultArray(y)(x) += heatMap(y)(x)
                        }
                        
                        // Increment of 50m
                        index += 50.0f
                    }
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
    override def main( args : Array[String] ) =
    {
        // Oxford
        val f = new OSMReader( args(0) )
        
        XMLUtils.saveToGML( "output.gml", f )
        import org.geotools.feature.{FeatureCollections}
        
        // Radius is in grid cells (for now, metres would be better)
        case class OfInterest( et : EntityType.Value, weight : Double, radius : Int )
        
        val ofInterest = List(
            new OfInterest( EntityType.highway,     -2000.0f,  10 ),
            new OfInterest( EntityType.building,    -1000.0f,  10 ),
            new OfInterest( EntityType.woodland,    1000.0f,   10 ),
            new OfInterest( EntityType.waterway,    1000.0f,   10 ),
            new OfInterest( EntityType.greenspace,  1000.0f,   10 ),
            new OfInterest( EntityType.farmland,    500.0f,    10 ) )

        def rasterDims = (1000, 1000)
        val envelope = f.bounds.envelope
        
        val cellWeights = Array.tabulate( rasterDims._1, rasterDims._2 )( (x, y) => 0.0f )
        for ( current <- ofInterest )
        {
            println( "Generating raster for %s".format(current.et) )
            val featureCollection = FeatureCollections.newCollection("lines")
        
            import org.geotools.geometry.jts.{JTSFactoryFinder}
            import com.vividsolutions.jts.geom.{GeometryFactory, LinearRing, Coordinate}
            import org.geotools.feature.simple.{SimpleFeatureBuilder}
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

            val gmk = new GaussianMaxKernel( current.radius,
            {
                (x, y) => if ( x >=0 && y >= 0 && x < rasterDims._1 && y < rasterDims._2 ) Some(gcArr(y)(x)) else None
            } )
            
            for ( x <- 0 until rasterDims._1; y <- 0 until rasterDims._2 )
            {
                if ( current.weight >= 0.0 )
                {
                    cellWeights(x)(y) += gmk(x, y)
                }
                else
                {
                    cellWeights(x)(y) -= gmk(x, y)
                }
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
            val gridGeom = heatMapCoverage.getGridGeometry()
            def worldToGrid( dp : DirectPosition2D) =
            {
                val res = gridGeom.worldToGrid(dp)
                (dp.x.toInt, dp.y.toInt)
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
        }
        
        // Stockholm
        //val f = new XMLFilter( args(0), new Bounds(17.638, 18.47, 59.165, 59.502) )
        
        // West Chilterns
        //val f = new XMLFilter( args(0), new Bounds(-1.0436, -0.8356, 51.5668, 51.6656) )
        
        //val rg = new RouteGraph( f )
        //val c = new Canvas( f.nodes.view.map( _._2 ), f.ways, rg )
    }
}


