package org.seacourt.osm

import scala.util.Random.{nextInt, nextFloat}
import scala.collection.{mutable, immutable}


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


class Bounds( val lon1 : Double, val lon2 : Double, val lat1 : Double, val lat2 : Double )
{
    assert( lon2 >= lon1 )
    assert( lat2 >= lat1 )
    def within( lat : Double, lon : Double ) = lon >= lon1 && lon <= lon2 && lat >= lat1 && lat <= lat2
}

object GISTypes
{
    // 4326 is WGS84
    val line = DataUtilities.createType("line", "centerline:LineString:srid=4326,weight:Float" )
    val shape = DataUtilities.createType("shape", "geom:Polygon:srid=4326,weight:Float" )
    //val highway = DataUtilities.createType("shape", "centerline:LineString,name:String" )
}

object TestRunner extends App
{
    def weightWay( way : Way, heatMap : GridCoverage2D, resultArray : Array[Array[Float]] )
    {
        import org.geotools.geometry.{DirectPosition2D}
        import org.geotools.geometry.jts.{JTSFactoryFinder}
        import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate}
        import com.vividsolutions.jts.linearref.LengthIndexedLine
        
        val lastIndex = way.nodes.length-1
        var currPoints = mutable.ArrayBuffer[Coordinate]()
        
        val envelope = heatMap.getEnvelope2D()
        val geometryFactory = JTSFactoryFinder.getGeometryFactory( null )
        val gridGeom = heatMap.getGridGeometry()
        for ( (n, i) <- way.nodes.zipWithIndex )
        {
            val currCoord = new Coordinate( n.lon, n.lat )
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
                        val res = Array[Float](0.0f)
                        if ( envelope.contains(dp) )
                        {
                            heatMap.evaluate(dp.asInstanceOf[java.awt.geom.Point2D], res)
                            val gridPos = gridGeom.worldToGrid(dp)
                            resultArray(gridPos.y)(gridPos.x) += res(0)
                        }
                        index += 0.0002
                    }
                }
                
                currPoints.clear()
                currPoints.append( currCoord )
            }
        }
    }

        
    override def main( args : Array[String] ) =
    {
        // Oxford
        //val b = new Bounds(-1.4558, -1.1949, 51.6554, 51.8916)
        val b = new Bounds( -1.3743, -1.216, 51.735, 51.82 )
        val f = new OSMReader( args(0), b )
        
        
        import org.geotools.feature.{FeatureCollections}
        val featureCollection = FeatureCollections.newCollection("lines")
        
        {
            import org.geotools.geometry.jts.{JTSFactoryFinder}
            import com.vividsolutions.jts.geom.{GeometryFactory, LinearRing, Coordinate}
            import org.geotools.feature.simple.{SimpleFeatureBuilder}
            
            import org.geotools.{GML}
            import org.geotools.gml.producer.{FeatureTransformer}
               
            val geometryFactory = JTSFactoryFinder.getGeometryFactory( null )
            
            // In rural areas with no forest/lakes/other big features labelled, upweight for
            // things like cattle grids, stiles, bridleway, regional walking route
            // archaeological
            
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
                    case EntityType.waterway   => 50000.0
                    case EntityType.greenspace => 3000.0
                    case EntityType.farmland   => 2000.0
                    case _ => 0.0
                }
                
                
                if ( weight != 0.0 )
                {
                    if ( w.entityType.closed && w.entityType != EntityType.waterway && coords.length > 3 )
                    {
                        val ring = geometryFactory.createLinearRing( (coords ++ List( coords.head )).toArray )
                        val holes : Array[LinearRing] = null
                        val polygon = geometryFactory.createPolygon( ring, holes )
                        val feature = SimpleFeatureBuilder.build(GISTypes.shape, Array[java.lang.Object](polygon, new java.lang.Float(weight)), null)
                        featureCollection.add( feature )
                    }
                    else if ( coords.length > 1 )
                    {   
                        val line = geometryFactory.createLineString( coords.toArray )
                        val feature = SimpleFeatureBuilder.build(GISTypes.line, Array[java.lang.Object](line, new java.lang.Float(weight)), null)
                        featureCollection.add( feature )
                    }
                }
            }
        }            
        
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
                    val v = scala.math.exp(-d*d*invrsq).toFloat
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
            
            val envelope = featureCollection.getBounds()
                
            
            import org.geotools.filter.{ConstantExpression, AttributeExpressionImpl}
            import org.geotools.process.raster.VectorToRasterProcess

            val gridCoverage = VectorToRasterProcess.process( featureCollection, new AttributeExpressionImpl("weight"), new java.awt.Dimension( 1000, 1000 ), envelope, "agrid", null )
            

            val df = new org.geotools.coverage.processing.CoverageProcessor()
            val convolver = new org.geotools.coverage.processing.operation.Convolve()
            
            val cparams = df.getOperation("Convolve").getParameters()
            cparams.parameter("Source").setValue(gridCoverage)
            cparams.parameter("kernel").setValue( makeGaussianKernel(20) )
            
            val convolved = convolver.doOperation(cparams, null).asInstanceOf[GridCoverage2D]
            
            def writeTiff( fileName : String, coverage : GridCoverage2D )
            {
                val outputFile = new java.io.File( fileName )
                val format = new GeoTiffFormat()
                val writer = format.getWriter(outputFile)
                writer.write( coverage, null )
            }
            
            writeTiff( "test.tiff", convolved )
            
            // Now step along each way, one by one sampling equally spaced points
            // using LengthIndexedLine (for both weight and SRTM rasters)
            val resultArray = Array.tabulate( 1000, 1000 )( (x, y) => 0.0f )
            for ( w <- f.ways if w.entityType == EntityType.footpath || w.entityType == EntityType.cycleway || w.entityType == EntityType.road )
            {
                weightWay( w, convolved, resultArray )
            }
            
            val gcf = new GridCoverageFactory()
            val resultGrid = gcf.create( "agrid2", resultArray, convolved.getEnvelope2D() )
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


