package org.seacourt.osm

import scala.collection.{mutable, immutable}

import java.io.{FileInputStream, BufferedInputStream}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream

import scala.io.Source
import scala.xml.pull.{XMLEventReader, EvElemStart, EvElemEnd, EvText}

import org.geotools.referencing.CRS
import org.geotools.geometry.{DirectPosition2D}

// Longitude W-E
class OSMReader( val fileName : String, bounds : Bounds )
{           
    var nodes = mutable.HashMap[Long, Node]()
    val ways = mutable.ArrayBuffer[Way]()
    
    // Some info here: http://alastaira.wordpress.com/2011/01/23/the-google-maps-bing-maps-spherical-mercator-projection/
    // 4326: WSG84, 3857: Gmap spherical mercator
    val lonLatCRS = CRS.decode("EPSG:4326", false)
    val ourCRS = CRS.decode("EPSG:3857", false)
    val lenient = true
    val transform = CRS.findMathTransform(lonLatCRS, ourCRS, lenient)
    
    //Geometry targetGeometry = JTS.transform( sourceGeometry, transform)

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
                    
                    //if ( bounds.within( lat, lon ) )
                    {
                        val pos = new DirectPosition2D( lat, lon )
                        transform.transform( pos, pos )
                        val nn = new Node( pos )
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
        // leisure:nature_reserve, tourism:viewpoint, natural:cliff, natural:cave_entrance,
        // natural:peak, waterway:waterfall, leisure:pitch, barrier:kissing_gate
        // oneway:yes
        val wood = w.has( "natural", "wood" ) || w.has( "landuse", "forest" )
        val highway = w.has( "highway" )
        val building = w.has( "building" ) || w.has( "landuse", "residential" )
        val waterway = w.has( "waterway", "riverbank" ) || w.has( "waterway", "canal" ) || w.has( "waterway", "stream" ) || w.has("natural", "water") || w.has("natural", "coastline")
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

object XMLUtils
{
    def saveToGML( fileName : String, f : OSMReader )
    {
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
                                                    w.nodes.map( n => n.pos.x + "," + n.pos.y ).mkString(" ")
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
                                            w.nodes.map( n => n.pos.x + "," + n.pos.y ).mkString(" ")
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
        
        scala.xml.XML.save(fileName, gml, "utf-8")
    }
}
