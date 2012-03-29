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

object TestRunner extends App
{
    class Taggable
    {
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
    class Way() extends Taggable
    {
        var nodes = mutable.ArrayBuffer[Node]()
    }
    
    class Canvas( val nodes : Iterable[Node], val ways : Iterable[Way] ) extends JFrame
    {
        val canvas = new PCanvas()        
        add(canvas)
        
        val layered = mutable.ListBuffer[(Int, PPath, Taggable)]()
        for ( w <- ways )
        {
            if ( !w.nodes.isEmpty )
            {
                val xs = w.nodes.map( _.lon.toFloat ).toArray
                val ys = w.nodes.map( -_.lat.toFloat ).toArray
                
                val wood = w.has( "natural", "wood" )
                val road = w.has( "highway" )
                val building = w.has( "building" )
                val waterway = w.has( "waterway", "riverbank" )
                val closed = wood || building || waterway
                
                val layer = if ( w.has("layer") ) w.keys("layer").toInt
                else if ( wood | waterway ) -1
                else 0
                
                val node = new PPath()
                node.setPathToPolyline( xs, ys )
                if (closed)
                {
                    val col = if ( wood ) new java.awt.Color( 0.0f, 1.0f, 0.0f )
                    else if ( building ) new java.awt.Color( 0.5f, 0.5f, 0.5f )
                    else if ( waterway ) new java.awt.Color( 0.5f, 0.5f, 1.0f )
                    else new java.awt.Color( 0.8f, 0.8f, 0.8f )
                    node.setPaint( col )
                    node.closePath()
                }
                else
                {
                    node
                }
                val lineCol = if ( road ) new java.awt.Color( 0.3f, 0.0f, 0.0f )
                else if ( w.has("waterway" ) ) new java.awt.Color( 0.5f, 0.5f, 1.0f )
                else new java.awt.Color( 0.7f, 0.7f, 0.7f )
                
                node.setStroke( new java.awt.BasicStroke( 1.0f, java.awt.BasicStroke.CAP_ROUND, java.awt.BasicStroke.JOIN_ROUND ) )
                node.setStrokePaint( lineCol )
                //canvas.getLayer().addChild(node)
                layered.append( (layer, node, w) )
            }
        }
        
        for ( (l, n, t) <- layered.sortWith( _._1 < _._1 ) )
        {
            canvas.getLayer().addChild(n)
            
            n.addInputEventListener( new PBasicInputEventHandler()
            {
                override def mousePressed( event : PInputEvent )
                {
                    println( t.keys.toList )
                }
                override def mouseDragged( event : PInputEvent ) {}
                override def mouseReleased( event : PInputEvent ) {}
                override def keyPressed( event : PInputEvent ) {}
            } )
        }
        
        for ( node <- nodes )
        {
            //val circ = PPath.createEllipse( node.lon.toFloat, -node.lat.toFloat, 3.0f, 3.0f )
            //canvas.getLayer().addChild( circ )
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
        val fin = new FileInputStream( fileName )
        val in = new BufferedInputStream(fin)
        val decompressor = new BZip2CompressorInputStream(in)
        
        val source = Source.fromInputStream( decompressor )
        val parser = new XMLEventReader(source)
        
        var nodes = mutable.HashMap[Long, Node]()
        val ways = mutable.ArrayBuffer[Way]()

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
                        val nn = new Node( (lat-meanlat) * 30000.0, (lon-meanlon) * 30000.0 )
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
                    currWay.nodes.append( nodes(ref) )
                }
                                
                case EvElemEnd(_, "way" ) =>
                {
                    ways.append( currWay )
                    currWay = new Way()
                }
                

                case _ =>
            }
        }
    }
    
    override def main( args : Array[String] ) =
    {
        val f = new XMLFilter( args(0), new Bounds(-1.3558, -1.2949, 51.7554, 51.7916) )
        val c = new Canvas( f.nodes.view.map( _._2 ), f.ways )
    }
}

