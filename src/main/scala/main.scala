import javax.swing.JFrame

import java.io.{FileInputStream, BufferedInputStream}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream

import scala.io.Source
import scala.xml.pull.{XMLEventReader, EvElemStart, EvElemEnd, EvText}


import edu.umd.cs.piccolo.PCanvas
import edu.umd.cs.piccolo.nodes.{PText, PPath}

import scala.util.Random.{nextInt, nextFloat}
import scala.collection.{mutable, immutable}

object TestRunner extends App
{
    class Taggable
    {
        var tags = mutable.HashMap[String, String]()  
        def add( k : String, v : String )
        {
            tags += k -> v
        }
    }
    
    class Node( val lat : Double, val lon : Double ) extends Taggable
    class Way() extends Taggable
    {
        var nodes = mutable.ArrayBuffer[Node]()
    }
    
    class Canvas( val nodes : Iterable[Node], val ways : Iterable[Way] ) extends JFrame
    {
        val canvas = new PCanvas()
        val text = new PText("Boom")
        
        canvas.getLayer().addChild(text)
        add(canvas)
        
        for ( w <- ways )
        {
            if ( !w.nodes.isEmpty )
            {
                val xs = w.nodes.map( _.lon.toFloat ).toArray
                val ys = w.nodes.map( -_.lat.toFloat ).toArray
                
                //val node = PPath.createPolyline( xs, ys )
                val node = new PPath()
                node.setPathToPolyline( xs, ys )
                node.setStroke( new java.awt.BasicStroke( 1.0f, java.awt.BasicStroke.CAP_ROUND, java.awt.BasicStroke.JOIN_ROUND ) )
                node.setStrokePaint( new java.awt.Color( nextFloat, nextFloat, nextFloat ) )
                canvas.getLayer().addChild(node)
            }
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
                        val nn = new Node( (lat-meanlat) * 10000.0, (lon-meanlon) * 10000.0 )
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

