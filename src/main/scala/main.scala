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


object EntityType extends Enumeration
{
    import java.awt.Color
    
    class Element( val name : String, val closed : Boolean, val color : java.awt.Color, val dashPattern : Option[Array[Float]] = None ) extends Val(name)
    
    val unknown         = new Element("Unknown",        false, new Color( 0.7f, 0.4f, 0.7f ))
    val highway         = new Element("Highway",        false, new Color( 0.3f, 0.0f, 0.0f ))
    val cycleway        = new Element("Cycleway",       false, new Color( 0.0f, 1.0f, 0.0f ), Some( Array( 2.0f, 1.0f ) ))
    val footpath        = new Element("Footpath",       false, new Color( 0.0f, 0.0f, 1.0f ), Some( Array( 2.0f, 1.0f ) ))
    val railway         = new Element("Railway",        false, new Color( 0.0f, 0.0f, 0.0f ), Some( Array( 3.0f, 3.0f ) ))
    val unknownLine     = new Element("Unknown line",   false, new Color( 0.7f, 0.7f, 0.7f ))
    
    val building        = new Element("Building",       true,  new Color( 0.9f, 0.9f, 0.9f ))
    val woodland        = new Element("Woodland",       true,  new Color( 0.0f, 0.6f, 0.0f ))
    val waterway        = new Element("Waterway",       true,  new Color( 0.5f, 0.5f, 1.0f ))
    val greenspace      = new Element("Greenspace",     true,  new Color( 0.0f, 1.0f, 0.0f ))
    val farmland        = new Element("Farmland",       true,  new Color( 0.5f, 0.3f, 0.3f ))
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
        var inWay = false
    }
    
    class Way() extends Taggable
    {
        var nodes = mutable.ArrayBuffer[Node]()
    }
    
    class Canvas( val nodes : Iterable[Node], val ways : Iterable[Way] ) extends JFrame
    {
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
        
        val layered = mutable.ListBuffer[(Int, PPath, Taggable)]()
        for ( w <- ways )
        {
            if ( !w.nodes.isEmpty )
            {
                val xs = w.nodes.map( _.lon.toFloat ).toArray
                val ys = w.nodes.map( -_.lat.toFloat ).toArray
                
                val wood = w.has( "natural", "wood" ) || w.has( "landuse", "forest" )
                val highway = w.has( "highway" )
                val building = w.has( "building" ) || w.has( "landuse", "residential" )
                val waterway = w.has( "waterway", "riverbank" ) || w.has("natural", "water") || w.has("natural", "coastline")
                val garden = w.has("residential", "garden" ) || w.has("leisure", "common") || w.has("leisure", "park") || w.has("landuse", "grass") || w.has("landuse", "meadow") || w.has("leisure", "pitch") || w.has( "leisure", "recreation_ground") || w.has( "landuse", "recreation_ground") || w.has( "landuse", "farmland") || w.has( "leisure", "nature_reserve")
                val field = w.has("landuse", "field") || w.has("landuse", "farm")
                val closed = wood || building || waterway || garden || field
                
                val layer = if ( w.has("layer") ) w.keys("layer").filter( _ != '+' ).toInt
                else if ( closed ) -1
                else 0
                
                val node = new PPath()
                node.setPathToPolyline( xs, ys )
                if (closed)
                {
                    val col = if ( wood ) new java.awt.Color( 0.0f, 0.6f, 0.0f )
                    else if ( building ) new java.awt.Color( 0.9f, 0.9f, 0.9f )
                    else if ( waterway ) new java.awt.Color( 0.5f, 0.5f, 1.0f )
                    else if ( garden ) new java.awt.Color( 0.0f, 1.0f, 0.0f )
                    else if ( field ) new java.awt.Color( 0.5f, 0.3f, 0.3f )
                    else new java.awt.Color( 0.8f, 0.8f, 0.8f )
                    node.setPaint( col )
                    node.closePath()
                }
                else
                {
                    node
                }
                
                var dashPattern : Option[Array[Float]] = None
                val lineCol = if ( highway )
                {
                    val htype = w.keys("highway")
                    if ( htype == "path" || htype == "track" || htype == "footway" || htype == "cycleway" || htype == "bridleway" )
                    {
                        dashPattern = Some( Array( 5.0f, 5.0f ) )
                        if (htype == "cycleway" )
                        {
                            new java.awt.Color( 0.0f, 1.0f, 0.0f )
                        }
                        else
                        {
                            new java.awt.Color( 0.0f, 0.0f, 1.0f )
                        }
                        
                    }
                    else
                    {
                        new java.awt.Color( 0.3f, 0.0f, 0.0f )
                    }
                }
                else if ( w.has("railway") )
                {
                    dashPattern = Some( Array( 3.0f, 3.0f ) )
                    new java.awt.Color( 0.0f, 0.0f, 0.0f )
                }
                else if ( w.has("waterway" ) ) new java.awt.Color( 0.5f, 0.5f, 1.0f )
                else new java.awt.Color( 0.7f, 0.7f, 0.7f )
                
                dashPattern match
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
                
                node.setStrokePaint( lineCol )
                //canvas.getLayer().addChild(node)
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
            
            n.addInputEventListener( new PBasicInputEventHandler()
            {
                override def mousePressed( event : PInputEvent )
                {
                    tm.data = t.keys.toArray.map( x => Array( x._1, x._2 ) )
                    tm.fireTableDataChanged()
                }
                override def mouseDragged( event : PInputEvent ) {}
                override def mouseReleased( event : PInputEvent ) {}
                override def keyPressed( event : PInputEvent ) {}
            } )
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
            
            circ.addInputEventListener( new PBasicInputEventHandler()
            {
                override def mousePressed( event : PInputEvent )
                {
                    tm.data = node.keys.toArray.map( x => Array( x._1, x._2 ) )
                    tm.fireTableDataChanged()
                }
                override def mouseDragged( event : PInputEvent ) {}
                override def mouseReleased( event : PInputEvent ) {}
                override def keyPressed( event : PInputEvent ) {}
            } )
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
                    {
                        val theNode = nodes(ref)
                        theNode.inWay = true
                        currWay.nodes.append( theNode )
                    }
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
        // Oxford
        val f = new XMLFilter( args(0), new Bounds(-1.4558, -1.1949, 51.6554, 51.8916) )
        
        // Stockholm
        //val f = new XMLFilter( args(0), new Bounds(17.638, 18.47, 59.165, 59.502) )
        
        // West Chilterns
        //val f = new XMLFilter( args(0), new Bounds(-1.0436, -0.8356, 51.5668, 51.6656) )
        
        val c = new Canvas( f.nodes.view.map( _._2 ), f.ways )
    }
}


