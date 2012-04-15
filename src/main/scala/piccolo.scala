package org.seacourt.osm

import javax.swing.JFrame
import scala.collection.{mutable, immutable}

import edu.umd.cs.piccolo.PCanvas
import edu.umd.cs.piccolo.nodes.{PText, PPath}
import edu.umd.cs.piccolo.event.{PBasicInputEventHandler, PInputEvent}

import org.jgrapht.graph._

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
            val xs = w.nodes.map( _.pos.x.toFloat ).toArray
            val ys = w.nodes.map( -_.pos.y.toFloat ).toArray
            
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
        val circ = PPath.createEllipse( node.pos.x.toFloat, -node.pos.y.toFloat, 3.0f, 3.0f )
        canvas.getLayer().addChild(circ)
        
        if ( false && node.has("name") )
        {
            val label = new PText()
            label.setBounds( node.pos.x.toFloat, -node.pos.y.toFloat, 0, 0 )
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
        val circ = PPath.createEllipse( node.pos.x.toFloat-3.0f, (-node.pos.y.toFloat)-3.0f, 6.0f, 6.0f )
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


class RouteGraph( xf : OSMReader )
{
    type GraphT = SimpleWeightedGraph[Node, DefaultWeightedEdge]
    
    val graph = new GraphT(classOf[DefaultWeightedEdge])
    
    private def nodeDist( n1 : Node, n2 : Node ) =
    {
        import scala.math._

        val dx = n1.pos.x - n2.pos.x
        val dy = n1.pos.y - n2.pos.y
        
        sqrt( dx*dx + dy*dy )
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
