package org.seacourt.osm

import org.geotools.geometry.{DirectPosition2D}
import scala.collection.{mutable, immutable}

import sbinary._
import sbinary.Operations._


final case class Pos( val x : Double, val y : Double )

object Pos
{
    def apply( dp : DirectPosition2D ) = new Pos(dp.x, dp.y)
}

final case class RouteNode( val pos : Pos )
{
    val edges = mutable.ArrayBuffer[RouteEdge]()
}

final case class RouteEdge( val from : RouteNode, val to : RouteNode, val length : Double, val rawLength : Double, val points : Array[Pos] )
{
    from.edges.append(this)
    to.edges.append(this)
    
    def other( node : RouteNode ) =
    {
        if ( node == from ) to
        else
        {
            assert( node == to )
            from
        }
    }
}

final case class LabelledNode( val node : RouteNode, val dist : Double = scala.Double.MaxValue )
{
}

class RouteGraph( val nodes : Array[RouteNode], val edges : Array[RouteEdge] )
{
    def shortestPath( from : RouteNode, to : RouteNode ) : List[(RouteNode, Double)] =
    {
        // Currently Dijkstra: improve to A*
        type FHNode = org.jgrapht.util.FibonacciHeapNode[RouteNode]
        class NodeInfo( var parent : Option[NodeInfo], val fhn : FHNode )
        
        val nodeMap = new java.util.HashMap[RouteNode, NodeInfo]()
        val heap = new org.jgrapht.util.FibonacciHeap[RouteNode]
        
        def newNode( n : RouteNode, dist : Double, parent : Option[NodeInfo] )
        {
            val fhn = new FHNode(n)
            heap.insert( fhn, dist )
            nodeMap.put( n, new NodeInfo( parent, fhn ) )
        }
        
        newNode( from, 0.0, None )
        
        while (!heap.isEmpty)
        {
            val top = heap.min
            heap.removeMin
            
            for ( e <- top.getData.edges )
            {
                val topDist = top.getKey
                val topN = top.getData
                val ndist = topDist + e.length
                val destN = e.other(topN)
                val topNI = nodeMap.get( topN )
                
                if ( !nodeMap.containsKey(destN) )
                {
                    newNode( destN, ndist, Some(topNI) )
                }
                else
                {
                    val ni = nodeMap.get( destN )
                    if ( ndist < ni.fhn.getKey )
                    {
                        heap.decreaseKey( ni.fhn, ndist )
                        ni.parent = Some(topNI)
                    }
                }
                
                if ( destN == to )
                {
                    type Path = List[(RouteNode, Double)]
                    def routePath( nio : Option[NodeInfo], path : Path ) : Path =
                    {
                        nio match
                        {
                            case Some(ni)   => routePath( ni.parent, (ni.fhn.getData, ni.fhn.getKey)::path)
                            case None       => path
                        }
                    }
                    val ni = Some(nodeMap.get(destN))
                    
                    return routePath( ni, Nil )
                }
            }
        }
        
        List()
    }
}

object SerializationProtocol extends sbinary.DefaultProtocol
{
    implicit object PosFormat extends Format[Pos]
    {
        def reads(in : Input) = new Pos( read[Double](in), read[Double](in) )
        def writes( out : Output, p : Pos ) = { write(out, p.x); write(out, p.y) }
    }
    
    implicit object RouteNodeFormat extends Format[RouteNode]
    {
        def reads(in : Input) =
        {
            val rn = new RouteNode( read[Pos](in) )
            //val size = read[Int](in)
            //(0 until size).foreach( i => rn.edges.append( RouteEdgeFormat.reads(in) ) )
            rn
        }
        def writes( out : Output, rn : RouteNode ) =
        {
            write(out, rn.pos)
            //write(out, rn.edges.size)
            //rn.edges.foreach( e => RouteEdgeFormat.writes(out, e) )
        }
    }
    
    implicit object RouteEdgeFormat extends Format[RouteEdge]
    {
        def reads(in : Input) =
        {
            // val from : RouteNode, val to : RouteNode, val length : Double, val points : Array[Pos]
            val from = read[RouteNode](in)
            val to = read[RouteNode](in)
            val length = read[Double](in)
            val rawLength = read[Double](in)
            val plen = read[Int](in)
            val points = (0 until plen).map( i => read[Pos](in) ).toArray
            new RouteEdge( from, to, length, rawLength, points )
        }
        
        def writes( out : Output, re : RouteEdge )
        {
            write( out, re.from )
            write( out, re.to )
            write( out, re.length )
            write( out, re.rawLength )
            write( out, re.points.length )
            re.points.foreach( p => write(out, p) )
        }
    }
    
    implicit object RouteGraphFormat extends Format[RouteGraph]
    {
        def reads(in : Input) =
        {
            val numNodes = read[Int](in)
            val nodes = (0 until numNodes).map( i => read[RouteNode](in) ).toArray
            val numEdges = read[Int](in)
            val edges = (0 until numEdges).map( i => read[RouteEdge](in) ).toArray
            
            new RouteGraph( nodes, edges )
        }
        def writes( out : Output, g : RouteGraph )
        {
            write( out, g.nodes.size )
            g.nodes.foreach( n => write( out, n ) )
            write( out, g.edges.size )
            g.edges.foreach( e => write( out, e ) )
        }
    }
}

