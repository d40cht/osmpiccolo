package org.seacourt.osm

import org.geotools.geometry.{DirectPosition2D}
import scala.collection.{mutable, immutable}

import sbinary._
import sbinary.Operations._


case class Pos( val x : Double, val y : Double )

object Pos
{
    def apply( dp : DirectPosition2D ) = new Pos(dp.x, dp.y)
}

case class RouteNode( val pos : Pos )
{
    val edges = mutable.ArrayBuffer[RouteEdge]()
}

case class RouteEdge( val from : RouteNode, val to : RouteNode, val length : Double, val points : Array[Pos] )
{
    from.edges.append(this)
    to.edges.append(this)
}

class RouteGraph( val nodes : Array[RouteNode] )
{
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
            val plen = read[Int](in)
            val points = (0 until plen).map( i => read[Pos](in) ).toArray
            new RouteEdge( from, to, length, points )
        }
        
        def writes( out : Output, re : RouteEdge )
        {
            write( out, re.from )
            write( out, re.to )
            write( out, re.length )
            write( out, re.points.length )
            re.points.foreach( p => write(out, p) )
        }
    }
    
    implicit object RouteGraphFormat extends Format[RouteGraph]
    {
        def reads(in : Input) =
        {
            val numNodes = read[Int](in)
            new RouteGraph( (0 until numNodes).map( i => read[RouteNode](in) ).toArray )
        }
        def writes( out : Output, g : RouteGraph )
        {
            write( out, g.nodes.size )
            g.nodes.foreach( n => write( out, n ) )
        }
    }
}

