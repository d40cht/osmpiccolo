import org.scalatest.FunSuite

import scala.collection.{mutable, immutable}

import org.seacourt.osm._

import sbinary._
import sbinary.Operations._

class Foo( val inc : Int )
{
    implicit def strToIncInt( str : String ) = str.toInt + inc
}

class Test extends FunSuite
{
    test("Serialization structure")
    {
        {
            val f = new Foo(3)
            import f._
            
            val conv : Int = "6"
            assert( conv === 9 )
        }
        
        {
            val g = new Foo(4)
            import g._
            
            val conv : Int = "11"
            assert( conv === 15 )
        }
    }
    
    
    test("Serialization")
    {
        val nodes = Array(
            new RouteNode( new Pos( 1, 2 ) ),
            new RouteNode( new Pos( 3, 4 ) ),
            new RouteNode( new Pos( 5, 6 ) ),
            new RouteNode( new Pos( 7, 8 ) ) )

        val edges = Array(
            new RouteEdge( nodes(0), nodes(1), 1.0, 2.0, Array() ),
            new RouteEdge( nodes(0), nodes(2), 3.0, 4.0, Array() ),
            new RouteEdge( nodes(0), nodes(3), 5.0, 6.0, Array() ),
            new RouteEdge( nodes(1), nodes(2), 7.0, 8.0, Array() ),
            new RouteEdge( nodes(1), nodes(3), 9.0, 10.0, Array() ),
            new RouteEdge( nodes(2), nodes(3), 11.0, 12.0, Array() ) )
        
        val rg1 = new RouteGraph( nodes, edges )
        
        val s =
        {
            val serializer = new SerializationProtocol()
            import serializer._
            toByteArray(rg1)
        }
        
        val rg2 =
        {
            val serializer = new SerializationProtocol()
            import serializer._
            fromByteArray[RouteGraph](s)
        }
        
        assert( rg2.nodes.size === 4 )
        assert( rg2.edges.size === 6 )
    }
}
