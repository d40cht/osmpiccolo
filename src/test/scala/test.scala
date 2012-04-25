import org.scalatest.FunSuite

import scala.collection.{mutable, immutable}

class Foo( val inc : Int )
{
    implicit def strToIncInt( str : String ) = str.toInt + inc
}

class Test extends FunSuite
{
    test("Serialization")
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
}
