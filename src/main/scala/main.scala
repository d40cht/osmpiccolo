import javax.swing.JFrame

import edu.umd.cs.piccolo.PCanvas
import edu.umd.cs.piccolo.nodes.{PText, PPath}

import scala.util.Random.{nextInt, nextFloat}
import scala.collection.{mutable, immutable}

object TestRunner extends App
{
    class Canvas extends JFrame
    {
        val canvas = new PCanvas()
        val text = new PText("Boom")
        
        canvas.getLayer().addChild(text)
        add(canvas)
        
        val xs = mutable.ArrayBuffer[Float]()
        val ys = mutable.ArrayBuffer[Float]()
        
        var px = 300
        var py = 200
        for ( j <- 0 until 20 )
        {
            for ( i <- 0 until 100 )
            {
                val x = nextInt(600)
                val y = nextInt(400)
                
                //val node = PPath.createEllipse( x, y, 20, 20 )
                
                //canvas.getLayer().addChild(node)
                px += nextInt(21)-10
                py += nextInt(21)-10
                xs.append( px )
                ys.append( py )
            }
            val node = PPath.createPolyline( xs.toArray, ys.toArray )
            node.setStroke( new java.awt.BasicStroke( 3.0f ) )
            node.setPaint( new java.awt.Color( nextFloat, nextFloat, nextFloat ) )
            canvas.getLayer().addChild(node)
        }

        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        setSize(600, 400)
        setVisible(true)
    }
    
    override def main( args : Array[String] ) =
    {
        val c = new Canvas()
    }
}

