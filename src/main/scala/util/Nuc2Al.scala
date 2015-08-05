package util

import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Nuc2Al {
  def apply(file:String,outPut:String){
    val s = Source.fromFile(file)
    val lines = s.getLines()
    lines.next()
    val buf = new ArrayBuffer[String]
    var tmp = ""

    for(line <- lines){
      line match{
        case x if x.length > 10 => tmp += line.trim
        case _ => if(! tmp.isEmpty){buf += tmp; tmp = ""}
      }
    }
    buf += tmp
    val out = new PrintWriter(outPut)
    for(i <- buf.head.indices){
      val x = for(b <- buf) yield b(i)
      out.println(x.mkString(" "))
    }
    out.close()
  }
}
