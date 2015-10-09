package main

import java.io._
import java.util.zip.GZIPInputStream
import alignment.Base

object Counter {
  def main(args:Array[String]):Unit = {
    val f = new File(args(0))
    val counts = Array.fill[Long](5)(0)
    val s = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(f))))
    var line = s.readLine()
    while(line != null){
      if ( line.head == 's') {
        val xs = line.split("\\s+")(6)
        xs.toCharArray.foreach(x => counts(Base.fromChar(x).toInt) += 1)
      }
      line = s.readLine()
    }
    val of = new PrintWriter("target/" + f.getName + ".ct.txt")
    of.println(counts.mkString(" "))
    of.close()
  }
}
