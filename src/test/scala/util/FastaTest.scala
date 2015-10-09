package util

import java.io.PrintWriter
import breeze.linalg.DenseVector
import breeze.plot._

import scala.io.Source
import fdur.ModelTree
import org.scalatest.FunSuite

class FastaTest extends FunSuite{
  /*test("sample.aa and sample.nuc") {
    val aa = "src/test/resources/util/sample.aa"
    val nuc = "src/test/resources/util/sample.nuc"
    val species = "src/test/resources/util/sample.species.txt"
    val nh = "src/test/resources/hg19.100way.nh"
    val cdn = "src/test/resources/codon.table.txt"
    val target = "target/result.txt"
    FdFilter.main(Array(nuc, aa, species, nh, cdn, target))
    //23 32 35
    //3 6 9 12 15 24 27 33 36 42 48 60 63 69 78 81 84 99 102 111 114 123
  }
  test("in main"){
    val target = "target/nh/"
    var nh = ModelTree.fromFile("src/test/resources/fdur/fdur.nh")
    for(_ <- 0 to 40){
      val w = new PrintWriter(target + nh.leafLength + ".nh")
      println(nh.leafLength)
      w.println(nh)
      w.close()
      nh = nh.init
    }
  }*/
  test("diff"){
    for(i <- 2 to 32){
      try{
        val bs = Source.fromFile("/home/yuto/tmp/b/" + i + "b.txt")
        val b = bs.getLines().toArray.map{
          case xs => xs.split(",").map(_.toDouble)
        }
        val names = Array("em", "ldem", "gd", "ldgd")
        val nh = ModelTree.fromFile("/home/yuto/resultnh/" + (i + 1) + ".nh")
        val f = Figure()
        val p = f.subplot(0)
        val x = DenseVector(Array.range(0, b.head.length).map(_.toDouble))
        p += plot(x, DenseVector(b(0)), '+')
        p += plot(x, DenseVector(b(1)), '.')
        p += plot(x, DenseVector(b(2)), '.')
        p += plot(x, DenseVector(b(3)), '.')
        p += plot(x, DenseVector(nh.branches.toArray),'+')
        p.xlabel = "branch index"
        p.ylabel = "value"
        bs.close()
      }
      catch{
        case e: Throwable => println(e)
      }
    }
  }
}
