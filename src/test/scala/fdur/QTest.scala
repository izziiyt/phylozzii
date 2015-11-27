package fdur

import java.io.PrintWriter

import main.{PostProcess, QReducer, QMapper}
import org.scalatest.FunSuite
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.Files.copy
import java.nio.file.Paths.get

class QTest extends FunSuite{
  implicit def toPath (filename: String) = get(filename)

  val nh = "src/test/resources/fdur/test3.nh"
  val maf = "src/test/resources/fdur/tmp.maf"
  val param = "src/test/resources/fdur/param.txt"
  test("QMapper"){
    copy ("src/test/resources/fdur/test3.nh.bu", "src/test/resources/fdur/test3.nh", REPLACE_EXISTING)
    copy ("src/test/resources/fdur/.param.txt", "src/test/resources/fdur/param.txt", REPLACE_EXISTING)
    val x  = new PrintWriter("tmp/lgl.txt")
    x.close()
    for(_ <- 0 to 100) {
      QMapper.main(Array("em", maf, param, nh, "tmp/suffstat/hoge.txt"))
      QReducer.main(Array("em", "tmp/suffstat", param, nh))
    }
    PostProcess.main(Array("em",param,nh))
    val parameter = Parameters.fromFile(param)
    val tree = ModelTree.fromFile(nh)
    /*val result = Optimizer.em(100,nh,maf,param)
    println("Qbranch " + tree.branches)
    println("branch  " + result._1)
    println("Qparam " + parameter)
    println("param  " + result._2)
    */
  }
}
