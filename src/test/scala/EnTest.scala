import fdur.ModelTree
import org.scalatest.FunSuite

import java.io.File
/**
  * Created by yuto on 15/12/17.
  */
class EnTest extends FunSuite{
  test("nontest"){
    val f = new File("src/test/resources/hg19.100way.izzii.nh")
    val tree = ModelTree.fromFile(f)
    val sum = tree.anclen("Human")
    val tmp = tree.anclenList("Human").map(_ / sum)
    println(tmp.mkString(","))
    println(tmp.length)
    val tmp2 = tree.subTreeLenList("Human")
    println(tmp2.map(_ / tmp2.last).mkString(","))
    println(tmp2.length)
    println(tree.anclenList("Human"))
    println(tmp2)
  }
}
