import org.scalatest.FunSuite
import java.io.File

import biformat.BedIterator
import phylozzii.core.{Enhancer, Others}

import scala.io.Source
/**
  * Created by yuto on 15/12/17.
  */
class EnhancerTest extends FunSuite{
  /*test("nontest"){
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
  }*/

  test("is sorted") {
    val bedf = new File("core/src/test/resources/enhancer_tss_associations.chr21.bed")
    val beds = Source.fromFile(bedf)
    val tmp = Others.enhancerConcat(BedIterator.fromSource(beds).map(Enhancer(_))).map(_.toBedLine)
      .foldLeft(0){case (n, e) => if(e.start > n) e.start else Int.MaxValue}
    assert(tmp != Int.MaxValue)
  }
}
