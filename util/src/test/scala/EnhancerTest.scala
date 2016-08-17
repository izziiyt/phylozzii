import org.scalatest.FunSuite
import java.io.File

import biformat.BedIterator
import phylozzii.util.EnhancerIterator.Enhancer
import phylozzii.util.Others

import scala.io.Source
/**
  * Created by yuto on 15/12/17.
  */
class EnhancerTest extends FunSuite{

  /*test("is sorted") {
    val bedf = new File("util/src/test/resources/enhancer_tss_associations.chr21.bed")
    val beds = Source.fromFile(bedf)
    val tmp = Others.enhancerConcat(BedIterator.fromSource(beds).map(Enhancer(_))).map(_.toBedLine)
      .foldLeft(0){case (n, e) => if(e.start > n) e.start else Int.MaxValue}
    assert(tmp != Int.MaxValue)
  }*/
}
