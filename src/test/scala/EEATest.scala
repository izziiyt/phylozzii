import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import alignment.Base
import breeze.linalg.{DenseMatrix, DenseVector, diag}
import eea.tree.{LDTree, LDTreeUtilTrait}
import fdur.Parameters
import org.scalatest.FunSuite

import scala.io.Source

class EEATest extends FunSuite with LDTreeUtilTrait{

  test("BLSer"){
    val prefix = "target/result"
    val compare = "src/test/resources/eea/blsertest.wig.gz"
    main.BLSer.main(Array(
      "src/test/resources/eea/test3.maf",
      "src/test/resources/eea/blstest.nh",
      "src/test/resources/eea/blstest.param",
      "hg19",
      prefix,
      "src/test/resources/hg19.100way.nh"))
    val s1 = Source.fromInputStream(new GZIPInputStream(new FileInputStream(prefix + ".bls.wig.gz")))
    val s2 = Source.fromInputStream(new GZIPInputStream(new FileInputStream(compare)))
    assert(s1.getLines().reduce(_+_) == s2.getLines().reduce(_+_))
    s1.close()
    s2.close()
  }

  test("confound"){
    val x = List(1,2,3)
    val y = List(4,5,6)
    val tmp = mkConfound(x,y)
    val hoge = List(List(4,2,3),List(1,5,3),List(1,2,6))
    assert(tmp == hoge)
    val a = DenseVector(1,2,3)
    val b = DenseVector(4,5,6)
    val c = diag(a) * DenseMatrix.ones[Int](3,3) * diag(b)
    assert(c == DenseMatrix((4,5,6),(8,10,12),(12,15,18)))
    assert(a * b.t == DenseMatrix((4,5,6),(8,10,12),(12,15,18)))
  }

  test("inside && outside"){
    val model = fdur.Model(Parameters(DenseVector(0.3,0.1,0.1,0.2,0.2,0.1),
      DenseVector(1.0,2.0,3.0,4.0)))
    val tree = fdur.ModelTree.fromFile("src/test/resources/eea/test3.nh")
    val cols:List[Array[Base]] = List(Array(Base.C),Array(Base.G),Array(Base.T))
    val target = "hg18"
    val ldtree = LDTree.inout(tree,model,cols,target)
    val fldtree = fdur.LDTree.inout(tree,model,cols)
    import fdur._
    (ldtree.toList,fldtree.toList).zipped.foreach((e,f) => assert(e.alpha.head == f.alpha.head))
    val ians = List(
      DenseVector(0.0, 1.0, 0.0, 0.0),
      DenseVector(0.0, 0.0, 0.0, 0.0),
      DenseVector(0.0, 0.0, 0.0, 0.0),
      DenseVector(0.0, 0.003145705593451748, 0.0, 0.0)
    )
    val oans = List(
      DenseVector(0.0, 0.0, 0.0, 0.0),
      DenseVector(0.0, 0.12204610197844742, 0.0, 0.0),
      DenseVector(0.0, 0.09761985381121624, 0.0, 0.0),
      DenseVector(0.0, 0.0, 0.0, 0.0)
    )
    (ldtree.toList,ians).zipped.foreach{(x,y) => assert(x.alphaD.head.value == y)}
    (ldtree.toList,oans).zipped.foreach{(x,y) => assert(x.betaD.head.value == y)}
  }


}
