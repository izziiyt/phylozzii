package phylozzii.branco

import java.io.File

import alignment.Base
import biformat.WigIterator
import biformat.WigIterator.WigUnit
import breeze.linalg.{DenseMatrix, DenseVector, diag}
import org.scalatest.FunSuite
import phylozzii.fdur
import phylozzii.fdur._
import phylozzii.fdur.util._

class BrancoTest extends FunSuite with LDTreeUtilTrait{

  test("main"){
    Main.main(Array(
      "-r",
      "-t", "hg19",
      "-o", "target/sample",
      "branco/src/test/resources/test3.param",
      "branco/src/test/resources/test3.nh",
      "branco/src/test/resources/test3.maf"
    ))
    diff(new File("target/sample.bls.wig"), new File("branco/src/test/resources/.sample.bls.wig"))
    diff(new File("target/sample.blsa.wig"), new File("branco/src/test/resources/.sample.blsa.wig"))
    def diff(f1: File, f2: File): Unit = {
      val bls1 = WigIterator.fromSource(biformat.bigSource(f1)).toArray
      val bls2 = WigIterator.fromSource(biformat.bigSource(f2)).toArray

      (bls1 zip bls2).foreach{
        case (x: WigUnit, y: WigUnit) =>
          (x.toVariableStep.lines zip y.toVariableStep.lines).foreach{
            case (p, q) =>
              assert(p._1 == q._1 && doubleEqual(p._2, q._2))
          }
      }
    }

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
    val tree = fdur.ModelTree.fromFile("branco/src/test/resources/testinout.nh")
    val cols:List[Array[Base]] = List(Array(Base.C),Array(Base.G),Array(Base.T))
    val target = "hg19"
    val ldtree = LDTree.inout(tree,model,cols,target)
    val fldtree = fdur.LDTree.inout(tree,model,cols)
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
    (ldtree.toList,ians).zipped.foreach{(x,y) => assert(doubleEqual(x.alphaD.head.value, y, 1.0E-6))}
    (ldtree.toList,oans).zipped.foreach{(x,y) => assert(doubleEqual(x.betaD.head.value, y, 1.0E-6))}
  }


}
