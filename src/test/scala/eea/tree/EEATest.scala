package eea.tree

import alignment.Base._
import alignment.Base
import breeze.linalg.{diag, DenseMatrix, DenseVector}
import fdur.{Model, ModelTree, Maf, Parameters}
import org.scalatest.FunSuite

class EEATest extends FunSuite with LDTreeUtilTrait{

  /*test("BLSer"){
    main.BLSer.main(Array("src/test/resources/eea/output.maf",
      "src/test/resources/eea/blstest.nh",
      "src/test/resources/eea/blstest.param",
    "hg19","target/result.wig",
    "src/test/resources/hg19.100way.nh"))
  }*/

  test("mkCols"){
    val mu = Maf.MafUnitIterator.fromMSA("src/test/resources/eea/huga3.maf")
    val tree = ModelTree.fromFile("src/test/resources/eea/huga3.nh")
    val cols = mu.map{
      x =>
        main.BLSer.mkCol(x,tree.names,"hg19")
    }
    //cols.foreach(x => x.foreach(y => println(y.mkString(""))))
    val model = Model(Parameters.fromFile("src/test/resources/eea/blstest.param"))
    val hoge = cols.map(c => LDTree.bls(tree,model,c,"hg19"))
    hoge.foreach(x => println(x.mkString(",")))
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
    val tree = fdur.ModelTree.fromFile("src/test/resources/eea/hoge3.nh")
    val cols:List[Array[Base]] = List(Array(Base.C),Array(Base.G),Array(Base.T))
    val target = "hg18"
    val ldtree = LDTree.inout(tree,model,cols,target)
    val fldtree = fdur.LDTree.inout(tree,model,cols)
    //setTransProb(tree)
    //println(DenseMatrix((1.0,2.0,3.0,4.0),(4.0,1.0,2.0,3.0),(3.0,4.0,1.0,2.0),(2.0,3.0,4.0,1.0)))
    val tmp = (0 to 3) map {x => if(x == C.toInt) 1.0 else 0.0}
    import fdur._
    (ldtree.toList,fldtree.toList).zipped.foreach((e,f) => assert(e.alpha.head == f.alpha.head))
    //(ldtree.toList,fldtree.toList).zipped.foreach((e,f) => assert(e.beta.head == f.beta.head))
    /*ldtree.toList.foreach{x =>
      println(x.trans.value)
      println(x.transD.value)
      println(x.alpha.head.value)
      println(x.alphaD.head.value)
      println(x.beta.head.value)
      println(x.betaD.head.value)
      println()
    }*/
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

  test("inside&outside2"){
    val model = fdur.Model(Parameters(DenseVector(0.3,0.2,0.1,0.4,0.5,0.9),
      DenseVector(1.0,2.0,3.0,4.0)))
    val tree = fdur.ModelTree.fromFile("src/test/resources/eea/hoge5.nh")
    val cols: List[Array[Base]] = List(Array(Base.C),Array(Base.C),Array(Base.C),Array(Base.C),Array(Base.C))
    val target = "alpha"
    val ldtree = LDTree.inout(tree,model,cols,target)
    val fldtree = fdur.LDTree.inout(tree,model,cols)
    (ldtree.toList,fldtree.toList).zipped.foreach((e,f) => assert(e.alpha.head == f.alpha.head))
    //ldtree.toList.foreach(e => println(e.alpha.head.toArray.map(_.value).mkString(",")))
    val x = LDTree.bls(tree,model,cols,"alpha")
    //println(x.mkString(","))
    //println(tree.branches.sum)
    //(ldtree.toList,fldtree.toList).zipped.foreach((e,f) => assert(e.beta.head == f.beta.head))

  }

}
