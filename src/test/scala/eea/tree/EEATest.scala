package eea.tree

import alignment.Base._
import alignment.Base
import breeze.linalg.{diag, DenseMatrix, DenseVector}
import breeze.numerics.log
import fdur.Parameters
import org.scalatest.FunSuite

class EEATest extends FunSuite with LDTreeUtilTrait{

  /*test("constructor"){
    val tree = fdur.ModelTree.fromFile("src/test/resources/brown.trees")
    assert(tree.names == List("Human","Chimpanzee","Gorilla"))
    /*def f(t:LDTree):List[Double] = t match {
      case LDLeaf(_,x) => List(x)
      case LDNode(l,r,x) => f(l) ++ f(r) ++ List(x)
      case LDRoot(l,r) => f(l) ++ f(r)
    }*/
    assert(tree.branches == List(0.2,0.1,0.5,0.3))
  }*/

  /*test("set_columns"){
    val tree = fdur.ModelTree.fromFile("src/test/resources/brown.trees")
    val column = List(C,A,G)
    tree.setColumn(column)
    assert(tree.column == column)
  }*/

  /*test("set_model"){
    val tree = fdur.ModelTree.fromFile("src/test/resources/brown.trees")
    tree.setModel(fdur.GTR())
    def f(t:Tree):List[EvolutionModel] = t match {
      case Leaf(_,_) => List(t.m)
      case Node(l,r,_) => f(l) ++ f(r) ++ List(t.m)
      case Root(l,r) => f(l) ++ f(r)
    }
    assert(f(tree).forall(_ == f(tree).head))
  }*/

  /*test("set_target"){
    val tree = fdur.ModelTree.fromFile("src/test/resources/brown.trees")
    //tree.setColumn(List(C,G,T))
    //tree.setTarget("Chimpanzee")
    /*def f(t:LDTree):List[LDTree] = t match {
      case LDLeaf(_,_) => List(t)
      case LDNode(ch,_) => f(l) ++ f(r)
      case LDRoot(,r) => f(l) ++ f(r)
    }*/

    val tmp = (0 to 3) map {x => if(x == G.toInt) 1.0 else 0.0}
    val l = LDTree.inout(tree,model,)
    assert(l.head.insideD == DenseVector.zeros[Double](4))
    assert(l(1).insideD == DenseVector(tmp.toArray))
    assert(l(2).insideD == DenseVector.zeros[Double](4))
  }*/

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
  test("hugahuga"){
    val model = fdur.Model(Parameters(DenseVector(0.3,0.1,0.1,0.2,0.2,0.1),
      DenseVector(1.0,2.0,3.0,4.0)))
    val tree = fdur.ModelTree.fromFile("src/test/resources/eea/hoge3.nh")
    val cols = fdur.Maf.readMaf("src/test/resources/fdur/small.maf",512)
    //val cols = Array[List[Array[Base]]](List(Array(Base.C),Array(Base.G),Array(Base.T)))
    val results = cols.map(c => LDTree.bls(tree,model,c,"hg18"))
    println(results.head.mkString(","))
  }
/*
  test("outside"){
    val tree = Tree.fromFile("src/test/resources/brown.trees")
    tree.setColumn(List(C,G,T))
    tree.setTarget("Human")
    tree.setModel(gtr)
    setTransProb(tree)
    assert(tree.outside == DenseVector(10.0,20.0,30.0,40.0))
    assert(tree.outsideD == DenseVector.zeros[Double](4))
    assert(tree.left.outside == DenseVector(520.0,280.0,1040.0,600.0))
    assert(tree.left.outsideD == DenseVector(0.0,0.0,0.0,0.0))
    assert(tree.right.outsideD == DenseVector(0.0,0.0,520.0,0.0))
  }
*/
  /*def setTransProb(t:Tree):Unit = {
    t match {
      case Leaf(_, _) => Unit
      case Node(l, r, _) => setTransProb(r);setTransProb(l)
      case Root(l, r) => setTransProb(l);setTransProb(r)
    }
    t.transProb = DenseMatrix((1.0,2.0,3.0,4.0),(4.0,1.0,2.0,3.0),(3.0,4.0,1.0,2.0),(2.0,3.0,4.0,1.0))
    t.transProbD = DenseMatrix((1.0,0.0,0.0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0.0,1.0))
  }*/
}
