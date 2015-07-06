package eea.tree

import alignment.Base._
import breeze.linalg.{DenseMatrix, DenseVector}
import fdur.{Parameters, EvolutionModel,GTR}
import org.scalatest.FunSuite

class EEATreeTest extends FunSuite {

  test("constructor"){
    val tree = EEATree.fromFile("src/test/resources/brown.trees")
    assert(tree.names == List("Human","Chimpanzee","Gorilla"))
    def f(t:EEATree):List[Double] = t match {
      case EEALeaf(_,x) => List(x)
      case EEANode(l,r,x) => f(l) ++ f(r) ++ List(x)
      case EEARoot(l,r) => f(l) ++ f(r)
    }
    assert(f(tree) == List(0.2,0.1,0.5,0.3))
  }

  test("set_columns"){
    val tree = EEATree.fromFile("src/test/resources/brown.trees")
    val column = List(C,A,G)
    tree.setColumn(column)
    assert(tree.column == column)
  }

  test("set_model"){
    val tree = EEATree.fromFile("src/test/resources/brown.trees")
    tree.setModel(fdur.GTR())
    def f(t:EEATree):List[EvolutionModel] = t match {
      case EEALeaf(_,_) => List(t.m)
      case EEANode(l,r,_) => f(l) ++ f(r) ++ List(t.m)
      case EEARoot(l,r) => f(l) ++ f(r)
    }
    assert(f(tree).forall(_ == f(tree).head))
  }

  test("set_target"){
    val tree = EEATree.fromFile("src/test/resources/brown.trees")
    tree.setColumn(List(C,G,T))
    tree.setTarget("Chimpanzee")
    def f(t:EEATree):List[EEATree] = t match {
      case EEALeaf(_,_) => List(t)
      case EEANode(l,r,_) => f(l) ++ f(r)
      case EEARoot(l,r) => f(l) ++ f(r)
    }
    val tmp = (0 to 3) map {x => if(x == G.toInt) 1.0 else 0.0}
    val l = f(tree)
    assert(l.head.insideD == DenseVector.zeros[Double](4))
    assert(l(1).insideD == DenseVector(tmp.toArray))
    assert(l(2).insideD == DenseVector.zeros[Double](4))
  }

  val gtr = GTR(Parameters(DenseVector(0.3,0.1,0.1,0.2,0.2,0.1),DenseVector(10.0,20.0,30.0,40.0)))

  test("inside"){
    val tree = EEATree.fromFile("src/test/resources/brown.trees")
    tree.setColumn(List(C,G,T))
    tree.setTarget("Human")
    tree.setModel(gtr)
    setTransProb(tree)
    println(DenseMatrix((1.0,2.0,3.0,4.0),(4.0,1.0,2.0,3.0),(3.0,4.0,1.0,2.0),(2.0,3.0,4.0,1.0)))
    val human = getHuman(tree)
    val tmp = (0 to 3) map {x => if(x == C.toInt) 1.0 else 0.0}
    assert(human.inside == DenseVector(tmp.toArray))
    assert(human.insideD == DenseVector(tmp.toArray))
    assert(tree.left.inside == DenseVector(12.0,6.0,2.0,4.0))
    assert(tree.left.insideD == DenseVector(0.0,0.0,2.0,0.0))
  }

  test("outside"){
    val tree = EEATree.fromFile("src/test/resources/brown.trees")
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

  def getHuman(t:EEATree):EEATree = t match {
    case EEALeaf(_,_) => t
    case EEANode(l,_,_) => getHuman(l)
    case EEARoot(l,_) => getHuman(l)
  }

  def setTransProb(t:EEATree):Unit = {
    t match {
      case EEALeaf(_, _) => Unit
      case EEANode(l, r, _) => setTransProb(r);setTransProb(l)
      case EEARoot(l, r) => setTransProb(l);setTransProb(r)
    }
    t.transProb = DenseMatrix((1.0,2.0,3.0,4.0),(4.0,1.0,2.0,3.0),(3.0,4.0,1.0,2.0),(2.0,3.0,4.0,1.0))
    t.transProbD = DenseMatrix((1.0,0.0,0.0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0.0,1.0))
  }
}
