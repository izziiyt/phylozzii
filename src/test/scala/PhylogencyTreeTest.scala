import breeze.linalg.{sum, DenseMatrix,DenseVector,eigSym,diag}
import org.scalatest.FunSuite
import math.{abs,log}

class PhylogencyTreeTest extends FunSuite {
  test("breeze.linalg"){
    val a = DenseMatrix((1,2),(3,4))
    val b = DenseVector(1,2)
    val c = DenseMatrix((1.0,2.0),(2.0,1.0))
    val d = DenseMatrix(
      (0,1,2,3),
      (0,0,4,5),
      (0,0,0,6),
      (0,0,0,0))
    assert(sum(a) == 10)
    assert(sum(a(0,::).t) == 3)
    assert(b.map(_ * 2) == DenseVector(2,4))
    assert(a :/ a == DenseMatrix((1,1),(1,1)))
    val (lambda:DenseVector[Double],r:DenseMatrix[Double]) = eigSym(c)
    val tmp = for(i <- 0 to 2;j <- i+1 to 3) yield d(i,j)
    assert(tmp == IndexedSeq(1,2,3,4,5,6))
  }

  test("gradient descent"){
    val (param,branch) = numerical()
    val a = analyticalPi()
    val b = analyticalB()
    val c = analyticalT()
    assert(abs(param.pi(1)-a) < math.exp(-5))
    assert(abs(param.a - b) < math.exp(-5))
    println(branch)
    println(c)
    assert(abs(branch(2) - c) < math.exp(-5))
  }
  def numerical()  = {
    val source = "src/test/resources/sample.nh"
    val pt = new PhylogencyTree(Tree(source),GTR())
    pt.root.setAlignment(List[Char](2,3,1))
    pt.inside(pt.root)
    pt.outside(pt.root)
    val likelihood = pt.root.likelihood(pt.model)
    pt.root.setPosterior(likelihood,pt.model)
    pt.deriveLL
  }

  def prepare(pt:PhylogencyTree):Double = {
    pt.root.setAlignment(List[Char](2,3,1))
    pt.inside(pt.root)
    println(log(pt.root.likelihood(pt.model)))
    log(pt.root.likelihood(pt.model))
  }

  def analyticalPi():Double = {
    val h = 0.001
    val PlusModel = GTR(Parameters(DenseVector[Double](1.0/12.0,2.0/12.0,3.0/12.0,1.0/12.0,2.0/12.0,3.0/12.0),
      DenseVector[Double](0.1,0.2+h/2,0.3,0.4)))
    val MinsModel = GTR(Parameters(DenseVector[Double](1.0/12.0,2.0/12.0,3.0/12.0,1.0/12.0,2.0/12.0,3.0/12.0),
      DenseVector[Double](0.1,0.2-h/2,0.3,0.4)))
    val source = "src/test/resources/sample.nh"
    val Pluspt = new PhylogencyTree(Tree(source),PlusModel)
    val Minspt = new PhylogencyTree(Tree(source),MinsModel)
    (prepare(Pluspt) - prepare(Minspt)) / h
  }

  def analyticalB():Double = {
    val h = 0.01
    val PlusModel = GTR(Parameters(DenseVector[Double](1.0/12.0+h/2,2.0/12.0,3.0/12.0,1.0/12.0,2.0/12.0,3.0/12.0),
      DenseVector[Double](0.1,0.2,0.3,0.4)))
    val MinsModel = GTR(Parameters(DenseVector[Double](1.0/12.0-h/2,2.0/12.0,3.0/12.0,1.0/12.0,2.0/12.0,3.0/12.0),
      DenseVector[Double](0.1,0.2,0.3,0.4)))
    val source = "src/test/resources/sample.nh"
    val Pluspt = new PhylogencyTree(Tree(source),PlusModel)
    val Minspt = new PhylogencyTree(Tree(source),MinsModel)
    (prepare(Pluspt) - prepare(Minspt)) / h
  }

  def analyticalT():Double = {
    val h = 0.01
    val source = "src/test/resources/sample.nh"
    val Pluspt = new PhylogencyTree(Tree(source),GTR())
    val Minspt = new PhylogencyTree(Tree(source),GTR())
    Pluspt.setBranch(List(0.5,0.4,0.3+h/2,0.2,0.0))
    Minspt.setBranch(List(0.5,0.4,0.3-h/2,0.2,0.0))
    (prepare(Pluspt) - prepare(Minspt)) / h
  }


  test("inside&outside"){
    val source = "src/test/resources/sample.nh"
    val pt = new PhylogencyTree(Tree(source),GTR(Parameters(DenseVector[Double](1.0/6.0,1.0/6.0,1.0/6.0,1.0/6.0,1.0/6.0,1.0/6.0),
      DenseVector[Double](1.0,2.0,3.0,4.0))))
    val tmp = DenseMatrix(
      (1.0,2.0,3.0,4.0),
      (2.0,1.0,4.0,3.0),
      (3.0,4.0,1.0,2.0),
      (4.0,3.0,2.0,1.0))
    def hoge(tree:Tree){
      tree match {
        case Node(left,right,cont) =>
          cont.manipulateTransition(tmp)
          hoge(right)
          hoge(left)
        case Leaf(_,cont) =>
          cont.manipulateTransition(tmp)
      }
    }
    hoge(pt.root)
    pt.root.setAlignment(List[Char](2,3,1))
    pt.inside(pt.root)
    pt.outside(pt.root)
    assert(pt.root.cont.alpha == DenseVector(150.0, 240.0, 50.0, 120.0))
    assert(pt.root.left.cont.beta == DenseVector(1500.0, 1680.0, 1100.0, 1200.0))
    val likelihood = pt.root.likelihood(pt.model)
    assert(likelihood == 1260.0)
    pt.root.setPosterior(likelihood,pt.model)
  }

  test("derivation"){
    val x = Parameters(DenseVector[Double](1,5,1,1,1,1),
      DenseVector[Double](0.25,0.25,0.25,0.25))
    val y = Parameters(DenseVector[Double](1,1,1,1,1,1),
      DenseVector[Double](0.25,0.25,0.25,0.25))
    val z = Parameters(DenseVector[Double](1,1,1,1,1,1),
      DenseVector[Double](0.25,0.45,0.25,0.25))
    assert(x + y + z == Parameters(DenseVector(3.0, 7.0, 3.0, 3.0, 3.0, 3.0),DenseVector(0.75, 0.95, 0.75, 0.75)))
  }
}
