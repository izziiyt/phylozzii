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
    //should be similar
    //println(c)
    //println(r * diag(lambda) * r.t)
  }

  test("gradient descent"){
    val n = numerical()
    val a = analytical()
    println("analyze " + a)
    println("numerical " + n)
    println(abs(n-a))
  }

  def numerical():Double = {
    val source = "src/test/resources/sample.nh"
    val pt = new PhylogencyTree(Tree(source),GTR())
    // prepare(pt)
    pt.root.setAlignment(List[Char](2,3,3))
    pt.inside(pt.root)
    pt.outside(pt.root)
    val likelihood = pt.root.likelihood(pt.model)
    println(log(likelihood))
    pt.root.setPosterior(likelihood,pt.model)
    val (param,_) = pt.deriveLL()
    param.pi(2)
  }

  def prepare(pt:PhylogencyTree):Double = {
    pt.root.setAlignment(List[Char](2,3,3))
    pt.inside(pt.root)
    println(log(pt.root.likelihood(pt.model)))
    log(pt.root.likelihood(pt.model))
  }

  def analytical():Double = {
    val h = 0.001
    val x = 1.0/6.0
    val PlusModel = GTR(Parameters(DenseVector[Double](x,x,x,x,x,x),
      DenseVector[Double](0.25,0.25,0.25+h/2,0.25)))
    val MinsModel = GTR(Parameters(DenseVector[Double](x,x,x,x,x,x),
      DenseVector[Double](0.25,0.25,0.25-h/2,0.25)))
    val source = "src/test/resources/sample.nh"
    val Pluspt = new PhylogencyTree(Tree(source),PlusModel)
    val Minspt = new PhylogencyTree(Tree(source),MinsModel)
    (prepare(Pluspt) - prepare(Minspt)) / h
  }
}
