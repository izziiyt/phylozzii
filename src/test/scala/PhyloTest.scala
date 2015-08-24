import breeze.linalg._
import fdur._
import org.scalatest.FunSuite
import math.log
import alignment.Base

class PhyloTest extends FunSuite {

  test("derivation"){
    val x = Parameters(DenseVector[Double](1,5,1,1,1,1),
      DenseVector[Double](0.25,0.25,0.25,0.25))
    val y = Parameters(DenseVector[Double](1,1,1,1,1,1),
      DenseVector[Double](0.25,0.25,0.25,0.25))
    val z = Parameters(DenseVector[Double](1,1,1,1,1,1),
      DenseVector[Double](0.25,0.45,0.25,0.25))

    assert(x + y + z == Parameters(DenseVector(3.0, 7.0, 3.0, 3.0, 3.0, 3.0),DenseVector(0.75, 0.95, 0.75, 0.75)))
  }

  test("branch"){
    val pt = new PhyloTreeTest(FdurTree.fromFile("src/test/resources/brown/brown.trees"),GTR())
    pt.setBranch(List(0.7,0.9,0.4,0.8))
    val x = pt.branches
    pt.setBranch(pt.branches)
    assert(util.doubleChecker(pt.branches,x))
  }

  test("breeze.linalg"){
    val a = DenseMatrix((1,2),(3,4))
    assert(a == new DenseMatrix(2,2,Array(1,3,2,4)))
    val b = DenseVector(1,2)
    val d = DenseMatrix(
      (0,1,2,3),
      (0,0,4,5),
      (0,0,0,6),
      (0,0,0,0))
    assert(sum(a) == 10)
    assert(sum(a(0,::).t) == 3)
    assert(b.map(_ * 2) == DenseVector(2,4))
    assert(a :/ a == DenseMatrix((1,1),(1,1)))
    val tmp = for(i <- 0 to 2;j <- i+1 to 3) yield d(i,j)
    assert(tmp == IndexedSeq(1,2,3,4,5,6))
  }


  val defPi = DenseVector[Double](0.1,0.2,0.3,0.4)
  val defB = DenseVector[Double](1.0/12.0,2.0/12.0,3.0/12.0,1.0/12.0,2.0/12.0,3.0/12.0)
  val defMod = GTR(Parameters(defB,defPi))
  val defBr = List(0.5,0.4,0.3,0.2,0.3,0.4,0.2,0.4,0.6,0.1,0.5,0.7)
  def tree = FdurTree.fromFile("src/test/resources/ce10.7way.nh")

  test("gradient descent"){
    val al = util.getAlignments("src/test/resources/test.al")
    val div = 0.001
    for(col <- al){
      val (param,branch) = numerical(col)
      val pi = analyticalPi(col,div)
      val b = analyticalB(col,div)
      val c = analyticalT(col,div)
      (param.pi.toArray,pi).zipped.foreach((x,y) => assert(!x.isNaN && !y.isNaN))
      (param.pi.toArray,pi).zipped.foreach{(x,y) => assert(util.doubleChecker(x,y,math.exp(-5)))}
      (param.Bvec.toArray,b).zipped.foreach((x,y) => assert(!x.isNaN && !y.isNaN))
      (param.Bvec.toArray,b).zipped.foreach((x,y) => assert(util.doubleChecker(x,y,math.exp(-5))))
      (branch,c).zipped.foreach((x,y) => assert(!x.isNaN && !y.isNaN))
      (branch,c).zipped.foreach((x,y) => assert(util.doubleChecker(x,y,math.exp(-5))))

    }
  }

  def numerical(colmn:Array[Base]) =
  {
    val pt = new PhyloTreeTest(tree, defMod)
    pt.setColumn(colmn)
    pt.setBranch(defBr)
    pt.inside()
    pt.outside()
    pt.setPosterior()
    pt.deriveLL
  }

  def prepare(pt:PhyloTreeTest, colmn:Array[Base]):Double =
  {
    pt.setColumn(colmn)
    pt.inside()
    log(pt.likelihood)
  }

  def analyticalPi(branch:Array[Base],h:Double):Seq[Double] =
    for (i <- 0 to 3) yield {
      val x = DenseVector.zeros[Double](4); x(i) = h/2
      val PlusModel = GTR(Parameters(defB, defPi + x))
      val MinsModel = GTR(Parameters(defB, defPi - x))
      val Pluspt = new PhyloTreeTest(tree, PlusModel)
      val Minspt = new PhyloTreeTest(tree, MinsModel)
      Pluspt.setBranch(defBr)
      Minspt.setBranch(defBr)
      (prepare(Pluspt, branch) - prepare(Minspt, branch)) / h
    }

  def analyticalB(branch:Array[Base],h:Double):Seq[Double] =
    for (i <- 0 to 5) yield {
      val x = DenseVector.zeros[Double](6); x(i) = h/2
      val PlusModel = GTR(Parameters(defB + x, defPi))
      val MinsModel = GTR(Parameters(defB - x, defPi))
      val Pluspt = new PhyloTreeTest(tree, PlusModel)
      val Minspt = new PhyloTreeTest(tree, MinsModel)
      Pluspt.setBranch(defBr)
      Minspt.setBranch(defBr)
      (prepare(Pluspt, branch) - prepare(Minspt, branch)) / h
    }

  def analyticalT(branch:Array[Base], h:Double):Seq[Double] =
    for (i <- defBr.indices) yield {
      val x = DenseVector.zeros[Double](defBr.length); x(i) = h/2
      val Pluspt = new PhyloTreeTest(tree, defMod)
      val Minspt = new PhyloTreeTest(tree, defMod)
      val p = defBr.zipWithIndex.map{case (a, j) => if(j == i) a + h/2 else a}
      val n = defBr.zipWithIndex.map{case (a, j) => if(j == i) a - h/2 else a}
      Pluspt.setBranch(p)
      Minspt.setBranch(n)
      (prepare(Pluspt, branch) - prepare(Minspt, branch)) / h
    }

  /*test("inside&outside"){
    val source = "src/test/resources/sample.nh"
    val pt = new PhyloTreeTest(FdurTree.fromFile(source),
      GTR(Parameters(DenseVector[Double](1.0/6.0,1.0/6.0,1.0/6.0,1.0/6.0,1.0/6.0,1.0/6.0),
      DenseVector[Double](1.0,2.0,3.0,4.0))))
    val tmp = DenseMatrix(
      (1.0,2.0,3.0,4.0),
      (2.0,1.0,4.0,3.0),
      (3.0,4.0,1.0,2.0),
      (4.0,3.0,2.0,1.0))
    def test.nh(tree:FdurTree){
      tree match {
        case FdurNode(left,right,cont) =>
          cont.manipulateTransition(tmp)
          test.nh(right)
          test.nh(left)
        case FdurLeaf(_,cont) =>
          cont.manipulateTransition(tmp)
      }
    }
    test.nh(pt.root)
    pt.root.setColumn(Array[Base](Base.C,Base.G,Base.A))
    pt.inside(pt.root)
    pt.outside(pt.root)
    assert(pt.root.cont.alpha == DenseVector(150.0, 240.0, 50.0, 120.0))
    assert(pt.root.left.cont.beta == DenseVector(1500.0, 1680.0, 1100.0, 1200.0))
    val likelihood = pt.root.likelihood(pt.model)
    assert(likelihood == 1260.0)
    pt.root.setPosterior(likelihood,pt.model)
  }*/
}

class PhyloTreeTest(r:FdurNode,m:EvolutionModel) extends PhylogencyTree(r,m){

  def deriveLL:(Parameters,List[Double]) = {
    val (lParam,lT) = deriveLL(root.left)
    val (rParam,rT) = deriveLL(root.right)
    val param = lParam + rParam
    val t = lT ::: rT
    val tmp = DenseVector((0 to 3).map(i => root.cont.posterior(i,i) / model.pi(i)).toArray)
    Pair(Parameters(param.Bvec,param.pi + tmp),t)
  }

  private def deriveLL(tree:FdurTree):(Parameters,List[Double]) = {
    val rs = for(i <- 0 to 3;j <- 0 to 3) yield deriveLWithLogR(i,j,tree.cont)
    val post = for(i <- 0 to 3;j <- 0 to 3) yield tree.cont.posterior(i,j)
    val ps:DenseVector[Double] = (rs,post).zipped.map((r,p) => deriveLWithPi(tree.cont,r) * p).reduceLeft(_ + _)
    val bs:DenseVector[Double] = (rs,post).zipped.map((r,p) => deriveLWithB(tree.cont,r) * p).reduceLeft(_ + _)
    val ts = (rs,post).zipped.map((r,p) => deriveLWithT(tree.cont,r) * p).sum

    tree match{
      case FdurNode(left,right,cont) =>
        val (lParam,lT) = deriveLL(left)
        val (rParam,rT) = deriveLL(right)
        val param = lParam + rParam + Parameters(bs,ps)
        val tlist:List[Double] = lT ::: rT ::: List(ts)
        Pair(param,tlist)
      case FdurLeaf(_,_) =>
        Pair(Parameters(bs,ps),List(ts))
    }
  }

  private def deriveLWithLogR(a:Int,b:Int,cont:Content):DenseMatrix[Double] = {
    val fd = cont.fdPremitive(model)
    val ns = cont.nsPremitive(model)
    ns(a)(b) - (diag(fd(a)(b)) * model.R.*(cont.t))
  }

  private def deriveLWithPi(cont:Content,r:DenseMatrix[Double]):DenseVector[Double] =
    DenseVector((0 to 3).map(i => sum(for(j <- 0 to 3;if j != i) yield r(j,i)) / model.pi(i)).toArray)

  private def deriveLWithB(cont:Content,r:DenseMatrix[Double]):DenseVector[Double] = {
    val tmp = (r + r.t) :/ model.B
    DenseVector((for(i <- 0 to 2;j <- i+1 to 3) yield tmp(i,j)).toArray)
  }

  private def deriveLWithT(cont:Content,r:DenseMatrix[Double]):Double = (sum(r) - trace(r)) / cont.t
}


