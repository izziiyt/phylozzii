import breeze.linalg.{DenseMatrix,DenseVector,sum,trace,diag}
import math.log

class PhylogencyTree(val root:Node,val model:EvolutionModel){

  root.setTransition(model)

  def this(nhFile:String,m:EvolutionModel) = this(Tree.fromFile(nhFile),m)

  def this(that:PhylogencyTree,m:EvolutionModel) = this(that.root.format(),m)

  def setBranch(x:List[Double]){
    val tmp = root.left.setBranch(x)
    root.right.setBranch(tmp)
    root.setTransition(model)
  }

  def count = Count(root.left.collectF(model) ::: root.right.collectF(model),
    root.left.collectN(model) ::: root.right.collectN(model),root.left.collectT ::: root.right.collectT,root.collectn(model),log(likelihood))

  def likelihood:Double = root.likelihood(model)

  def setColumn(al:Array[Char]){
    root.setColumn(al)
  }

  def setPosterior(){root.setPosterior(likelihood,model)}

  def branches() = root.left.branches ::: root.right.branches

  def inside(tree:Tree = root):DenseVector[Double] = {
    tree match{
      case Node(left,right,cont) =>
        val fromLeft = inside(left)
        val fromRight = inside(right)
        for(i <- 0 to 3){cont.alpha(i) = fromLeft(i) * fromRight(i)}
        cont.accumInsideBelief(model)
      case Leaf(_,cont) =>
        if(cont.nuc > 3){
          cont.alpha(0 to 3) := 1.0
        }else{
          cont.alpha(0 to 3) := 0.0
          cont.alpha(cont.nuc.toInt) = 1.0
        }
        cont.accumInsideBelief(model)
    }
  }

  def outside(tree:Tree = root,fromBro:DenseVector[Double] = model.pi,fromPar:DenseVector[Double] = DenseVector(1,1,1,1)){
    tree match{
      case Node(left,right,cont) =>
        for(i <- 0 to 3) cont.beta(i) = fromBro(i) * fromPar(i)
        innerOutside(left,right,cont)
      case Leaf(_,cont) =>
        for(i <- 0 to 3) cont.beta(i) = fromBro(i) * fromPar(i)
    }
  }

  private def innerOutside(left:Tree,right:Tree,cont:Content){
    val fromLeft = left.cont.accumInsideBelief(model)
    val fromRight = right.cont.accumInsideBelief(model)
    val fromThis = cont.accumOutsideBelief(model)
    outside(right,fromLeft,fromThis)
    outside(left,fromRight,fromThis)
  }

  def deriveLL:(Parameters,List[Double]) = {
    val (lParam,lT) = deriveLL(root.left)
    val (rParam,rT) = deriveLL(root.right)
    val param = lParam + rParam
    val t = lT ::: rT
    val tmp = DenseVector((0 to 3).map(i => root.cont.posterior(i,i) / model.pi(i)).toArray)
    Pair(Parameters(param.Bvec,param.pi + tmp),t)
  }

  private def deriveLL(tree:Tree):(Parameters,List[Double]) = {
    val rs = for(i <- 0 to 3;j <- 0 to 3) yield deriveLWithLogR(i,j,tree.cont)
    val post = for(i <- 0 to 3;j <- 0 to 3) yield tree.cont.posterior(i,j)
    val ps = (rs,post).zipped.map((r,p) => deriveLWithPi(tree.cont,r) * p).reduceLeft(_ + _)
    val bs = (rs,post).zipped.map((r,p) => deriveLWithB(tree.cont,r) * p).reduceLeft(_ + _)
    val ts = (rs,post).zipped.map((r,p) => deriveLWithT(tree.cont,r) * p).reduceLeft(_ + _)

    tree match{
      case Node(left,right,cont) =>
        val (lParam,lT) = deriveLL(left)
        val (rParam,rT) = deriveLL(right)
        val param = lParam + rParam + Parameters(bs,ps)
        val tlist:List[Double] = lT ::: rT ::: List(ts)
        Pair(param,tlist)
      case Leaf(_,_) =>
        Pair(Parameters(bs,ps),List(ts))
    }
  }

  private def deriveLWithLogR(a:Int,b:Int,cont:Content):DenseMatrix[Double] =
    cont.NsMati(a,b,model) - (diag(cont.FdVeci(a,b,model)) * model.R * cont.t)

  private def deriveLWithPi(cont:Content,r:DenseMatrix[Double]):DenseVector[Double] =
    DenseVector((0 to 3).map(i => sum(for(j <- 0 to 3;if j != i) yield r(j,i)) / model.pi(i)).toArray)

  private def deriveLWithB(cont:Content,r:DenseMatrix[Double]):DenseVector[Double] = {
    val tmp = (r + r.t) :/ model.B
    DenseVector((for(i <- 0 to 2;j <- i+1 to 3) yield tmp(i,j)).toArray)
  }

  private def deriveLWithT(cont:Content,r:DenseMatrix[Double]):Double = (sum(r) - trace(r)) / cont.t

}