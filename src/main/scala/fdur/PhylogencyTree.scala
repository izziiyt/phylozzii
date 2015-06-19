package fdur

import breeze.linalg.DenseVector
import scala.collection.mutable.ListBuffer
import scala.math._
import alignment.Base

class PhylogencyTree(val root:Node,val model:EvolutionModel){

  root.setTransition(model)

  def this(nhFile:String,m:EvolutionModel) = this(FdurTree.fromFile(nhFile),m)

  def this(that:PhylogencyTree,m:EvolutionModel) = this(that.root.format(),m)

  def setBranch(x:List[Double]){
    val tmp = root.left.setBranch(x)
    root.right.setBranch(tmp)
    root.setTransition(model)
  }

  def count = {
    val cList = mkTreeList.map(_.cont.nsAndFd(model))
    Count(cList.map(_._1),cList.map(_._2),root.collectn(model),log(likelihood))
  }

  def likelihood:Double = root.likelihood(model)

  def setColumn(al:Array[Base]) = root.setColumn(al)

  def setPosterior(){root.setPosterior(likelihood)}

  def branches = root.left.branches ::: root.right.branches

  def inside(tree:FdurTree = root):DenseVector[Double] = {
    tree match{
      case Node(left,right,cont) =>
        val fromLeft = inside(left)
        val fromRight = inside(right)
        cont.isNull = left.isNull && right.isNull
        if(cont.isNull){
          for(i <- 0 to 3) cont.alpha(i) = 1.0
          DenseVector(1.0,1.0,1.0,1.0)
        }else{
          for(i <- 0 to 3){cont.alpha(i) = fromLeft(i) * fromRight(i)}
          cont.accumInsideBelief(model)
        }
      case Leaf(_,cont) =>
        if(cont.nuc != Base.N){
          cont.isNull = true
          cont.alpha(0 to 3) := 1.0
          DenseVector(1.0,1.0,1.0,1.0)
        }else{
          cont.alpha(0 to 3) := 0.0
          cont.alpha(cont.nuc.toInt) = 1.0
          cont.accumInsideBelief(model)
        }
    }
  }

  def outside(tree:FdurTree = root,fromBro:DenseVector[Double] = model.pi,fromPar:DenseVector[Double] = DenseVector(1,1,1,1)){
    tree match{
      case Node(left,right,cont) =>
        for(i <- 0 to 3) cont.beta(i) = fromBro(i) * fromPar(i)
        if(!cont.isNull) innerOutside(left,right,cont)
      case Leaf(_,cont) =>
        for(i <- 0 to 3) cont.beta(i) = fromBro(i) * fromPar(i)
    }
  }

  protected def mkTreeList = {
    val bl = ListBuffer[FdurTree]()
    def f(bl:ListBuffer[FdurTree],t:FdurTree){
      t match {
        case Node(l,r,_) =>
          f(bl,l)
          f(bl,r)
        case Leaf(_,_) =>
          Unit
      }
      bl += t
    }
    f(bl,root)
    bl.init.toList
  }

  protected def innerOutside(left:FdurTree,right:FdurTree,cont:Content){
    val fromLeft = left.cont.accumInsideBelief(model)
    val fromRight = right.cont.accumInsideBelief(model)
    val fromThis = cont.accumOutsideBelief(model)
    outside(left,fromRight,fromThis)
    outside(right,fromLeft,fromThis)
  }

  /*def deriveLL:(fdur.Parameters,List[Double]) = {
    val (lParam,lT) = deriveLL(root.left)
    val (rParam,rT) = deriveLL(root.right)
    val param = lParam + rParam
    val t = lT ::: rT
    val tmp = DenseVector((0 to 3).map(i => root.cont.posterior(i,i) / model.pi(i)).toArray)
    Pair(fdur.Parameters(param.Bvec,param.pi + tmp),t)
  }

  private def deriveLL(tree:Tree):(fdur.Parameters,List[Double]) = {
    val rs = for(i <- 0 to 3;j <- 0 to 3) yield deriveLWithLogR(i,j,tree.cont)
    val post = for(i <- 0 to 3;j <- 0 to 3) yield tree.cont.posterior(i,j)
    val ps = (rs,post).zipped.map((r,p) => deriveLWithPi(tree.cont,r) * p).reduceLeft(_ + _)
    val bs = (rs,post).zipped.map((r,p) => deriveLWithB(tree.cont,r) * p).reduceLeft(_ + _)
    val ts = (rs,post).zipped.map((r,p) => deriveLWithT(tree.cont,r) * p).reduceLeft(_ + _)

    tree match{
      case Node(left,right,cont) =>
        val (lParam,lT) = deriveLL(left)
        val (rParam,rT) = deriveLL(right)
        val param = lParam + rParam + fdur.Parameters(bs,ps)
        val tlist:List[Double] = lT ::: rT ::: List(ts)
        Pair(param,tlist)
      case Leaf(_,_) =>
        Pair(fdur.Parameters(bs,ps),List(ts))
    }
  }

  private def deriveLWithLogR(a:Int,b:Int,cont:fdur.Content):DenseMatrix[Double] =
    cont.NsMati(a,b,model) - (diag(cont.FdVeci(a,b,model)) * model.R * cont.t)

  private def deriveLWithPi(cont:fdur.Content,r:DenseMatrix[Double]):DenseVector[Double] =
    DenseVector((0 to 3).map(i => sum(for(j <- 0 to 3;if j != i) yield r(j,i)) / model.pi(i)).toArray)

  private def deriveLWithB(cont:fdur.Content,r:DenseMatrix[Double]):DenseVector[Double] = {
    val tmp = (r + r.t) :/ model.B
    DenseVector((for(i <- 0 to 2;j <- i+1 to 3) yield tmp(i,j)).toArray)
  }

  private def deriveLWithT(cont:fdur.Content,r:DenseMatrix[Double]):Double = (sum(r) - trace(r)) / cont.t
*/
}