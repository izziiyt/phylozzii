package fdur

import breeze.linalg.DenseVector
import scala.collection.mutable.ListBuffer
import scala.math._
import alignment.Base

class PhylogencyTree(val root:FdurNode,val model:EvolutionModel){

  root.setTransition(model)

  def this(nhFile:String,m:EvolutionModel) = this(FdurTree.fromFile(nhFile),m)

  def this(that:PhylogencyTree,m:EvolutionModel) = this(that.root,m)

  def setBranch(x:List[Double]){
    val tmp1 = root.left.setBranch(x)
    val tmp2 = root.right.setBranch(tmp1)
    if(tmp2.nonEmpty) sys.error("bad branches")
    root.setTransition(model)
  }

  def count = {
    val cList = mkTreeList.map(_.cont.nsAndFd(model))
    Count(cList.map(_._1),cList.map(_._2),root.collectn(model),log(likelihood))
  }

  def likelihood:Double = root.likelihood(model)

  def setColumn(al:Array[Base]) = {val tmp = root.formatWithColumn(al);if(tmp.nonEmpty) sys.error("bad column")}

  def setPosterior(){root.setPosterior(likelihood)}

  def contents = mkTreeList.map(_.cont)

  def branches = root.left.branches ::: root.right.branches

  def inside(tree:FdurTree = root):DenseVector[Double] = {
    tree match{
      case FdurNode(left,right,cont) =>
        val fromLeft = inside(left)
        val fromRight = inside(right)
        /*if(tree.isNull){
          cont.alpha
        }*/
        //else{
        for(i <- 0 to 3){cont.alpha(i) = fromLeft(i) * fromRight(i)}
        cont.accumInsideBelief(model)
      //}
      case FdurLeaf(_,cont) =>
        if(cont.nuc.isN){
          cont.alpha(0 to 3) := 1.0
          cont.accumInsideBelief(model)
        }
        else{
          cont.alpha(0 to 3) := 0.0
          cont.alpha(cont.nuc.toInt) = 1.0
          cont.accumInsideBelief(model)
        }
    }
  }

  def outside(tree:FdurTree = root,fromBro:DenseVector[Double] = model.pi,fromPar:DenseVector[Double] = DenseVector.ones[Double](4)){
    for(i <- 0 to 3) tree.cont.beta(i) = fromBro(i) * fromPar(i)
    tree match{
      case FdurNode(left,right,cont) => innerOutside(left,right,cont)
      case _ => Unit
    }
  }

  protected def mkTreeList = {
    val bl = ListBuffer[FdurTree]()
    def f(bl:ListBuffer[FdurTree],t:FdurTree){
      t match {
        case FdurNode(l,r,_) =>
          f(bl,l)
          f(bl,r)
        case FdurLeaf(_,_) =>
      }
      bl += t
    }
    f(bl,root)
    bl.init.toList
  }

  protected def innerOutside(left:FdurTree,right:FdurTree,cont:Content) {
    val fromLeft = left.cont.accumInsideBelief(model)
    val fromRight = right.cont.accumInsideBelief(model)
    val fromThis = cont.accumOutsideBelief(model)
    outside(left, fromRight, fromThis)
    outside(right, fromLeft, fromThis)
  }

}