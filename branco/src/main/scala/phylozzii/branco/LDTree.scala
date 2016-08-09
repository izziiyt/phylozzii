package phylozzii.branco

import phylozzii.fdur._
import breeze.linalg._
import alignment.Base
import breeze.numerics.exp
import scala.annotation.tailrec
import breeze.math.LogDouble.SemiringLogDouble

/**
  * Belief propagation algorithms are caluculated on LogDouble scale.
  * VL is DenseVector[LogDouble].
  * ML is DenseMatrix[LogDouble].
  */
sealed trait LDTree extends PrimitiveTree{
  /** inside variable*/
  def alpha: Array[VL]
  /** diagonal-inside variable*/
  def alphaD: Array[VL]
  /** outside variable*/
  def beta: Array[VL]
  /** diagonal-outside variable*/
  def betaD: Array[VL]
  /** trans(i,j) is mutation probability j to i in time [[phylozzii.fdur.PrimitiveTree.t]]*/
  def trans: ML
  /** diagonal matrix of [[trans]]*/
  def transD: ML
  /** evolutionary model*/
  def model: Model
  /** used in inside-outside algorithm*/
  lazy val insideProp: Array[VL] = {
    require(alpha.nonEmpty)
    alpha.map(a => trans.t * a)
  }
  /** used in insideD-outsideD algorithm*/
  lazy val insidePropD: Array[VL] = {
    require(alpha.nonEmpty)
    alphaD.map(a => transD.t * a)
  }
  /** tree-form to [[List]] form*/
  def toList: List[LDTree]
}

object LDTree extends LDTreeUtilTrait {
  @tailrec
  protected def innerIn(ch: List[ModelChild], cols: List[Array[Base]], result: List[LDChild], model:Model,
                        target:String, targetColumn:Array[Base]): (List[LDChild], List[Array[Base]]) = {
    if (ch == Nil)
      (result.reverse, cols)
    else {
      val (newTree,newCols) = inside(ch.head,model,cols,target,targetColumn)
      innerIn(ch.tail, newCols, newTree :: result, model,target,targetColumn)
    }
  }

  protected def inside(tr: ModelChild, model: Model, columns: List[Array[Base]], target:String, targetColumn: Array[Base]):
  (LDChild, List[Array[Base]]) =
    tr match {
      case ModelLeaf(name,t) =>
        (LDLeaf.inside(name, t, columns.head, model, name == target, targetColumn),columns.tail)
      case ModelNode(ch,t) =>
        val tmp = innerIn(ch,columns,Nil,model,target,targetColumn)
        (LDNode.inside(tmp._1,t,model),tmp._2)
    }

  def inside(tr: ModelRoot, model: Model, columns: List[Array[Base]], target:String): LDRoot = {
    val targetColumn: Array[Base] = if(target.startsWith("hg1")) columns.head else columns.last
    val (newCh,newCols) = innerIn(tr.children,columns,Nil,model,target,targetColumn)
    if(newCols.nonEmpty) sys.error("Bad Columns")
    LDRoot.inside(newCh,model)
  }

  @tailrec
  protected def innerOut(prefix:List[LDChild],suffix:List[LDChild],result:List[LDChild], fromThis:Array[VL],fromThisD:Array[VL]):
  List[LDChild] = {
    if(prefix.isEmpty) result.reverse
    else{
      innerOut(prefix.tail, prefix.head :: suffix,
        outside(prefix.head,(suffix ::: prefix.tail).map(_.insideProp),fromThis,
          (suffix ::: prefix.tail).map(_.insidePropD),fromThisD) :: result,fromThis,fromThisD)
    }
  }

  protected def mkBeta(fromSib:List[Array[VL]],fromPar:Array[VL]):Array[VL] = mkAlpha(fromPar :: fromSib)

  protected def mkBetaD(fromSib:List[Array[VL]],fromPar:Array[VL],fromSibD:List[Array[VL]],fromParD:Array[VL]):Array[VL] =
    mkAlphaD(fromPar :: fromSib, fromParD :: fromSibD)

  protected def outside(tree:LDChild, fromSib:List[Array[VL]], fromPar:Array[VL],
                        fromSibD:List[Array[VL]],fromParD:Array[VL]): LDChild = {
    val beta = mkBeta(fromSib,fromPar)
    val betaD = mkBetaD(fromSib,fromPar,fromSibD,fromParD)
    lazy val fromThis = outsideProp(beta,tree.trans)
    lazy val fromThisD = outsideProp(betaD,tree.transD)
    tree match {
      case LDLeaf(name, t, trans, transD, alpha, alphaD, _, _, model) =>
        LDLeaf(name,t,trans,transD,alpha,alphaD,beta,betaD,model)
      case LDNode(ch,t,trans,transD,alpha,alphaD,_,_,model) =>
        val newch = innerOut(ch,Nil,Nil,fromThis,fromThisD)
        LDNode(newch,t,trans,transD,alpha,alphaD,beta,betaD,model)
    }
  }

  def outside(tr:LDRoot):LDRoot = {
    require(tr.alpha.nonEmpty)
    val n = tr.alpha.length
    val beta = Array.fill(n)(tr.model.pi.toLogDouble)
    val betaD = Array.fill(n)(DenseVector.zeros[Double](4).toLogDouble)
    val fromThis = outsideProp(beta,tr.trans)
    val fromThisD = outsideProp(betaD,tr.transD)
    val newch = innerOut(tr.children,Nil,Nil,fromThis,fromThisD)
    LDRoot(newch,tr.trans,tr.transD,tr.alpha,tr.alphaD,beta,betaD,tr.model)
  }

  protected def outsideProp(beta:Array[VL],trans:ML): Array[VL] = {
    require(beta.nonEmpty)
    beta.map(b => trans * b)
  }

  def inout(tr: ModelRoot, model: Model, columns: List[Array[Base]],target:String): LDRoot = {
    require(columns.forall(_.length == columns.head.length))
    require(columns.head.nonEmpty)
    require(tr.leafLength == columns.length)
    val afterIn = inside(tr, model, columns, target)
    outside(afterIn)
  }

  /** Expected Barnch Length Score, expected conservation time on phylogenetic tree.
    *
    * @param tr input ModelTree
    * @param model input Model
    * @param columns input
    * @param target target species name
    * @return
    */
  def bls(tr:ModelRoot, model:Model, columns:List[Array[Base]], target:String): (Array[Double], Array[Double]) = {
    val root = inout(tr, model, columns, target)
    val regbls = tr.branches.sum
    val regblsa = tr.anclen(target)
    (root.bls.map(_ / regbls), root.blsa.map(_ / regblsa))
  }

}

trait LDChild extends LDTree with PrimitiveChild{
  def model: Model

  def toList: List[LDTree]

  def leafList: List[LDLeaf]

  lazy val dltA: DenseMatrix[Double] = {
    val tmp = for(from <- 0 to 3; to <- 0 to 3) yield
      (0 to 3).foldLeft(0.0){(n,x) => n + model.u(to,x) * model.ui(x,from) * k(t * model.R(to,to), t * model.lambda(x))}
    new DenseMatrix[Double](4,4,tmp.toArray) * t
  }

  lazy val dltB: DenseMatrix[Double] = {
    val tmp = for(from <- 0 to 3; to <- 0 to 3) yield
      (0 to 3).foldLeft(0.0){(n,x) => n + model.u(to,x) * model.ui(x,from) * k(t * model.lambda(x), t * model.R(from,from))}
    new DenseMatrix[Double](4,4,tmp.toArray) * t
  }

  protected def k(x: Double, y: Double): Double = {
    val tmp = (exp(x) - exp(y)) / (x - y); if(tmp.isNaN) exp(x) else tmp
  }

  protected def innerBls(lgl: Array[Double], as: Array[VL], bs: Array[VL], dlt: MD): Array[Double] =
    (as,bs,lgl).zipped.map {
      (a, b, l) =>
        val tmp = a.value * b.value.t
        sum(tmp :* dlt) / l
    }

  def bls(lgl:Array[Double]): Array[Double]

  /**bls on ancestory*/
  def blsa(lgl:Array[Double]): Array[Double]
}

trait LDParent extends LDTree with PrimitiveParent {
  def leafList: List[LDLeaf] = children.foldLeft(Nil:List[LDLeaf])((n,x) => x.leafList ::: n)
  override def children: List[LDChild]
  def toList: List[LDTree] = this :: children.foldLeft(Nil:List[LDTree])((n,x) => x.toList ::: n)
  def bls(lgl:Array[Double]): Array[Double] = {
    require(beta.nonEmpty && betaD.nonEmpty)
    val n = beta.length
    children.foldLeft(Array.fill[Double](n)(0.0))((m,x) => (m,x.bls(lgl)).zipped.map(_ + _))
  }
  def blsa(lgl:Array[Double]): Array[Double] = {
    require(beta.nonEmpty && betaD.nonEmpty)
    val n = beta.length
    children.foldLeft(Array.fill[Double](n)(0.0))((m,x) => (m,x.blsa(lgl)).zipped.map(_ + _))
  }
}

trait LDTreeUtilTrait {
  /**
    *
    * @param fromChildren
    * @return
    */
  def mkAlpha(fromChildren: List[Array[VL]]): Array[VL] =
    fromChildren.reduceLeft{(ns,xs) => (ns,xs).zipped.map(_ :* _)}

  def mkAlphaD(fromChildren: List[Array[VL]], fromChildrenD: List[Array[VL]]): Array[VL] = {
    mkConfound(fromChildren, fromChildrenD).map(mkAlpha).reduce((n,x) => (n,x).zipped.map(_ + _))
  }

  def mkTrans(ti:Double,mi:Model):ML = {
    val tmp: DenseMatrix[Double] = mi.u * diag(exp(mi.lambda * ti)).*(mi.ui)
    tmp.toLogDouble
  }

  def mkConfound[T](base:List[T],confounder:List[T]): List[List[T]] = {
    @tailrec
    def f(i:List[T], t:List[T], b:List[T], result:List[List[T]]):List[List[T]] = {
      if(b.isEmpty)
        result.reverse
      else
        f((t.head :: i.reverse).reverse, t.tail, b.tail, (t.tail.reverse ::: (b.head :: i.reverse)).reverse :: result)
    }
    f(Nil, base, confounder, Nil)
  }
}
