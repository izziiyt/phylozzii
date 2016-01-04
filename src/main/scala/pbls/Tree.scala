package pbls

import fdur._
import breeze.linalg._
import alignment.Base
import breeze.math.LogDouble
import breeze.numerics.exp
import scala.annotation.tailrec
import breeze.math.LogDouble.SemiringLogDouble

/**
  * Belief propagation algorithms are caluculated on LogDouble scale.
  * VL is DenseVector[LogDouble] defined at [[eea.tree]].
  * ML is DenseMatrix[LogDouble] defined at [[eea.tree]].
  */
trait LDTree extends PrimitiveTree{
  /** inside variable*/
  def alpha: Array[VL]
  /** inside diagonal variable*/
  def alphaD: Array[VL]
  /** outside variable*/
  def beta: Array[VL]
  /** outside diagonale variable*/
  def betaD: Array[VL]
  /** (i,j) is mutation probability j to i in time [[fdur.PrimitiveTree.t]]*/
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
                        target:String, targetColumn:Array[Base]):
  (List[LDChild], List[Array[Base]]) = {
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

  def mkBeta(fromSib:List[Array[VL]],fromPar:Array[VL]):Array[VL] = mkAlpha(fromPar :: fromSib)

  def mkBetaD(fromSib:List[Array[VL]],fromPar:Array[VL],fromSibD:List[Array[VL]],fromParD:Array[VL]):Array[VL] =
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

  /** probablistic Barnch Length Score, sum of expected preserved time on all branches.
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
    (root.bls.map(_ / regbls),root.blsa.map(_ / regblsa))
  }
  /**
    *  probablistic Branch Length Score, sum of expected preserved time on branches which are
    *  ancestors of target species.Branch Length Score in Ancestory.
   */
  /*def blsa(tr:ModelRoot,model:Model,columns:List[Array[Base]],target:String): Array[Double] = {
    val root = inout(tr,model,columns,target)
    val reg = tr.anclen(target)
    root.blsa.map(_ / reg)
  }*/
}

trait LDChild extends LDTree with PrimitiveChild{
  def model:Model

  def toList:List[LDTree]

  def leafList:List[LDLeaf]

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

  protected def k(x:Double,y:Double):Double = {
    val tmp = (exp(x) - exp(y)) / (x - y); if(tmp.isNaN) exp(x) else tmp
  }

  protected def innerBls(lgl:Array[Double],as:Array[VL],bs:Array[VL],dlt:MD):Array[Double] =
    (as,bs,lgl).zipped.map {
      (a, b, l) =>
        val tmp = a.value * b.value.t
        sum(tmp :* dlt) / l
    }

  def bls(lgl:Array[Double]):Array[Double]

  /**bls on ancestory*/
  def blsa(lgl:Array[Double]):Array[Double]
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

case class LDLeaf(name:String, t:Double, trans:ML, transD:ML, alpha:Array[VL], alphaD:Array[VL], beta:Array[VL],
                  betaD:Array[VL], model:Model) extends LDChild with PrimitiveLeaf{
  def toList:List[LDTree] = this :: Nil
  def leafList = this :: Nil
  def bls(lgl:Array[Double]) =
    (innerBls(lgl,alphaD, beta, dltA),innerBls(lgl,alpha, betaD, dltB)).zipped.map(_ + _)
  def blsa(lgl:Array[Double]) =
    innerBls(lgl, alphaD, beta, dltA)
}

case class LDNode(children:List[LDChild], t:Double, trans:ML, transD:ML,alpha:Array[VL], alphaD:Array[VL],
                  beta:Array[VL], betaD:Array[VL], model:Model) extends LDChild with LDParent with PrimitiveNode {
  override def bls(lgl:Array[Double]) = {
    val ax = innerBls(lgl,alphaD, beta, dltA)
    val bx = innerBls(lgl,alpha, betaD, dltB)
    (super.bls(lgl), ax, bx).zipped.map(_ + _ + _)
  }
  override def blsa(lgl:Array[Double]) = {
    val ax = innerBls(lgl,alphaD, beta, dltA)
    (super.blsa(lgl), ax).zipped.map(_ + _)
  }
}

case class LDRoot(children:List[LDChild], trans:ML, transD:ML, alpha:Array[VL], alphaD:Array[VL],
                  beta:Array[VL], betaD:Array[VL], model:Model) extends PrimitiveRoot with LDParent{
  override def leafList = super.leafList.reverse
  override def toList = super.toList.reverse
  def bls: Array[Double] = {
    val ll = alpha.map(a => a.value.t * model.pi)
    super.bls(ll)
  }
  def blsa: Array[Double] = {
    val lgl = alpha.map(a => a.value.t * model.pi)
    super.blsa(lgl)
  }
}

object LDLeaf extends LDTreeUtilTrait{
  def inside(name:String, t:Double, column:Array[Base],m:Model,isTarget:Boolean,targetColumn: Array[Base]):LDLeaf = {
    val alpha = mkAlpha(column, targetColumn)
    val alphaD =
      if(isTarget) alpha
      else         Array.fill(alpha.length)(DenseVector.zeros[Double](4).toLogDouble)
    val trans = mkTrans(t,m)
    val transD = DenseMatrix.zeros[Double](4,4).toLogDouble
    for(i <- 0 to 3) {transD(i,i) = trans(i,i)}
    new LDLeaf(name,t,trans,transD,alpha,alphaD,null,null,m)
  }

  protected def mkAlpha(column: Array[Base], targetColmun: Array[Base]): Array[VL] = {
    (column, targetColmun).zipped.map {
      case (x, tgt) if x.nonNuc =>
        val tmp = DenseVector.ones[LogDouble](4)
        tmp(tgt.toInt) = LogDoubleZero
        tmp
      case (x, _) =>
        val tmp = DenseVector.zeros[Double](4)
        tmp(x.toInt) = 1.0
        tmp.toLogDouble
      case _ =>
        sys.error("mkAlpha error.")
    }
  }
}

object LDNode extends LDTreeUtilTrait{
  def inside(ch:List[LDChild], t:Double, m:Model) = {
    val props = ch.map(_.insideProp)
    val alpha = mkAlpha(props)
    val alphaD = mkAlphaD(props, ch.map(_.insidePropD))
    val trans = mkTrans(t,m)
    val transD = DenseMatrix.zeros[Double](4,4).toLogDouble
    for(i <- 0 to 3) {transD(i,i) = trans(i,i)}
    new LDNode(ch,t,trans,transD,alpha,alphaD,null,null,m)
  }
}

object LDRoot extends LDTreeUtilTrait{
  def inside(ch:List[LDChild],m:Model) = {
    val props = ch.map(_.insideProp)
    val alpha = mkAlpha(props)
    val alphaD = mkAlphaD(props, ch.map(_.insidePropD))
    val trans = diag(DenseVector.ones[Double](4)).toLogDouble
    val transD = DenseMatrix.zeros[Double](4,4).toLogDouble
    for(i <- 0 to 3) {transD(i,i) = trans(i,i)}
    new LDRoot(ch,trans,transD,alpha,alphaD,null,null,m)
  }
}

trait LDTreeUtilTrait {
  def mkAlpha(fromChildren:List[Array[VL]]): Array[VL] =
    fromChildren.reduceLeft{(ns,xs) => (ns,xs).zipped.map(_ :* _)}
  def mkAlphaD(fromChildren: List[Array[VL]], fromChildrenD: List[Array[VL]]): Array[VL] = {
    //val tmp: List[List[Array[VL]]] = mkConfound(fromChildren,fromChildrenD)
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
