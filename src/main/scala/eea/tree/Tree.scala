package eea.tree

import fdur._
import breeze.linalg._
import alignment.Base
import breeze.math.LogDouble
import breeze.numerics.exp
import scala.annotation.tailrec
import breeze.math.LogDouble.SemiringLogDouble

trait LDTree extends PrimitiveTree{
  def alpha: Array[VL]
  def alphaD: Array[VL]
  def beta: Array[VL]
  def betaD: Array[VL]
  def trans: ML
  def transD: ML
  def model: Model
  lazy val insideProp: Array[VL] = {
    require(alpha.nonEmpty)
    alpha.map(a => trans.t * a)
  }
  lazy val insidePropD: Array[VL] = {
    require(alpha.nonEmpty)
    alphaD.map(a => transD.t * a)
  }

  def toList: List[LDTree]
}

object LDTree extends LDTreeUtilTrait {
  @tailrec
  protected def innerIn(ch: List[ModelChild], cols: List[Array[Base]], result: List[LDChild], model:Model,target:String):
  (List[LDChild], List[Array[Base]]) = {
    if (ch == Nil)
      (result.reverse, cols)
    else {
      val (newTree,newCols) = inside(ch.head,model,cols,target)
      innerIn(ch.tail, newCols, newTree :: result, model,target)
    }
  }

  protected def inside(tr: ModelChild, model: Model, columns: List[Array[Base]],target:String): (LDChild, List[Array[Base]]) =
    tr match {
      case ModelLeaf(name,t) =>
        (LDLeaf.inside(name,t,columns.head,model,name == target),columns.tail)
      case ModelNode(ch,t) =>
        val tmp = innerIn(ch,columns,Nil,model,target)
        (LDNode.inside(tmp._1,t,model),tmp._2)
    }

  def inside(tr: ModelRoot, model: Model, columns: List[Array[Base]], target:String): LDRoot = {
    val (newCh,newCols) = innerIn(tr.children,columns,Nil,model,target)
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
    require(tr.leafLength == columns.length)
    val afterIn = inside(tr, model, columns, target)
    outside(afterIn)
  }

  // probablistic Barnch Length Score, sum of expected preserved time on all branches.
  def bls(tr:ModelRoot,model:Model,columns:List[Array[Base]],target:String): Array[Double] = {
    val root = inout(tr,model,columns,target)
    root.bls
  }
  /* probablistic Branch Length Score, sum of expected preserved time on branches which are
  ancestors of target species.Branch Length Score in Ancestory.
   */
  def blsa(tr:ModelRoot,model:Model,columns:List[Array[Base]],target:String): Array[Double] = {
    val root = inout(tr,model,columns,target)
    root.blsa
  }
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

  /*protected def thisBls(lgl: Array[Double]): Array[Double] = {
    def f(as:Array[VL],bs:Array[VL],dlt:MD):Array[Double] = (as,bs,lgl).zipped.map {
      (a, b, l) =>
        val tmp = a.value * b.value.t
        //val tmp = diag(a.value) * DenseMatrix.ones[Double](4,4) * diag(b.value)
        sum(tmp :* dlt) / l
    }
    val ax = f(alphaD, beta, dltA)
    val bx = f(alpha, betaD, dltB)
    (ax, bx).zipped.map(_ + _)
  }*/

  protected def innerBls(lgl:Array[Double],as:Array[VL],bs:Array[VL],dlt:MD):Array[Double] =
    (as,bs,lgl).zipped.map {
      (a, b, l) =>
        val tmp = a.value * b.value.t
        sum(tmp :* dlt) / l
    }

  /*protected def thisBlsa(lgl: Array[Double]): Array[Double] = {
    def f(as:Array[VL],bs:Array[VL],dlt:MD):Array[Double] = (as,bs,lgl).zipped.map {
      (a, b, l) =>
        val tmp = a.value * b.value.t
        //val tmp = diag(a.value) * DenseMatrix.ones[Double](4,4) * diag(b.value)
        sum(tmp :* dlt) / l
    }
    f(alphaD, beta, dltA)
    //val bx = f(alpha, betaD, dltB)
    //(ax, bx).zipped.map(_ + _)
  }*/

  def bls(lgl:Array[Double]):Array[Double]
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
  //override def blsa(lgl:Array[Double]) = (super.bls(lgl), thisBlsa(lgl)).zipped.map(_ + _)
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
    val lgl = alpha.map(a => a.value.t * model.pi)
    super.bls(lgl)
  }
  def blsa: Array[Double] = {
    val lgl = alpha.map(a => a.value.t * model.pi)
    super.blsa(lgl)
  }
}

object LDLeaf extends LDTreeUtilTrait{
  def inside(name:String,t:Double,column:Array[Base],m:Model,isTarget:Boolean):LDLeaf = {
    val alpha = mkAlpha(column)
    val alphaD = if(isTarget) alpha else Array.fill(alpha.length)(DenseVector.zeros[Double](4).toLogDouble)
    val trans = mkTrans(t,m)
    val transD = DenseMatrix.zeros[Double](4,4).toLogDouble
    for(i <- 0 to 3) {transD(i,i) = trans(i,i)}
    new LDLeaf(name,t,trans,transD,alpha,alphaD,null,null,m)
  }

  protected def mkAlpha(column:Array[Base]): Array[VL] = column.map{
    case Base.N =>
      DenseVector.ones[LogDouble](4)
    case x =>
      val tmp = DenseVector.zeros[Double](4)
      tmp(x.toInt) = 1.0
      tmp.toLogDouble
  }
}

object LDNode extends LDTreeUtilTrait{
  def inside(ch:List[LDChild],t:Double,m:Model) = {
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
  def mkAlpha(fromChildren:List[Array[VL]]):Array[VL] =
    fromChildren.reduceLeft{(ns,xs) => (ns,xs).zipped.map(_ :* _)}
  def mkAlphaD(fromChildren:List[Array[VL]],fromChildrenD:List[Array[VL]]):Array[VL] = {
    val tmp:List[List[Array[VL]]] = mkConfound(fromChildren,fromChildrenD)
    tmp.map(mkAlpha).reduce((n,x) => (n,x).zipped.map(_ + _))
  }
  def mkTrans(ti:Double,mi:Model):ML = {
    val tmp: DenseMatrix[Double] = mi.u * diag(exp(mi.lambda * ti)).*(mi.ui)
    tmp.toLogDouble
  }
  def mkConfound[T](base:List[T],confounder:List[T]) = {
    @tailrec
    def f(i:List[T], t:List[T], b:List[T], result:List[List[T]]):List[List[T]] = {
      if(b.isEmpty)
        result.reverse
      else
        f((t.head :: i.reverse).reverse, t.tail, b.tail, (t.tail.reverse ::: (b.head :: i.reverse)).reverse :: result)
    }
    f(Nil,base,confounder,Nil)
  }
}

/*
import java.io.FileReader
import alignment.Base
import breeze.linalg.{sum, DenseMatrix, DenseVector, diag}
import fdur.NHParser

import scala.math._

trait Tree extends fdur.Tree {

  def setModel(e:EvolutionModel):Unit

  def setTarget(n:String):Unit

  def setColumn(l:List[Base]):List[Base]

  def names:List[String]

  def column:List[Base]

  protected[tree] def inside:DenseVector[Double]

  protected[tree] def outside:DenseVector[Double]

  protected[tree] def calcOutside(fromPar:DenseVector[Double],fromSib:DenseVector[Double]):Unit

  protected[tree] def insideD:DenseVector[Double]

  protected[tree] def outsideD:DenseVector[Double]

  protected[tree] def m:EvolutionModel

  protected[tree] var transProb:DenseMatrix[Double]
  // cell of i-th row and j-th column is transition probability(base i -> base j)

  protected[tree] var transProbD:DenseMatrix[Double]

  def bls:Double
}

trait HavingChildren extends Tree {

  def left:HavingParent

  def right:HavingParent

  def names:List[String] = left.names ++ right.names

  def column:List[Base] = left.column ++ right.column

  lazy val inside:DenseVector[Double] = {
    val l = left.transProb * left.inside
    val r = right.transProb * right.inside
    l :* r
  }

  lazy val insideD:DenseVector[Double] = {
    val l = left.transProb * left.inside
    val ld = left.transProbD * left.insideD
    val r = right.transProb * right.inside
    val rd = right.transProbD * right.insideD
    l :* rd + ld :* r
  }

  protected[tree] def calcOutside(fromPar:DenseVector[Double],fromSib:DenseVector[Double]):Unit = {
    left.calcOutside(transProb.t * outside,right.transProb * right.inside)
    right.calcOutside(transProb.t * outside,left.transProb * left.inside)
  }

  def setModel(e:EvolutionModel):Unit = {
    left.setModel(e)
    right.setModel(e)
  }

  def setTarget(n:String):Unit = {
    left.setTarget(n)
    right.setTarget(n)
  }

  def setColumn(column:List[Base]):List[Base] = {
    val tmp = left.setColumn(column)
    right.setColumn(tmp)
  }

}

abstract class HavingParent(val t:Double) extends Tree {

  protected[tree] def calcOutsideD(fromPar:DenseVector[Double],
                                   fromSib:DenseVector[Double],
                                   fromParD:DenseVector[Double],
                                   fromSibD:DenseVector[Double]):Unit

  protected[tree] var outside:DenseVector[Double] = null

  protected[tree] var outsideD:DenseVector[Double] = null

  def expConsIn: DenseMatrix[Double] = {
    def k(x:Double,y:Double) = {
      val tmp = (exp(x) - exp(y)) / (x - y)
      if(tmp.isNaN) exp(x) else tmp
    }
    val tmp = for(b <- 0 to 3;a <- 0 to 3) yield {
      (0 to 3)./:(0.0)((x,i) => x + m.u(a,i) * m.ui(i,b) * k(t * m.R(a,a),t * m.lambda(i)))
    }
    new DenseMatrix(4,4,tmp.toArray)
  }

  def expConsOut: DenseMatrix[Double] = {
    def k(x:Double,y:Double) = {
      val tmp = (exp(x) - exp(y)) / (x - y)
      if(tmp.isNaN) exp(x) else tmp
    }
    val tmp = for(b <- 0 to 3;a <- 0 to 3) yield {
      (0 to 3)./:(0.0)((x,i) => x + m.u(a,i) * m.ui(i,b) * k(t * m.R(b,b),t * m.lambda(i)))
    }
    new DenseMatrix(4,4,tmp.toArray)
  }

}

case class Leaf(name:String,b:Double) extends HavingParent(b) {

  protected var base:Base = Base.N

  def names:List[String] = List(name)

  def column:List[Base] = List(base)

  protected[tree] var m:EvolutionModel = null

  protected[tree] var insideD:DenseVector[Double] = null

  protected[tree] var transProb:DenseMatrix[Double] = null

  protected[tree] var transProbD:DenseMatrix[Double] = null

  def setTarget(n:String):Unit = {
    insideD =
      if(name == n) DenseVector((0 to 3).map{x => if(x == base.toInt) 1.0 else 0.0}.toArray)
      else DenseVector.zeros[Double](4)
  }

  def setModel(e:EvolutionModel):Unit = {
    m = e
    transProb = m.u * diag(m.lambda.map { x => math.exp(x * t) }).*(m.ui)
    transProbD = transProb
    for (i <- 0 to 3; j <- 0 to 3; if i != j) {
      transProbD(i, j) = 0.0
    }
  }

  def setColumn(column:List[Base]):List[Base] = {
    base = column.head
    if(column.nonEmpty) column.tail else List(Base.N)
  }

  lazy val inside:DenseVector[Double] =
    DenseVector((0 to 3).map{x => if(x == base.toInt) 1.0 else 0.0}.toArray)

  protected[tree] def insideD(n:String):DenseVector[Double] =
    if(n == name) inside else DenseVector.zeros[Double](4)

  protected[tree] def calcOutside(fromPar:DenseVector[Double],fromSib:DenseVector[Double]):Unit = {
    outside = fromPar :* fromSib
  }

  protected[tree] def calcOutsideD(fromPar:DenseVector[Double],
                                            fromSibD:DenseVector[Double],
                                            fromParD:DenseVector[Double],
                                            fromSib:DenseVector[Double]): Unit
  = {
    outsideD = (fromPar :* fromSibD) + (fromParD :* fromSib)
  }

  def bls: Double = {
    val tmp1 = insideD.t * expConsIn.*(outside)
    val tmp2 = inside.t * expConsOut.*(outsideD)
    tmp1 + tmp2
  }
}

case class Root(left:HavingParent,right:HavingParent) extends HavingChildren {

  protected[tree] var m:EvolutionModel = null

  protected var hasModel = false

  protected var hasTarget = false

  protected var hasColumn = false

  protected[tree] var transProb = DenseMatrix.zeros[Double](4,4)

  protected[tree] var transProbD = DenseMatrix.zeros[Double](4,4)

  override def setModel(e:EvolutionModel):Unit = {
    hasModel = true
    m = e
    super.setModel(e)
  }

  override def setTarget(n:String):Unit = {
    hasTarget = true
    super.setTarget(n)
  }

  override def setColumn(column:List[Base]):List[Base] = {
    hasColumn = true
    super.setColumn(column)
  }

  lazy val outside:DenseVector[Double] = {
    left.calcOutside(transProb.t * m.pi,right.transProb * right.inside)
    right.calcOutside(transProb.t * m.pi,left.transProb * left.inside)
    m.pi
  }

  lazy val outsideD:DenseVector[Double] = {
    val tmp = DenseVector.zeros[Double](4)
    val fromThis = transProb.t * outside
    val fromThisD = transProbD.t * tmp
    left.calcOutsideD(fromThis,right.transProbD * right.insideD,fromThisD,right.transProb * right.inside)
    right.calcOutsideD(fromThis,left.transProbD * left.insideD,fromThisD,left.transProb * left.inside)
    tmp
  }

  def bls:Double = {
    left.bls + right.bls
  }
}

case class Node(left:HavingParent,right:HavingParent,override val t:Double) extends HavingParent(t) with HavingChildren {

  protected[tree] var m:EvolutionModel = null

  protected[tree] var transProb:DenseMatrix[Double] = null

  protected[tree] var transProbD:DenseMatrix[Double] = null

  override def setModel(e:EvolutionModel): Unit ={
    m = e
    transProb = m.u * diag(m.lambda.map { x => math.exp(x * t) }).*(m.ui)
    transProbD = transProb
    for (i <- 0 to 3; j <- 0 to 3; if i != j) {
      transProbD(i, j) = 0.0
    }
    super.setModel(e)
  }

  override protected[tree] def calcOutside(fromPar:DenseVector[Double],fromSib:DenseVector[Double]):Unit = {
    outside = fromPar :* fromSib
    super[HavingChildren].calcOutside(fromPar,fromSib)
  }

  override protected[tree] def calcOutsideD(fromPar:DenseVector[Double],
                                            fromSibD:DenseVector[Double],
                                            fromParD:DenseVector[Double],
                                            fromSib:DenseVector[Double]):Unit = {
    outsideD = (fromPar :* fromSibD) + (fromParD :* fromSib)
    val fromThis = transProb.t * outside
    val fromThisD = transProbD.t * outsideD
    left.calcOutsideD(fromThis,right.transProbD * right.insideD,fromThisD,right.transProb * right.inside)
    right.calcOutsideD(fromThis,left.transProbD * left.insideD,fromThisD,left.transProb * left.inside)
  }

  def bls: Double = {
    val tmp1 = insideD.t * expConsIn.*(outside)
    val tmp2 = inside.t * expConsOut.*(outsideD)
    left.bls + right.bls + tmp1 + tmp2
  }
}

object Tree extends NHParser4{

  def fromFile(nhFile:String):Root = {
    val reader = new FileReader(nhFile)
    val tmp = parseAll(tree,reader).get
    reader.close()
    tmp
  }

  def fromString(nhString:String):Root = {
    parseAll(tree,nhString).get
  }

}

class NHParser4 extends NHParser[Tree] {

  override def nodePair:Parser[(HavingParent,HavingParent)] = "("~>node~","~node<~")"  ^^
    {case left~","~right => (left,right)}

  def tree:Parser[Root] =  nodePair<~";"  ^^
    {case (left,right) => Root(left,right)}

  def node:Parser[HavingParent] = nodePair~":"~value ^^
    {case (left,right)~":"~value => Node(left,right,value.toDouble)} | leaf

  def leaf:Parser[Leaf] = name~":"~value ^^
    {case name~":"~value => Leaf(name,value.toDouble)}

}*/