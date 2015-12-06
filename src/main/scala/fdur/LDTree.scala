package fdur

import breeze.linalg._
import alignment.Base
import breeze.math.LogDouble
import breeze.numerics.exp
import scala.annotation.tailrec
import breeze.math.LogDouble.SemiringLogDouble

trait LDTree extends PrimitiveTree{
  type NsFd = (MD,VD)
  def alpha: Array[VL]
  def beta: Array[VL]
  def post: Array[MD]
  def trans: ML
  def model: Model
  lazy val insideProp: Array[VL] = {
    require(alpha.nonEmpty)
    alpha.map(a => trans.t * a)
  }
  def toList: List[LDTree]
  def diffWithPi: Array[VD]
  def diffWithB: Array[MD]
  def diffWithT: List[Array[Double]]
}

object LDTree extends LDTreeUtilTrait {
  type NsFd = Array[(MD,VD)]

  @tailrec
  protected def innerIn(ch: List[ModelChild], cols: List[Array[Base]], result: List[LDChild], model:Model):
  (List[LDChild], List[Array[Base]]) = {
    if (ch == Nil)
      (result.reverse, cols)
    else {
      val (newTree,newCols) = inside(ch.head,model,cols)
      innerIn(ch.tail, newCols, newTree :: result, model)
    }
  }

  protected def inside(tr: ModelChild, model: Model, columns: List[Array[Base]]): (LDChild, List[Array[Base]]) =
    tr match {
      case ModelLeaf(name,t) =>
        (LDLeaf.inside(name,t,columns.head,model),columns.tail)
      case ModelNode(ch,t) =>
        val tmp = innerIn(ch,columns,Nil,model)
        (LDNode.inside(tmp._1,t,model,tmp._1.map(_.insideProp)),tmp._2)
    }

  def inside(tr: ModelRoot, model: Model, columns: List[Array[Base]]): LDRoot = {
    val (newCh,newCols) = innerIn(tr.children,columns,Nil,model)
    if(newCols.nonEmpty) sys.error("Bad Columns")
    LDRoot.inside(newCh,model,newCh.map(_.insideProp))
  }

  @tailrec
  protected def innerOut(prefix:List[LDChild],suffix:List[LDChild],result:List[LDChild], fromThis:Array[VL],lh:Array[LogDouble]):
  List[LDChild] = {
    if(prefix.isEmpty) result.reverse
    else{
      innerOut(prefix.tail, prefix.head :: suffix,
        outside(prefix.head,(suffix ::: prefix.tail).map(_.insideProp),fromThis,lh) :: result,fromThis,lh)
    }
  }

  protected def outside(tree:LDChild, fromSib:List[Array[VL]], fromPar:Array[VL],lh:Array[LogDouble]): LDChild = {
    val beta = mkBeta(fromSib,fromPar)
    lazy val fromThis = outsideProp(beta,tree.trans)
    tree match {
      case LDLeaf(name, t, trans, alpha, _, _, model) =>
        LDLeaf(name,t,trans,alpha,beta,mkPost(alpha,beta,trans,lh),model)
      case LDNode(ch,t,trans,alpha,_,_,model) =>
        val newch = innerOut(ch,Nil,Nil,fromThis,lh)
        LDNode(newch,t,trans,alpha,beta,mkPost(alpha,beta,trans,lh),model)
    }
  }

  def outside(tr:LDRoot):LDRoot = {
    require(tr.alpha.nonEmpty)
    val n = tr.alpha.length
    val beta = Array.fill(n)(tr.model.pi.toLogDouble)
    val fromThis = outsideProp(beta,tr.trans)
    val newch = innerOut(tr.children,Nil,Nil,fromThis,tr.likelihood)
    LDRoot(newch,tr.trans,tr.alpha,beta,mkPost(tr.alpha,beta,tr.trans,tr.likelihood),tr.model)
  }

  protected def outsideProp(beta:Array[VL],trans:ML): Array[VL] = {
    require(beta.nonEmpty)
    beta.map(b => trans * b)
  }

  def inout(tr: ModelRoot, model: Model, columns: List[Array[Base]]): LDRoot = {
    require(columns.forall(_.length == columns.head.length))
    require(tr.leafLength == columns.length)
    val afterIn = inside(tr, model, columns)
    outside(afterIn)
  }

  def suffStat(tr:ModelRoot,model:Model,columns:List[Array[Base]]): (VD, List[MD], List[VD], Double, Long) = {
    val root = inout(tr,model,columns)
    val sufs = root.suffStats
    val lgl = root.loglikelihood.sum
    (sufs._1,sufs._2,sufs._3,lgl, columns.head.length.toLong)
  }

}

trait LDChild extends LDTree with PrimitiveChild{
  def model:Model

  lazy val f: Array[Array[MD]] = {
    def l(start:Int,end:Int,from:Int,to:Int,u:Int,v:Int):Double = {
      def k(x:Double,y:Double):Double = {val tmp = (exp(x) - exp(y)) / (x - y); if(tmp.isNaN) exp(x) else tmp}
      model.u(end, u) * model.ui(u, to) * model.u(from, v) * model.ui(v, start) *
        k(t * model.lambda(u), t * model.lambda(v))
    }
    Array.tabulate(4){ end =>
      Array.tabulate(4){ start =>
        val tmp = for (from <- 0 to 3; to <- 0 to 3) yield {
          (0 to 3).foldLeft(0.0){(n,u) => n +
            (0 to 3).foldLeft(0.0){(m,v) => m + l(start, end, from, to, u, v)}}
        }
        new DenseMatrix[Double](4, 4, tmp.toArray) / trans(end, start).value
      }
    }
  }

  def Fd(to:Int,from:Int):DenseVector[Double] = diag(f(to)(from))

  def Ns(to:Int,from:Int):DenseMatrix[Double] = model.R :* f(to)(from) * t

  def postFd:Array[VD] = post.map { p => {for(a <- 0 to 3;b <- 0 to 3) yield Fd(a,b) * p(a,b)}.reduceLeft(_ + _)}

  def postNs:Array[MD] = post.map { p => {for (a <- 0 to 3; b <- 0 to 3) yield Ns(a, b) * p(a, b)}.reduceLeft(_ + _)}

  lazy val r:Array[MD] = (postNs, postFd).zipped.map { (ns, fd) => ns - (model.R * diag(fd) * t)}

  def ldt:Array[Double] = r map {x => (sum(x) - trace(x)) / t}

  def ldb:Array[MD] = r map { x => (x + x.t) :/ model.B}

  def ldp:Array[VD] = r.map{x => DenseVector(Array.tabulate(4){i => (sum(x(i,::).t) - x(i,i)) / model.pi(i)})}

  def nsAndfd:NsFd = (postNs.reduceLeft(_ + _), postFd.reduceLeft(_ + _))

  def suffStat:List[NsFd]

  def toList:List[LDTree]

  def leafList:List[LDLeaf]
}

trait LDParent extends LDTree with PrimitiveParent {
  def leafList: List[LDLeaf] = children.foldLeft(Nil:List[LDLeaf])((n,x) => x.leafList ::: n)
  override def children: List[LDChild]
  def toList: List[LDTree] = this :: children.foldLeft(Nil:List[LDTree])((n,x) => x.toList ::: n)
  def diffWithPi = children.map(_.diffWithPi).reduceLeft((n,x) => (n,x).zipped.map(_ + _))
  def diffWithT = children.foldLeft(Nil:List[Array[Double]])((n,x) => x.diffWithT ::: n)
  def diffWithB = children.map(_.diffWithB).reduceLeft((n,x) => (n,x).zipped.map(_ + _))
  def suffStat: List[NsFd] = children.foldLeft(Nil:List[NsFd]){(ns,x) => x.suffStat ::: ns}
}

case class LDLeaf(name:String, t:Double, trans:ML, alpha:Array[VL], beta:Array[VL],
                post:Array[MD], model:Model) extends LDChild with PrimitiveLeaf{
  def suffStat:List[NsFd] = nsAndfd :: Nil
  def toList:List[LDTree] = this :: Nil
  def diffWithPi = ldp
  def diffWithT = ldt :: Nil
  def diffWithB = ldb
  def leafList = this :: Nil
}

case class LDNode(children:List[LDChild], t:Double, trans:ML, alpha:Array[VL],
                beta:Array[VL], post:Array[MD], model:Model) extends LDChild with LDParent with PrimitiveNode{
  override def suffStat:List[NsFd] = nsAndfd :: super.suffStat
  override def diffWithT = ldt :: super.diffWithT
  override def diffWithB = (super.diffWithB, ldb).zipped.map(_ + _)
  override def diffWithPi = (super.diffWithPi, ldp).zipped.map(_ + _)
}

case class LDRoot(children:List[LDChild], trans:ML, alpha:Array[VL],
                beta:Array[VL], post:Array[MD], model:Model) extends PrimitiveRoot with LDParent{

  override def diffWithPi = (super.diffWithPi, nsArray.map(x => x :/ model.pi)).zipped.map(_ + _)
  override def diffWithB = super.diffWithB
  override def diffWithT = super.diffWithT.reverse

  lazy val likelihood: Array[LogDouble] = {
    require(alpha.nonEmpty)
    alpha.map(model.pi.toLogDouble.t * _)
  }

  override def leafList = super.leafList.reverse

  lazy val loglikelihood: Array[Double] = likelihood.map(_.logValue)

  def suffStats:(VD,List[MD],List[VD]) = {
    val (x,y) = suffStat.reverse.unzip
    (ns,x,y)
  }

  protected def ns: VD = nsArray.reduce(_ + _)

  protected def nsArray: Array[VD] = post.map(x => diag(x))

  override def toList = super.toList.reverse
}

object LDLeaf extends LDTreeUtilTrait{
  def inside(name:String,t:Double,column:Array[Base],m:Model):LDLeaf = {
    val alpha = mkAlpha(column)
    val trans = mkTrans(t,m)
    new LDLeaf(name,t,trans,alpha,null,null,m)
  }

  protected def mkAlpha(column:Array[Base]): Array[VL] = column.map{
    case x if x.nonNuc =>
      DenseVector.ones[LogDouble](4)
    case x =>
      val tmp = DenseVector.zeros[Double](4)
      tmp(x.toInt) = 1.0
      tmp.toLogDouble
  }
}

object LDNode extends LDTreeUtilTrait{
  def inside(ch:List[LDChild],t:Double,m:Model,fromChildren:List[Array[VL]]) = {
    val alpha = mkAlpha(fromChildren)
    val trans = mkTrans(t,m)
    new LDNode(ch,t,trans,alpha,null,null,m)
  }
}

object LDRoot extends LDTreeUtilTrait{
  def inside(ch:List[LDChild],m:Model,fromChildren:List[Array[VL]]) = {
    val alpha = mkAlpha(fromChildren)
    val trans = diag(DenseVector.ones[Double](4)).toLogDouble
    new LDRoot(ch,trans,alpha,null,null,m)
  }
}

trait LDTreeUtilTrait {
  def mkAlpha(fromChildren:List[Array[VL]]):Array[VL] =
    fromChildren.reduceLeft{(ns,xs) => (ns,xs).zipped.map(_ :* _)}
  def mkBeta(fromSib:List[Array[VL]],fromPar:Array[VL]):Array[VL] =
    (mkAlpha(fromSib),fromPar).zipped.map(_ :* _)
  def mkTrans(ti:Double,mi:Model):ML = {
    val tmp: DenseMatrix[Double] = mi.u * diag(exp(mi.lambda * ti)).*(mi.ui)
    tmp.toLogDouble
  }
  def mkPost(alpha:Array[VL],beta:Array[VL], trans:ML,likelihood:Array[LogDouble]): Array[MD] = {
    def f(a: VL, b: VL, l:LogDouble): MD = {
      val p = DenseMatrix.zeros[Double](4, 4)
      for (i <- 0 to 3; j <- 0 to 3) p(i, j) = (a(i) * b(j) * trans(i, j) / l).value
      p
    }
    (alpha,beta,likelihood).zipped.map((a,b,l) => f(a,b,l))
  }
}
