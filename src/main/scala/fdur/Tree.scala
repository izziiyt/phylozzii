package fdur

import breeze.linalg._
import alignment.Base
import breeze.numerics.exp
import scala.annotation.tailrec

trait Tree extends PrimitiveTree{
  type NsFd = (MD,VD)
  def alpha: Array[VD]
  def beta: Array[VD]
  def post: Array[MD]
  def trans: MD
  def model: Model
  lazy val insideProp: Array[VD] = {
    require(alpha.nonEmpty)
    alpha.map(a => trans.t * a)
  }
  def toList: List[Tree]
  def diffWithPi: Array[VD]
  def diffWithB: Array[MD]
  def diffWithT: List[Array[Double]]
}

object Tree extends TreeUtilTrait {
  type NsFd = Array[(MD,VD)]

  @tailrec
  protected def innerIn(ch: List[ModelChild], cols: List[Array[Base]], result: List[Child], model:Model):
  (List[Child], List[Array[Base]]) = {
    if (ch == Nil)
      (result.reverse, cols)
    else {
      val (newTree,newCols) = inside(ch.head,model,cols)
      innerIn(ch.tail, newCols, newTree :: result, model)
    }
  }

  protected def inside(tr: ModelChild, model: Model, columns: List[Array[Base]]): (Child, List[Array[Base]]) =
    tr match {
      case ModelLeaf(name,t) =>
        (Leaf.inside(name,t,columns.head,model),columns.tail)
      case ModelNode(ch,t) =>
        val tmp = innerIn(ch,columns,Nil,model)
        (Node.inside(tmp._1,t,model,tmp._1.map(_.insideProp)),tmp._2)
    }

  def inside(tr: ModelRoot, model: Model, columns: List[Array[Base]]): Root = {
    val (newCh,newCols) = innerIn(tr.children,columns,Nil,model)
    if(newCols.nonEmpty) sys.error("Bad Columns")
    Root.inside(newCh,model,newCh.map(_.insideProp))
  }

  @tailrec
  protected def innerOut(prefix:List[Child],suffix:List[Child],result:List[Child], fromThis:Array[VD],lh:Array[Double]):
  List[Child] = {
    if(prefix.isEmpty) result.reverse
    else{
      innerOut(prefix.tail, prefix.head :: suffix,
        outside(prefix.head,(suffix ::: prefix.tail).map(_.insideProp),fromThis,lh) :: result,fromThis,lh)
    }
  }

  protected def outside(tree:Child, fromSib:List[Array[VD]], fromPar:Array[VD],lh:Array[Double]): Child = {
    val beta = mkBeta(fromSib,fromPar)
    lazy val fromThis = outsideProp(beta,tree.trans)
    tree match {
      case Leaf(name, t, trans, alpha, _, _, model) =>
        Leaf(name,t,trans,alpha,beta,mkPost(alpha,beta,trans,lh),model)
      case Node(ch,t,trans,alpha,_,_,model) =>
        val newch = innerOut(ch,Nil,Nil,fromThis,lh)
        Node(newch,t,trans,alpha,beta,mkPost(alpha,beta,trans,lh),model)
    }
  }

  def outside(tr:Root):Root = {
    require(tr.alpha.nonEmpty)
    val n = tr.alpha.length
    val beta = Array.fill(n)(tr.model.pi)
    val fromThis = outsideProp(beta,tr.trans)
    val newch = innerOut(tr.children,Nil,Nil,fromThis,tr.likelihood)
    Root(newch,tr.trans,tr.alpha,beta,mkPost(tr.alpha,beta,tr.trans,tr.likelihood),tr.model)
  }

  protected def outsideProp(beta:Array[VD],trans:MD): Array[VD] = {
    require(beta.nonEmpty)
    beta.map(b => trans * b)
  }

  def inout(tr: ModelRoot, model: Model, columns: List[Array[Base]]): Root = {
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

trait Child extends Tree with PrimitiveChild{
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
        new DenseMatrix[Double](4, 4, tmp.toArray) / trans(end, start)
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

  def toList:List[Tree]

  def leafList:List[Leaf]
}

trait Parent extends Tree with PrimitiveParent {
  def leafList:List[Leaf] = children.foldLeft(Nil:List[Leaf])((n,x) => x.leafList ::: n)
  override def children:List[Child]
  def toList:List[Tree] = this :: children.foldLeft(Nil:List[Tree])((n,x) => x.toList ::: n)
  def diffWithPi = children.map(_.diffWithPi).reduceLeft((n,x) => (n,x).zipped.map(_ + _))
  def diffWithT = children.foldLeft(Nil:List[Array[Double]])((n,x) => x.diffWithT ::: n)
  def diffWithB = children.map(_.diffWithB).reduceLeft((n,x) => (n,x).zipped.map(_ + _))
  def suffStat:List[NsFd] = children.foldLeft(Nil:List[NsFd]){(ns,x) => x.suffStat ::: ns}
}

case class Leaf(name:String, t:Double, trans:MD, alpha:Array[VD], beta:Array[VD],
                post:Array[MD], model:Model) extends Child with PrimitiveLeaf{
  def suffStat:List[NsFd] = nsAndfd :: Nil
  def toList:List[Tree] = this :: Nil
  def diffWithPi = ldp
  def diffWithT = ldt :: Nil
  def diffWithB = ldb
  def leafList = this :: Nil
}

case class Node(children:List[Child], t:Double, trans:MD, alpha:Array[VD],
                beta:Array[VD], post:Array[MD], model:Model) extends Child with Parent with PrimitiveNode{
  override def suffStat:List[NsFd] = nsAndfd :: super.suffStat
  override def diffWithT = ldt :: super.diffWithT
  override def diffWithB = (super.diffWithB, ldb).zipped.map(_ + _)
  override def diffWithPi = (super.diffWithPi, ldp).zipped.map(_ + _)
}

case class Root(children:List[Child], trans:MD, alpha:Array[VD],
                beta:Array[VD], post:Array[MD], model:Model) extends PrimitiveRoot with Parent{

  override def diffWithPi = (super.diffWithPi, nsArray.map(x => x :/ model.pi)).zipped.map(_ + _)
  override def diffWithB = super.diffWithB
  override def diffWithT = super.diffWithT.reverse

  lazy val likelihood: Array[Double] = {
    require(alpha.nonEmpty)
    alpha.map(model.pi.t * _)
  }

  override def leafList = super.leafList.reverse

  lazy val loglikelihood: Array[Double] = likelihood.map(math.log)

  def suffStats:(VD,List[MD],List[VD]) = {
    val (x,y) = suffStat.reverse.unzip
    (ns,x,y)
  }

  protected def ns: VD = nsArray.reduce(_ + _)

  protected def nsArray: Array[VD] = post.map(diag(_))

  override def toList = super.toList.reverse
}

object Leaf extends TreeUtilTrait{
  def inside(name:String,t:Double,column:Array[Base],m:Model):Leaf = {
    val alpha = mkAlpha(column)
    val trans = mkTrans(t,m)
    new Leaf(name,t,trans,alpha,null,null,m)
  }

  protected def mkAlpha(column:Array[Base]) = column.map{
    case Base.N =>
      DenseVector.ones[Double](4)
    case x =>
      val tmp = DenseVector.zeros[Double](4)
      tmp(x.toInt) = 1.0
      tmp
  }
}

object Node extends TreeUtilTrait{
  def inside(ch:List[Child],t:Double,m:Model,fromChildren:List[Array[VD]]) = {
    val alpha = mkAlpha(fromChildren)
    val trans = mkTrans(t,m)
    new Node(ch,t,trans,alpha,null,null,m)
  }
}

object Root extends TreeUtilTrait{
  def inside(ch:List[Child],m:Model,fromChildren:List[Array[VD]]) = {
    val alpha = mkAlpha(fromChildren)
    val trans = diag(DenseVector.ones[Double](4))
    //val trans = mkTrans(0.0,m)
    new Root(ch,trans,alpha,null,null,m)
  }
}

trait TreeUtilTrait {
  def mkAlpha(fromChildren:List[Array[VD]]):Array[VD] =
    fromChildren.reduceLeft{(ns,xs) => (ns,xs).zipped.map(_ :* _)}
  def mkBeta(fromSib:List[Array[VD]],fromPar:Array[VD]):Array[VD] =
    (mkAlpha(fromSib),fromPar).zipped.map(_ :* _)
  def mkTrans(ti:Double,mi:Model):MD =
    mi.u * diag(exp(mi.lambda * ti)).*(mi.ui)
  def mkPost(alpha:Array[VD],beta:Array[VD], trans:MD,likelihood:Array[Double]): Array[MD] = {
    def f(a: VD, b: VD, l:Double): MD = {
      val p = DenseMatrix.zeros[Double](4, 4)
      for (i <- 0 to 3; j <- 0 to 3) p(i, j) = a(i) * b(j) * trans(i, j) / l
      p
    }
    (alpha,beta,likelihood).zipped.map((a,b,l) => f(a,b,l))
  }
}