/*package eea.tree

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