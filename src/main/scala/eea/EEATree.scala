package eea

import java.io.FileReader
import alignment.Base
import breeze.linalg.{DenseMatrix, DenseVector,diag}
import fdur._

trait EEATree extends Tree {

  def setModel(e:EvolutionModel):Unit

  def setTarget(n:String):Unit

  def inside:DenseVector[Double]

  protected def outside:DenseVector[Double]

  def calcOutside(fromPar:DenseVector[Double],fromSib:DenseVector[Double]):Unit

  def calcOutsideD(fromPar:DenseVector[Double],fromSib:DenseVector[Double],fromParD:DenseVector[Double],fromSibD:DenseVector[Double]):Unit = Unit

  def insideD:DenseVector[Double]

  def outsideD:DenseVector[Double]

  protected def m:EvolutionModel

  def transProb:DenseMatrix[Double] // cell of i-th row and j-th column is transition probability(base i -> base j)

  def transProbD:DenseMatrix[Double]

}

trait HavingChildren extends EEATree {

  def left:EEATree

  def right:EEATree

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

  def calcOutside(fromPar:DenseVector[Double],fromSib:DenseVector[Double]):Unit = {
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

  def setColumn(column:Array[Base]) = {
    val tmp = left.setColumn(column)
    right.setColumn(tmp)
  }

}

abstract class HavingParent(val br:Double) extends EEATree {

  val transProb = m.u * diag(m.lambda.map { x => math.exp(x * br) }) * m.ui
  val transProbD = transProb
  for (i <- 0 to 3; j <- 0 to 3; if i != j) {
    transProbD(i, j) = 0.0
  }

  var outside:DenseVector[Double] = null

  var outsideD:DenseVector[Double] = null

}

case class EEALeaf(name:String,b:Double) extends HavingParent(b) {

  var base:Base = Base.N

  protected var m:EvolutionModel = null

  var insideD:DenseVector[Double] = null

  def setTarget(n:String):Unit = {
    insideD =
      if(name == n) DenseVector((0 to 3).map{x => if(x == base.toInt) 1.0 else 0.0}.toArray)
      else DenseVector.zeros[Double](4)
  }

  def setModel(e:EvolutionModel):Unit = {
    m = e
  }

  def setColumn(column:Array[Base]) = {
    base = column.head
    if(!column.isEmpty) column.tail else Array(Base.N)
  }

  lazy val inside:DenseVector[Double] =
    DenseVector((0 to 3).map{x => if(x == base.toInt) 1.0 else 0.0}.toArray)

  def insideD(n:String):DenseVector[Double] =
    if(n == name) inside else DenseVector.zeros[Double](4)

  def calcOutside(fromPar:DenseVector[Double],fromSib:DenseVector[Double]):Unit = {
    outside = fromPar :* fromSib
  }

  override def calcOutsideD(fromPar:DenseVector[Double],fromSib:DenseVector[Double],fromParD:DenseVector[Double],fromSibD:DenseVector[Double]): Unit ={
    outsideD = fromPar :* fromSibD + fromParD :* fromSib
  }

}

case class EEARoot(left:EEATree,right:EEATree) extends HavingChildren {

  protected var m:EvolutionModel = null

  protected var hasModel = false

  protected var hasTarget = false

  protected var hasColumn = false

  var transProb = DenseMatrix.zeros[Double](4,4)

  var transProbD = DenseMatrix.zeros[Double](4,4)

  override def setModel(e:EvolutionModel):Unit = {
    hasModel = true
    super.setModel(e)
  }

  override def setTarget(n:String):Unit = {
    hasTarget = true
    super.setTarget(n)
  }

  override def setColumn(column:Array[Base]) = {
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

}

case class EEANode(left:EEATree,right:EEATree,override val br:Double) extends HavingParent(br) with HavingChildren{

  protected var m:EvolutionModel = null

  override def calcOutside(fromPar:DenseVector[Double],fromSib:DenseVector[Double]):Unit = {
    outside = fromPar :* fromSib
    super[HavingChildren].calcOutside(fromPar,fromSib)
  }

  override def calcOutsideD(fromPar:DenseVector[Double],fromSib:DenseVector[Double],fromParD:DenseVector[Double],fromSibD:DenseVector[Double]):Unit = {
    outsideD = fromPar :* fromSibD + fromParD :* fromSib
    val fromThis = transProb.t * outside
    val fromThisD = transProbD.t * outsideD
    left.calcOutsideD(fromThis,right.transProbD * right.insideD,fromThisD,right.transProb * right.inside)
    right.calcOutsideD(fromThis,left.transProbD * left.insideD,fromThisD,left.transProb * left.inside)
  }

}

object EEATree extends NHParser4EEA{

  def fromFile(nhFile:String):EEARoot = {
    val reader = new FileReader(nhFile)
    val tmp = parseAll(tree,reader).get
    reader.close()
    tmp
  }

  def fromString(nhString:String):EEARoot = {
    parseAll(tree,nhString).get
  }

}

class NHParser4EEA extends NHParser[EEATree] {

  def tree:Parser[EEARoot] =  nodePair<~";"  ^^
    {case (left,right) => EEARoot(left,right)}

  def node:Parser[EEATree] = nodePair~":"~value ^^
    {case (left,right)~":"~value => EEANode(left,right,value.toDouble)} | leaf

  def leaf:Parser[EEALeaf] = name~":"~value ^^
    {case name~":"~value => EEALeaf(name,value.toDouble)}

}