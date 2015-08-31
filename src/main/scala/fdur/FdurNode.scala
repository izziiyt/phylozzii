package fdur

import breeze.linalg.DenseVector
import alignment.Base

case class FdurNode(left:FdurTree,right:FdurTree,cont:Content) extends FdurTree{

  def formatWithColumn(x:Array[Base]): Array[Base] ={
    val tmp1 = left.formatWithColumn(x)
    val tmp2 = right.formatWithColumn(tmp1)
    cont.isNull = left.isNull && right.isNull
    cont.format()
    tmp2
  }

  override def toString = cont match {
    case ContentOfRoot(_) => "(" + left + "," + right + ");"
    case ContentOfNode(_) => "(" + left + "," + right + "):" + cont.t
  }

  def names = left.names ::: right.names

  def branches = left.branches ::: right.branches ::: List(cont.t)

  def likelihood(m:EvolutionModel):Double = cont.likelihood(m)

  def collectn(m:EvolutionModel):DenseVector[Double] = (cont.alpha :* m.pi) / likelihood(m)

  def setBranch(x:List[Double]) = {
    val y = left.setBranch(x)
    val z = right.setBranch(y)
    cont.t_=(z.head)
    z.tail
  }

  def setPosterior(likelihood:Double){
    cont.setPosterior(likelihood)
    left.setPosterior(likelihood)
    right.setPosterior(likelihood)
  }

  def setTransition(m:EvolutionModel){
    cont.setTransProb(m)
    left.setTransition(m)
    right.setTransition(m)
  }
}
