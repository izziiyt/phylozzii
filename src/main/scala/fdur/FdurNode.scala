package fdur

import breeze.linalg.DenseVector
import alignment.Base

case class FdurNode(left:FdurTree,right:FdurTree,cont:Content) extends FdurTree{

  def setColumn(x:Array[Base]):Array[Base] = {
    val tmp = left.setColumn(x)
    right.setColumn(tmp)
  }

  def isNull = cont.isNull

  def format() = {
    cont.format()
    right.format()
    left.format()
    this
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
    if(isNull) setPosteriorNull(likelihood,cont.beta)
    else{
      cont.setPosterior(likelihood)
      left.setPosterior(likelihood)
      right.setPosterior(likelihood)
    }
  }

  def setPosteriorNull(likelihood:Double,beta:DenseVector[Double]){
    cont.setPosteriorNull(likelihood,beta)
    left.setPosteriorNull(likelihood,beta)
    right.setPosteriorNull(likelihood,beta)
  }

  def setTransition(m:EvolutionModel){
    cont.setTransProb(m)
    left.setTransition(m)
    right.setTransition(m)
  }
}
