package fdur

import breeze.linalg.DenseVector
import alignment.Base

case class FdurLeaf(species:String,cont:ContentOfLeaf) extends FdurTree{

  def setBranch(x:List[Double]) = {
    cont.t_=(x.head)
    x.tail
  }

  def setPosteriorNull(likelihood:Double,beta:DenseVector[Double]){cont.setPosteriorNull(likelihood,beta)}

  def isNull = cont.isNull

  override def toString = species + ":" + cont.t

  def branches = List(cont.t)

  def names = List(species)

  def setColumn(x:Array[Base]):Array[Base] = {
    cont.nuc_=(x.head)
    x.tail
  }

  def format() = {
    cont.format()
    this
  }

  def setPosterior(likelihood:Double){
    cont.setPosterior(likelihood)
  }

  def setTransition(m:EvolutionModel){
    cont.setTransProb(m)
  }

}
