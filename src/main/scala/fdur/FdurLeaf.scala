package fdur

import alignment.Base
import breeze.linalg.DenseVector

case class FdurLeaf(species:String,cont:ContentOfLeaf) extends FdurTree{

  def setBranch(x:List[Double]) = {
    cont.t_=(x.head)
    x.tail
  }

  def setPosteriorNull(likelihood:Double,beta:DenseVector[Double]){cont.setPosteriorNull(likelihood,beta)}

  override def toString = species + ":" + cont.t

  def branches = List(cont.t)

  def names = List(species)

  def setColumn(x:Array[Base]):Array[Base] = {
    cont.nuc_=(x.head)
    cont.isNull = if(cont.nuc.isN) true else false
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
