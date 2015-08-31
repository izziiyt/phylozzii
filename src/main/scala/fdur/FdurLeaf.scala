package fdur

import alignment.Base
import breeze.linalg.DenseVector

case class FdurLeaf(species:String,cont:ContentOfLeaf) extends FdurTree{

  def setBranch(x:List[Double]) = {
    cont.t_=(x.head)
    x.tail
  }

  override def toString = species + ":" + cont.t

  def branches = List(cont.t)

  def names = List(species)

  def formatWithColumn(x:Array[Base]): Array[Base]={
    cont.format(x.head)
    x.tail
  }

  def setPosterior(likelihood:Double){
    cont.setPosterior(likelihood)
  }

  def setTransition(m:EvolutionModel){
    cont.setTransProb(m)
  }

}
