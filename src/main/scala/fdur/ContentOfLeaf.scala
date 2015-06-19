package fdur

import alignment.Base

case class ContentOfLeaf(var tx:Double,var nuc:Base) extends Content(tx){
  override def format(){
    alpha(0 to 3) := 0.0
    beta(0 to 3) := 0.0
    posterior(0 to 3,0 to 3) := 0.0
    nuc = Base.N
    isNull = false
  }
}
