package fdur

import alignment.Base

case class ContentOfLeaf(var tx:Double,var nuc:Base) extends Content(tx){
  def format(x:Base){
    beta(0 to 3) := 0.0
    posterior(0 to 3,0 to 3) := 0.0
    nuc = x
    if(x.isN){
      alpha(0 to 3) := 1.0
      isNull = true
    }
    else{
      alpha(0 to 3) := 0.0
      alpha(x.toInt) = 1.0
      isNull = false
    }
  }
}
