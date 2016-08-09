package phylozzii.branco

import breeze.linalg.{DenseMatrix, DenseVector, diag}
import phylozzii.fdur.{Model, PrimitiveRoot, _}

case class LDRoot(children:List[LDChild], trans:ML, transD:ML, alpha:Array[VL], alphaD:Array[VL],
                  beta:Array[VL], betaD:Array[VL], model:Model) extends PrimitiveRoot with LDParent{
  override def leafList = super.leafList.reverse
  override def toList = super.toList.reverse
  def bls: Array[Double] = {
    val ll = alpha.map(a => a.value.t * model.pi)
    super.bls(ll)
  }
  def blsa: Array[Double] = {
    val lgl = alpha.map(a => a.value.t * model.pi)
    super.blsa(lgl)
  }
}

object LDRoot extends LDTreeUtilTrait{
  /**
    * returns LDRoot of which inside value is calculated
    * @param ch result LDRoot has
    * @param m
    * @return
    */
  def inside(ch: List[LDChild], m: Model): LDRoot = {
    val props = ch.map(_.insideProp)
    val alpha = mkAlpha(props)
    val alphaD = mkAlphaD(props, ch.map(_.insidePropD))
    val trans = diag(DenseVector.ones[Double](4)).toLogDouble
    val transD = DenseMatrix.zeros[Double](4,4).toLogDouble
    for(i <- 0 to 3) {transD(i,i) = trans(i,i)}
    new LDRoot(ch, trans, transD, alpha, alphaD, null, null, m)
  }
}