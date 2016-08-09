package phylozzii.branco

import breeze.linalg.DenseMatrix
import phylozzii.fdur.{Model, PrimitiveNode, _}


case class LDNode(children:List[LDChild], t:Double, trans:ML, transD:ML,alpha:Array[VL], alphaD:Array[VL],
                  beta:Array[VL], betaD:Array[VL], model:Model) extends LDChild with LDParent with PrimitiveNode {
  override def bls(lgl:Array[Double]) = {
    val ax = innerBls(lgl,alphaD, beta, dltA)
    val bx = innerBls(lgl,alpha, betaD, dltB)
    (super.bls(lgl), ax, bx).zipped.map(_ + _ + _)
  }
  override def blsa(lgl:Array[Double]) = {
    val ax = innerBls(lgl,alphaD, beta, dltA)
    (super.blsa(lgl), ax).zipped.map(_ + _)
  }
}

object LDNode extends LDTreeUtilTrait{
  def inside(ch:List[LDChild], t:Double, m:Model) = {
    val props = ch.map(_.insideProp)
    val alpha = mkAlpha(props)
    val alphaD = mkAlphaD(props, ch.map(_.insidePropD))
    val trans = mkTrans(t,m)
    val transD = DenseMatrix.zeros[Double](4,4).toLogDouble
    for(i <- 0 to 3) {transD(i,i) = trans(i,i)}
    new LDNode(ch,t,trans,transD,alpha,alphaD,null,null,m)
  }
}

