package phylozzii.branco

import alignment.Base
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math.LogDouble
import phylozzii.fdur._


case class LDLeaf(name:String, t:Double, trans:ML, transD:ML, alpha:Array[VL], alphaD:Array[VL], beta:Array[VL],
                  betaD:Array[VL], model:Model) extends LDChild with PrimitiveLeaf{
  def toList:List[LDTree] = this :: Nil
  def leafList = this :: Nil
  def bls(lgl:Array[Double]) =
    (innerBls(lgl,alphaD, beta, dltA),innerBls(lgl,alpha, betaD, dltB)).zipped.map(_ + _)
  def blsa(lgl:Array[Double]) =
    innerBls(lgl, alphaD, beta, dltA)
}



object LDLeaf extends LDTreeUtilTrait{
  def inside(name:String, t:Double, column:Array[Base],m:Model,isTarget:Boolean,targetColumn: Array[Base]):LDLeaf = {
    val alpha = mkAlpha(column, targetColumn)
    val alphaD =
      if(isTarget) alpha
      else         Array.fill(alpha.length)(DenseVector.zeros[Double](4).toLogDouble)
    val trans = mkTrans(t,m)
    val transD = DenseMatrix.zeros[Double](4,4).toLogDouble
    for(i <- 0 to 3) {transD(i,i) = trans(i,i)}
    new LDLeaf(name,t,trans,transD,alpha,alphaD,null,null,m)
  }

  protected def mkAlpha(column: Array[Base], targetColmun: Array[Base]): Array[VL] = {
    (column, targetColmun).zipped.map {
      case (x, tgt) if x.nonNuc =>
        val tmp = DenseVector.ones[LogDouble](4)
        tmp(tgt.toInt) = LogDoubleZero
        tmp
      case (x, _) =>
        val tmp = DenseVector.zeros[Double](4)
        tmp(x.toInt) = 1.0
        tmp.toLogDouble
      case _ =>
        sys.error("mkAlpha error.")
    }
  }
}