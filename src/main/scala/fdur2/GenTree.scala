/*package fdur2

import alignment.Base
import breeze.linalg.{diag, DenseVector}
import breeze.numerics._

import scala.util.Random

trait GenTree extends PrimitiveTree {
  def genCols:List[Array[Base]]
  def genBase(pi:DenseVector[Double]) = {
    val r = Random.nextDouble()
    val v = pi.toArray.scanLeft(0.0)(_ + _).tail
    val i = (0 to 3).foldLeft(0){(n,i) => if(v(i) < r) n+1 else n}
    if(i > 3) Base.fromInt(3) else Base.fromInt(i)
  }
}
trait GenParent extends GenTree with PrimitiveParent {
  def genCols: List[Array[Base]] = children.foldLeft(Nil:List[Array[Base]]){(n,x) => x.genCols ::: n}
}
trait GenChild extends GenTree with PrimitiveChild
case class GenRoot(children:List[GenChild],pi:DenseVector[Double]) extends GenParent with PrimitiveRoot {
  def genCols(n:Int) = {
    val bases = Array.fill(n)(genBase(pi))


  }
}
case class GenNode(children:List[GenChild],pi:DenseVector[Double],t:Double) extends GenParent with GenChild with PrimitiveNode
case class GenLeaf(name:String,pi:DenseVector[Double],t:Double) extends GenChild with PrimitiveLeaf {
  def genCols =
}

object GenTree {
  def apply(tree:ModelRoot,pi:DenseVector[Double],model:Model):GenRoot = {
    require(pi.length == 4)
    val newch = tree.children.map(construct(_,pi,model))
    GenRoot(newch,pi)
  }

  protected def mkTrans(t:Double,m:Model):MD = m.u * diag(exp(m.lambda * t)).*(m.ui)

  protected def construct(ch:ModelChild,pi:DenseVector[Double],model:Model):GenChild = {
    def mkpi(t:Double) = mkTrans(t,model) * pi
    ch match {
      case ModelLeaf(name,t) =>
        GenLeaf(name,mkpi(t),t)
      case ModelNode(chr,t) =>
        val thispi = mkpi(t)
        val newch = chr.map(construct(_,thispi,model))
        GenNode(newch,thispi,t)
    }
  }

}
*/