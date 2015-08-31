import breeze.linalg.{DenseMatrix, DenseVector}

import scala.annotation.tailrec
import scala.util.Random

package object fdur2 {
  import alignment.Base

  type VD = DenseVector[Double]
  type MD = DenseMatrix[Double]

  def div[T](seqs:List[Array[T]],size:Int):Array[List[Array[T]]] = {
    @tailrec
    def f(xs:List[Array[T]],ys:List[List[Array[T]]],index:Int):Array[List[Array[T]]] = {
      if (xs.isEmpty) ys.reverse.toArray
      else {
        val (target, reserve) = xs.map{x => x.splitAt(index)}.unzip
        f(reserve, target :: ys, index)
      }
    }
    f(seqs,Nil,size)
  }

  def randMaf(tr:PrimitiveTree,param:Parameters,num:Int,pernum:Int) = {
    require(num > pernum && num % pernum == 0)
    val m = Model(param)
    val gen = new Random(0)
    val rootBase = Base.fromInt(gen.nextInt(4))


  }
}
