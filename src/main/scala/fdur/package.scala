import java.io.{PrintWriter, OutputStream}

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.annotation.tailrec
import scala.math._
import scala.util.Random

package object fdur {
  import alignment.Base

  type VD = DenseVector[Double]
  type MD = DenseMatrix[Double]


  def randMaf(tr:PrimitiveTree,param:Parameters,num:Int,pernum:Int) = {
    require(num > pernum && num % pernum == 0)
    val m = Model(param)
    val gen = new Random(0)
    val rootBase = Base.fromInt(gen.nextInt(4))


  }

  def printExeTime[T](proc: => T,txt:String,os:OutputStream=System.out) = {
    val start = System.currentTimeMillis
    val result = proc
    val writer = new PrintWriter(os)
    writer.println(txt + "\t" + (System.currentTimeMillis - start))
    writer.flush()
    result
  }

  def doubleEqual(x:Double,y:Double,th:Double = 1.0E-14):Boolean = abs(x - y) < th

  def doubleEqual(x:DenseVector[Double],y:DenseVector[Double]):Boolean = doubleEqual(x.toArray,y.toArray)

  def doubleEqual(x:Seq[Double],y:Seq[Double]):Boolean = (x,y).zipped.forall{(i,j) => doubleEqual(i,j)}

}
