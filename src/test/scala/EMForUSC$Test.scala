/**import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest.FunSuite
import breeze.linalg._
import scala.collection.mutable.ListBuffer

/**
 * Created by yuto on 14/06/14.
 */
class EMForUSC$Test extends FunSuite {

  /**test("diagDecomp"){
    val (a,l,u,ut) = EMForUSC.diagDecomp(Array(1,2))
    println(a)
    val la = DenseMatrix((l(0),0.0,0.0),(0.0,l(1),0.0),(0.0,0.0,l(2)))
    println(la)
    val xi = DenseMatrix((1.0,2.0),(3.0,1.0))
    val xj = DenseMatrix((1.0,1.0),(3.0,1.0))
    println(xi * xj)
    println(u * la * u.t)
  }*/

  test("divExpMatrix"){
    val y = -16 * math.exp(1) - 48 * math.exp(2) + 162 * math.exp(3)
    val lambda = DenseVector[Double](1.0,2.0,3.0)
    val u = DenseMatrix((1.0,2.0,3.0),(1.0,2.0,3.0),(1.0,2.0,3.0))
    val x = EMForUSC.divExpMatrix((2,1),2,1,lambda,u,u.t)
    assert(math.abs(x-y) < math.pow(10,-10))
  }

 test("apply"){
    val data = Array[(Int,Int)]((0,2),(3,2),(3,2),(4,2),(1,2))
    for(i <- 0 until 10) {
      val f1 = "/home/yuto/data/" + i.toString + ".txt"
      val f2 = "/home/yuto/data/" + i.toString + ".png"
      EMForUSC(data,4,loop=50000,f1,f2)
    }
  }

  test("foldLeft"){
    val x = (0 until 10).foldLeft(0)((a,b) => a + b)
    assert(x == 45)
  }

  test("ListBuffer"){
    val x = ListBuffer[Int](2)
    for(i <- 0 until 10){x += i}
    println(x)

  }

 /** test("Nd and Fd"){
    val (a,l,u,ut) = EMForUSC.diagDecomp(Array(0.1,0.2,0.3))
    def f(i:Int,j:Int)(pair:(Int,Int)) = EMForUSC.divExpMatrix(pair,i,j,l,u,ut)
    val x = (0 until 4) map (i => f(i,i)((1,2)))
    println("Fs")
    println(x)
    println(a(0,1))
    val y = (0 until 4) map (i => (0 until 4) map (j => a(i,j) * f(i,j)((1,2))))
    println("Nd")
    y.foreach(println(_))
  } */

  /**test("symeig"){
    val x = DenseMatrix((2.0,2.0),(1.0,3.0))
    val (l,u) = eig(x)
    println(l)
    println(u)
  }*/
}
**/