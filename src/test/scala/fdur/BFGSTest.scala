package fdur

import breeze.linalg._
import breeze.numerics.{exp,pow}
import breeze.optimize.{DiffFunction, LBFGS}

import org.scalatest.FunSuite
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.ArrayBuffer
import breeze.plot._

class BFGSTest extends FunSuite with LazyLogging {
  val nh = "src/test/resources/fdur/fdur.nh"
  val maf = "src/test/resources/fdur/fdur.maf"
  /*val pi = DenseVector(0.25469972246441502, 0.2452686122063337, 0.24531127848266232, 0.25472038684658888)
  val b = DenseVector(0.81053441227174539, 2.4236183781739533, 0.65677131469221517,
                      0.88544145555567511, 2.4233580444379776, 0.8106412600752263)
*/
  val pi = DenseVector(0.23137857635453807, 0.28408070157281884, 0.27729375318455474, 0.20724696888808836)
  val b = DenseVector(0.6586096484894902, 2.329377965568423, 0.8207872557873153, 0.9101830004835019, 2.7967009808428305, 0.5488972207907554)
  test("graddescent") {
    val (brnch,param) = Optimizer.gd(1000,nh,maf,pi,b)
    println(param)
    println(brnch)
    val m = Model(param)
    println(m.R)
  }

  /*test("EM") {
    val (brnch,param) = Optimizer.em(10000,nh,maf,pi,b)
    println(param)
    println(brnch)
    val m = Model(param)
    println(m.R)
  }*/

/*  class ParamLog(val lgl:ArrayBuffer[Double],val b:Array[ArrayBuffer[Double]],
                 val pi:Array[ArrayBuffer[Double]],val brnch:Array[ArrayBuffer[Double]]){
    protected def append(lglx:Double,bx:VD,pix:VD,brnchx:Array[Double]){
      lgl += lglx
      (b,bx.toArray).zipped.foreach((x,y) => x += y)
      (pi,pix.toArray).zipped.foreach((x,y) => x += y)
      (brnch,brnchx).zipped.foreach((x,y) => x += y)
    }

    def append(lglx:Double,p:VD): Unit ={
      val param = Parameters(p(0 to 9))
      append(lglx, param.Bvec, param.pi, p(10 until p.length).toArray)
    }

    def show = {
      val n = lgl.length
      require(n == pi.head.length)
      require(b.head.length == n)
      require(n == brnch.head.length)
      innerShow(pi,"target/pi.png","iteration","pi",n)
      innerShow(b,"target/b.png","iteration","b",n)
      innerShow(brnch,"target/brnch.png","iteration","branch_length",n)
      lglshow
    }

    protected def lglshow = {
      val f = Figure()
      val p = f.subplot(0)
      val x = DenseVector(Array.range(0,lgl.length).map(_.toDouble))
      p += plot(x, DenseVector(lgl.toArray))
      p.xlabel = "iteration"
      p.ylabel = "log_likelihood"
      f.saveas("target/lgl.png")
    }

    protected def innerShow(xs:Array[ArrayBuffer[Double]],out:String,xlab:String,ylab:String,n:Int) = {
      val f = Figure()
      val p = f.subplot(0)
      val x = DenseVector(Array.range(0,n).map(_.toDouble))
      for(i <- xs.indices){p += plot(x, DenseVector(xs(i).toArray))}
      p.xlabel = xlab
      p.ylabel = ylab
      f.saveas(out)
    }
  }

  object ParamLog{
    def apply(n:Int) = {
      new ParamLog(ArrayBuffer[Double](),
        Array.fill(6)(ArrayBuffer[Double]()),
        Array.fill(4)(ArrayBuffer[Double]()),
        Array.fill(n)(ArrayBuffer[Double]()))
    }
  }
  */
}

