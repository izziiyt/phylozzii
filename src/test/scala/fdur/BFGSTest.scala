package fdur

import breeze.linalg._
import breeze.numerics.{exp,pow}
import breeze.optimize.{DiffFunction, LBFGS}

import org.scalatest.FunSuite
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.ArrayBuffer
import breeze.plot._

class BFGSTest extends FunSuite with LazyLogging {
  val nh = "src/test/resources/fdur/small.nh"
  val maf = "src/test/resources/fdur/small.maf"
  test("graddescent") {
    val cols = Maf.readMaf(maf, 100)
    val template = ModelTree.fromFile(nh)
    //val plog = ParamLog(template.branches.length)

    val f = new DiffFunction[VD] {
      def calculate(p: VD) = {
        val param = Parameters(p(0 to 9))
        val branch = p(10 until p.length).toArray.toList
        val tree = template.changeBranches(branch)
        val model = Model(param)
        val diffs = cols.map(EM.gradMap(tree, _, model))
        val (regb, regpi) = mkreg(param)
        val (nlgl, newp) = EM.gradReduce(diffs, regb, regpi)
      //  plog.append(nlgl,newp)
        (nlgl,newp)
      }
    }

    val lbfgs = new LBFGS[VD](maxIter = 1000, m = 3)
    val iniparam = DenseVector.ones[Double](10 + template.branches.length)
    val optparam = lbfgs.minimize(f, iniparam)
    val param = Parameters(optparam(0 to 9))
    val brnch = optparam(10 until optparam.length).toArray.toList
    val (rbr, rpa) = EM.regularize(brnch,param)
    println(rpa)
    println(rbr)
    //plog.show
  }

  test("EM") {
    val (brnch,param) =EM.exe(100,nh,maf)
    println(param)
    println(brnch)
  }

  protected def mkreg(p: Parameters) = {
    val pitmp = DenseMatrix.vertcat(p.pi.asDenseMatrix,p.pi.asDenseMatrix,p.pi.asDenseMatrix,p.pi.asDenseMatrix)
    val pi = diag(p.pi) * (DenseMatrix.eye[Double](4) - pitmp)
    val btmp = DenseMatrix.vertcat(p.Bvec.asDenseMatrix, p.Bvec.asDenseMatrix,
      p.Bvec.asDenseMatrix, p.Bvec.asDenseMatrix, p.Bvec.asDenseMatrix, p.Bvec.asDenseMatrix)
    val b = diag(p.Bvec) * (DenseMatrix.eye[Double](6) - btmp)
    (b, pi)
  }

  class ParamLog(val lgl:ArrayBuffer[Double],val b:Array[ArrayBuffer[Double]],
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
      if(out.endsWith("pi.png")){
        xs.foreach{y => y.foreach(x => print(x + " ")) ;println}
      }
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
}

