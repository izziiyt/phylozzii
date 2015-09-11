package fdur

import breeze.linalg.{DenseVector, sum}
import breeze.numerics.{exp,pow}
import breeze.optimize.{DiffFunction, LBFGS}
import org.scalatest.FunSuite
import com.typesafe.scalalogging.LazyLogging

class BFGSTest extends FunSuite with LazyLogging {
  val nh = "src/test/resources/fdur/small.nh"
  val maf = "src/test/resources/fdur/tmp.maf"
  test("hoge"){
    val cols = Maf.readMaf(maf,1000)
    val template = ModelTree.fromFile(nh)
    val lbfgs = new LBFGS[VD](maxIter = 100,m = 6)

    val f = new DiffFunction[VD] {
      def calculate(p:VD) = {
        val param = Parameters(p(0 to 9))
        val branch = p(10 until p.length).toArray.toList
        val tree = template.changeBranches(branch)
        val model = Model(param)
        val diffs = cols.map(EM.gradMap(tree,_,model))
        val (regb,regpi) = mkreg(p(0 to 9))
        val tmp = EM.gradReduce(diffs,regb,regpi)
        logger.info(tmp._1.toString)
        logger.info(tmp._2.toString)
        tmp
      }
    }

    val iniparam = DenseVector.ones[Double](9 + template.length)
    val optparam = lbfgs.minimize(f,iniparam)
    val param = Parameters(optparam(0 to 9))
    //println(f.valueAt(optparam))
    //println(f.gradientAt(optparam))
    val brnch = optparam(10 until optparam.length).toArray.toList
    println(param)
    println(brnch)
    val x = getClass().getClassLoader().getResource("logback.xml")
    println(x)
  }

  protected def mkreg(p:VD) = {
    require(p.length == 10)
    val expp = exp(p)
    val b = expp(0 to 5)
    val pi = expp(6 to 9)
    val breg = b.map(x => (sum(b) - x) * x / pow(sum(b),2.0))
    val pireg = b.map(x => (sum(b) - x) * x / pow(sum(b),2.0))
    (breg,pireg)
  }
}
