import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest.FunSuite
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math._

class EM$Test extends FunSuite {
  val em = new EM

  /*test("EM"){
   /*
   This test is collect when all regularized phylogency tree's branches length are integral.
   test.al: single colmn is written
   test.nh: three-species phylogency tree is written
   */
    Util.printExecutionTime(em.test(2000,"src/test/resources/test.nh","src/test/resources/test.al"),"EM")
  }*/

  /*test("EMQsub"){

    val dir = "src/test/resources/alignments/"
    val cdir = "src/test/resources/count/"
    val alFiles = new java.io.File(dir).listFiles.map(_.getName)
    val countFileList = new java.io.File(cdir).listFiles.map(cdir + _.getName)
    for(i <- 1 to 10){
      alFiles.foreach(f => Estep.exe("src/test/resources/param.txt",dir + f,
        "src/test/resources/ce10.7way.nh",cdir + f + ".ct"))
      Mstep.exe(countFileList,"src/test/resources/param.txt","src/test/resources/ce10.7way.nh","src/test/resources/log")
    }

    PostProc.exe("src/test/resources/log/","src/test/resources/ce10.7way.nh","src/test/resources/param.txt","target/rtree.txt")
    }*/
  test("maf2al"){
    val s = Source.fromFile("src/test/resources/test.maf")
    val lines = s.getLines().map(l => l.map(Maf2Alignments.trans)).toArray
    val tmp = for(i <- 0 until lines(0).length)yield {Array(lines(0)(i).toChar,lines(1)(i).toChar,lines(2)(i).toChar,lines(3)(i).toChar)}
    val em = new EM
    em.test(30,"src/test/resources/ce10.7way.nh",tmp.toArray)
    }
  /*test("M"){
    val em = new TestEM
    val alignments = Util.getAlignments("src/test/resources/1.al")
    val pt = new PhylogencyTree("src/test/resources/ce10.7way.nh",GTR())
    val counts = alignments.map(em.eStep(pt,_))
    em.testMstep(pt,counts)
  }*/
  /*test("EMnext"){
    val em = new EM
    Util.printExecutionTime(em.test(10,"src/test/resources/ce10.7way.nh","src/test/resources/1.al"),"EM")
  }*/
  class TestEM extends EM {
    def testMstep(pt:PhylogencyTree,counts:Array[Count]):PhylogencyTree = {
      val sumCount = counts.reduce(_+_) / counts.length //column-wise means
      val Ns = sumCount.Ns.reduce(_+_)
      val Td:DenseVector[Double] = (sumCount.Fd,pt.branches).zipped.map(_*_).reduce(_+_)
      testQ1(sumCount.Fd,sumCount.Ns,newB(Ns,Td,pt.model),pt,Ns)
      val tmp = new PhylogencyTree(pt,GTR(Parameters(newB(Ns,Td,pt.model),newPi(Ns,Td,sumCount.ns,pt.model))))
      tmp.setBranch(newT(sumCount.Ns,sumCount.Fd,pt.model))
      tmp
    }
    def testQ1(Fd:List[DenseVector[Double]],Ns:List[DenseMatrix[Double]],newB:DenseVector[Double],pt:PhylogencyTree,NNs:DenseMatrix[Double]):Boolean = {
      val tmp = for(i <- 0 until pt.branches.length) yield
        {{pt.branches(i)*(pt.model.pi(2)*Fd(i)(3)+pt.model.pi(3)*Fd(i)(2))} - {(Ns(i)(2,3) + Ns(i)(3,2)) / newB(5)}}
      val r = tmp.sum
      println("difference is: " + r)
      Util.doubleChecker(r,0.0)
    }
  }
}

