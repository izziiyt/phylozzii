import breeze.linalg.{sum, diag, DenseMatrix, DenseVector}
import java.io.{PrintWriter, FileOutputStream}
import org.scalatest.FunSuite
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

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
    val ot = new FileOutputStream("src/test/resources/log/tm.log",true)
    for(i <- 1 to 20){
      alFiles.foreach(f => Util.printExecutionTime(Estep.exe("src/test/resources/param.txt",dir + f,
        "src/test/resources/ce10.7way.nh",cdir + f + ".ct"),"estep",ot))
      val countFileList = new java.io.File(cdir).listFiles.map(cdir + _.getName)
      Util.printExecutionTime(Mstep.exe(countFileList,
        "src/test/resources/param.txt","src/test/resources/ce10.7way.nh","src/test/resources/log"),"mstep",ot)
    }
    PostProc.exe("src/test/resources/log/","src/test/resources/ce10.7way.nh","src/test/resources/param.txt","target/rtree.txt")
  }*/

  /*test("maf2al"){
    val s = Source.fromFile("src/test/resources/test.maf")
    val lines = s.getLines().map(l => l.map(Maf2Alignments.trans)).toArray
    val tmp = for(i <- 0 until lines(0).length)yield {Array(lines(0)(i).toChar,lines(1)(i).toChar,lines(2)(i).toChar,lines(3)(i).toChar)}
    val em = new EM
    em.test(30,"src/test/resources/ce10.7way.nh",tmp.toArray)
  }*/
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

  /*test("makeAlignments"){
    AlGen("src/test/resources/my.tree")
  }*/

  test("Comparison with PAML"){
    //val al = Util.getAlignments("src/test/resources/241.al")
    //val al = Util.getAlignments("src/test/resources/brown.al")
    val al = Util.getAlignments("/home/yuto/sample.al")
    val out = new FileOutputStream("target/hoge")
    Util.printExecutionTime(em.test(100,"src/test/resources/hg19.100way.nh",al,out),"hoge")
    out.close()
  }

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

  object AlGen{
    def apply(tFile:String){
      exe(Tree.fromFile(tFile),Parameters(DenseVector(0.1,0.2,0.2,0.1,0.3,0.1),DenseVector(0.2,0.3,0.4,0.1)))
    }

    def distance(t:Double,m:EvolutionModel):DenseMatrix[Double] = {
      val tmp:DenseMatrix[Double] = diag(m.lambda.map(x => math.exp(x * t)))
      m.u * tmp * m.ui
    }

    def exe(tree:Tree,param:Parameters){
      val rgen = new Random()
      val br = tree.branches
      val m = GTR(param)
      val hc = distance(br(2),m)
      val h = distance(br(0),m)
      val c = distance(br(1),m)
      val g = distance(br(3),m)
      val write = new PrintWriter("src/test/resources/my.al")
      for(i <- 0 until 10000){
        val top = BaseGen(param.pi.toArray.toList,rgen)
        val hcTop = BaseGen(hc(top,::).t.toArray.toList,rgen)
        val hv = h(hcTop,::).t.toArray
        val cv = c(hcTop,::).t.toArray
        val gv = g(top,::).t.toArray
        val human = BaseGen(hv.toList,rgen)
        val chimp = BaseGen(cv.toList,rgen)
        val gollira = BaseGen(gv.toList,rgen)
        write.println(Array(human,chimp,gollira).mkString(" "))
      }




    }
  }

  object BaseGen{
    def apply(xs:List[Double],rgen:Random) = f(xs,rgen.nextDouble())

    @tailrec
    private def f(xs:List[Double],r:Double,i:Int = 0):Int = {
      val tmp = r - xs.head
      if(tmp > 0.0 && !xs.tail.isEmpty) f(xs.tail,tmp,i+1)
      else i
    }

  }
}

