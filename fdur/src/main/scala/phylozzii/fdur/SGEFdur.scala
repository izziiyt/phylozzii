package phylozzii.fdur

import java.io._

import biformat.MafIterator
import breeze.linalg.{DenseMatrix, DenseVector}
import scala.util.parsing.combinator.JavaTokenParsers

object SGEFdur extends SuffStatParser{
  def qmstep(intermidiates: File, pif: File, pof: File, nhi: File, nho: File, log: File, cbf: Boolean): Unit = {
    val files = intermidiates.listFiles.filter(_.getName.endsWith(".txt"))
    val suffs = files.map(EMreader)
    val param = Parameters.fromFile(pif)
    val tree = ModelTree.fromFile(nhi)
    val (lgl, brnch, newparam) = Optimizer.mstep(suffs, Model(param), tree.branches)
    val newtree = tree.changeBranches(brnch)

    //prints new parameters
    if(!cbf) {
      val pw = new PrintWriter(pof)
      pw.println(if (cbf) Parameters(newparam.Bvec, param.pi).toString else newparam.toString, false)
      pw.close()
    }

    //prints new newick tree
    val nw = new PrintWriter(nho)
    nw.println(newtree.toString, false)
    nw.close()

    //prints log
    writeLog(newparam.Bvec.toArray.mkString(",") + "," + newparam.pi.toArray.mkString(","), new File(log.getAbsolutePath + "param.log.txt"))
    writeLog(newtree.branches.mkString(","), new File(log.getAbsolutePath + "tree.log.txt"))
    writeLog(lgl.toString, new File(log.getAbsolutePath + "lgl.log.txt"))
  }

  def writeLog(x:String, f: File): Unit = {
    val y = new PrintWriter(new FileWriter(f, true))
    y.println(x)
    y.close()
  }

  protected def EMreader(x:File): (VD, List[MD], List[VD], Double, Long) = {
    val reader = new FileReader(x)
    parseAll(suffstat,reader).get
  }

  def qestep(maf: File, nh: File, pf: File, out: File): Unit = {
    //println(scala.collection.parallel.availableProcessors)
    //${al} ${nh} ${count} target/time/e/${SGE_TASK_ID}.time
    val source = biformat.bigSource(maf)
    val its = MafIterator.fromSource(source, "hg19")
    val cols = readMaf(its)
    val tree = ModelTree.fromFile(nh)
    val param = Parameters.fromFile(pf)
    val results = cols.map { c => Optimizer.ldestep(tree, c, Model(param)) }
    val summed = results.reduce {
      (m, x) =>
        val ns = m._1 + x._1
        val Ns = (m._2, x._2).zipped.map(_ + _)
        val Fd = (m._3, x._3).zipped.map(_ + _)
        val l = m._4 + x._4
        val i = m._5 + x._5
        (ns, Ns, Fd, l, i)
    }
    EMprinter(summed, out)
  }

  protected def EMprinter(result:(VD,List[MD],List[VD],Double,Long),out:File): Unit = {
    val w = new BufferedWriter(new FileWriter(out))
    w.write("ns: " + result._1.toArray.mkString("(", ",", ")"))
    w.newLine()
    w.write("Fd: " + result._2.map(x => x.toArray.mkString("(", ",", ")")).mkString(","))
    w.newLine()
    w.write("Ns: " + result._3.map(x => x.toArray.mkString("(",",",")")).mkString(","))
    w.newLine()
    w.write("lgl: " + result._4)
    w.newLine()
    w.write("length: " + result._5)
    w.close()
  }

  protected def GDprinter(result:(VD, MD, List[Double], Double),w:PrintWriter): Unit = {
    w.println("pi: " + result._1.toArray.mkString("(", "\t", ")"))
    w.println("b: " + result._2.toArray.mkString("(", "\t", ")"))
    w.println("branch: " + result._3.mkString("(", "\t", ")"))
    w.println("lgl: " + result._4)
  }
}

trait SuffStatParser extends JavaTokenParsers {
  def doubleArray: Parser[Array[Double]] = repsep(floatingPointNumber,",") ^^ { case xs => xs.map(_.toDouble).toArray }

  def densevector: Parser[DenseVector[Double]] = "(" ~> doubleArray <~ ")" ^^ { case xs => DenseVector(xs) }

  def Ns: Parser[List[DenseVector[Double]]] = "[Nn]s:".r ~> repsep(densevector,",")

  def Fd: Parser[List[DenseMatrix[Double]]] = "Fd:" ~> repsep(densematrix(4, 4),",")

  def densematrix(n: Int, m: Int): Parser[DenseMatrix[Double]] = "(" ~> doubleArray <~ ")" ^^ { case xs => new DenseMatrix[Double](n, m, xs) }

  def lgl: Parser[Double] = "lgl:" ~> floatingPointNumber ^^ { x => x.toDouble }

  def length: Parser[Long] = "length:" ~> wholeNumber ^^ { x => x.toLong }

  def suffstat: Parser[(VD, List[MD], List[VD], Double, Long)] =
    Ns ~ Fd ~ Ns ~ lgl ~ length ^^ { case x ~ y ~ z ~ w ~ v => (x.head, y, z, w, v) }

  def pi: Parser[DenseVector[Double]] = "pi:" ~> densevector

  def b: Parser[DenseMatrix[Double]] = "b:" ~> densematrix(4, 4)

}
