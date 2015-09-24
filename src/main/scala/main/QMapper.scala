package main

import java.io.{File, FileReader, FileWriter, PrintWriter}
import breeze.linalg.{DenseMatrix, DenseVector}
import fdur._
import scala.util.parsing.combinator.JavaTokenParsers

object QMapper {
  def main(args:Array[String]): Unit = {
    //${al} ${nh} ${count} target/time/e/${SGE_TASK_ID}.time
    val cols = Maf.readMaf(args(1), 1000).toParArray
    val tree = ModelTree.fromFile(args(3))
    val out = new PrintWriter(args(4))

    try {
      args(0) match {
        case "em" =>
          val param = Parameters.fromFile(args(2))
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
        case "gd" =>
          val param = Parameters.readAsGD(args(2))
          val model = Model(Parameters(DenseVector.vertcat(param,DenseVector(tree.branches.toArray))))
          val results = cols.map{c => Optimizer.ldgradMap(tree, c, model)}
          val summed = results.reduce {
            (x, y) =>
              val n1 = x._1 + y._1
              val n2 = x._2 + y._2
              val n3 = (x._3, y._3).zipped.map(_ + _)
              val n4 = x._4 + y._4
              (n1, n2, n3, n4)
          }
          GDprinter(summed, out)
      }
    }finally {
      out.close()
    }

  }

  protected def EMprinter(result:(VD,List[MD],List[VD],Double,Long),w:PrintWriter): Unit = {
    w.println("ns: " + result._1.toArray.mkString("(", "\t", ")"))
    w.print("Fd: ")
    result._2.foreach(x => w.println(x.toArray.mkString("(", "\t", ")")))
    w.print("Ns: ")
    result._3.foreach(x => w.println(x.toArray.mkString("(", "\t", ")")))
    w.println("lgl: " + result._4)
    w.println("length: " + result._5)
  }

  protected def GDprinter(result:(VD, MD, List[Double], Double),w:PrintWriter): Unit = {
    w.println("pi: " + result._1.toArray.mkString("(", "\t", ")"))
    w.println("b: " + result._2.toArray.mkString("(", "\t", ")"))
    w.println("branch: " + result._3.mkString("(", "\t", ")"))
    w.println("lgl: " + result._4)
  }
}

object QReducer extends SuffStatParser{
  def main(args:Array[String]): Unit = {
    val files = new java.io.File(args(1)).listFiles.filter(_.getName.endsWith(".txt"))
    val suffs = files.map(EMreader).toParArray
    val param = Parameters.fromFile(args(2))
    val tree = ModelTree.fromFile(args(3))
    val (lgl, brnch, newparam) = Optimizer.mstep(suffs,Model(param), tree.branches)
    write(newparam.toString,args(2),false)
    write(tree.changeBranches(brnch).toString,args(3),false)
    write(lgl.toString,args(4),true)
  }

  protected def write(x:String,f:String,bool:Boolean): Unit = {
    val y = new PrintWriter(new FileWriter(f,bool))
    y.println(x)
    y.close()
  }

  protected def EMreader(x:File): (VD, List[MD], List[VD], Double, Long) = {
    val reader = new FileReader(x)
    parseAll(suffstat,reader).get
  }
}

trait SuffStatParser extends JavaTokenParsers {
  def ns: Parser[DenseVector[Double]] = "ns:"~>densevector

  def doubleArray: Parser[Array[Double]] = rep(floatingPointNumber) ^^
    {case xs => xs.map(_.toDouble).toArray}

  def densevector: Parser[DenseVector[Double]] = "("~>doubleArray<~")" ^^
    {case xs => DenseVector(xs)}

  def Ns: Parser[List[DenseVector[Double]]] = "Ns:"~>rep(densevector)

  def Fd: Parser[List[DenseMatrix[Double]]] = "Fd:"~>rep(densematrix(4, 4))

  def densematrix(n:Int, m:Int): Parser[DenseMatrix[Double]] = "("~>doubleArray<~")" ^^
    {case xs => new DenseMatrix[Double](n, m, xs)}

  def lgl: Parser[Double] = "lgl:"~>floatingPointNumber ^^ {x => x.toDouble}

  def length: Parser[Long] = "length:"~>wholeNumber ^^ {x => x.toLong}

  def suffstat: Parser[(VD, List[MD], List[VD], Double, Long)] =
    ns~Fd~Ns~lgl~length ^^ {case x~y~z~w~v => (x, y, z, w, v)}

  def pi: Parser[DenseVector[Double]] = "pi:"~>densevector

  def b: Parser[DenseMatrix[Double]] = "b:"~>densematrix(4, 4)

  def branch: Parser[List[Double]] = "branch: ("~>rep(floatingPointNumber)<~")" ^^
    {case xs => xs.map(_.toDouble)}

  def gradient: Parser[(VD, MD, List[Double], Double)] =
    pi~b~branch~lgl ^^ {case x~y~z~w => (x, y, z, w)}
}