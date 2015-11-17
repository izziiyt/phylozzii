package main

import java.io.{FileWriter, BufferedWriter}

import alignment.Base
import fdur._
import fdur.Maf._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object BLSer {
  def main(args:Array[String]):Unit = {
    val its = MafUnitIterator.fromMSA(args(0))
    val model = Model(Parameters.fromFile(args(2)))
    val target = args(3)
    val out = new BufferedWriter(new FileWriter(args(4)))
    val tree = {
      val tmp = ModelTree.fromFile(args(1))
      if (args.length == 7)
        tmp.changeNames(ModelTree.fromFile(args(6)).names)
      else
        tmp
    }
    if(!tree.names.contains(target))
      sys.error("newick formatted tree doesn't contain " + target + ".")
    try{
      if(args(5) == "-bls") blsexe(its, tree, model, target, out)
      else if(args(5) == "-blsa") blsaexe(its,tree,model,target,out)
      else sys.error("Option -bls or -blsa is needed.")
    }
    catch{case e: Throwable => sys.error(e.toString)}
    finally{out.close()}
  }

  def mkCol(mu:MafUnit, names:List[String],target: String): List[Array[Base]] = {
    def getWithIndices(xs:Array[Base], indices:Array[Int]): Array[Base] = indices.map(xs(_))

    val targetX = mu.lines.find(_.name == target).get
    if(targetX.strand == "-") sys.error("error -")
    val targetSeq = targetX.seq
    val indices = targetSeq.zipWithIndex.withFilter{case (b,_) => !b.isN}.map(_._2)
    val n = indices.length
    if(n == 0) Nil
    else names.map{
      name =>
        val tmp = mu.lines.find(_.name == name)
        if(tmp.isDefined) getWithIndices(tmp.get.seq, indices) else Array.fill[Base](n)(Base.N)
    }
  }

  def blsexe(its: MafUnitIterator, tree: ModelRoot, model: Model, target: String, out: BufferedWriter): Unit = {
    val reg = tree.branches.sum
    out.write("## Branch Length Score in the tree. Target species is " + target + ".")
    out.newLine()
    its.foreach{
      it =>
        val cols = dev(mkCol(it, tree.names, target), 10000)
        if(cols.nonEmpty) {
          val hg19 = it.lines.find(_.name == target).get
          val bls = cols.flatMap(col => eea.tree.LDTree.bls(tree, model, col, target))
          out.write("fixedStep chrom=" + hg19.subname + " start=" + hg19.start + " step=1")
          out.newLine()
          bls.foreach { b => out.write((b / reg).toString); out.newLine() }
          out.newLine()
        }
    }
  }

  protected def dev[T](xs:List[Array[T]],n: Int): Array[List[Array[T]]] = {
    if(xs == Nil) Array[List[Array[T]]]()
    @tailrec
    def f(current: List[Array[T]], result: ArrayBuffer[List[Array[T]]] = new ArrayBuffer[List[Array[T]]]()):
    ArrayBuffer[List[Array[T]]] = {
      if(current.head.length < n) result :+ current
      else{
        val (x, y) = current.map(_.splitAt(n)).unzip
        f(y, result :+ x)
      }
    }
    f(xs).toArray
  }

  protected var i = 0

  def blsaexe(its: MafUnitIterator, tree: ModelRoot, model: Model, target: String, out: BufferedWriter): Unit = {
    val reg = tree.branches.sum
    out.write("## Branch Length Score in the ancestory of " + target + ".")
    out.newLine()
    its.foreach {
      it =>
        val cols = dev(mkCol(it, tree.names, target), 10000)
        if (cols.nonEmpty) {
          val hg19 = it.lines.find(_.name == target).get
          val blsa = cols.flatMap{col => eea.tree.LDTree.blsa(tree, model, col, target)}
          out.write("fixedStep chrom=" + hg19.subname + " start=" + hg19.start + " step=1")
          out.newLine()
          blsa.foreach { b => out.write((b / reg).toString); out.newLine() }
          out.newLine()
        }
    }
  }
}
