package main

import java.io._
import java.util.zip.GZIPOutputStream
import alignment.Base
import fdur._
import biformat.MafIterator
import biformat.MafIterator.MafUnit
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object BLSer {

  def blser(target: String, maf: File, newick: File, param: File, bls: File, blsa: File, nameFile: File): Unit = {
    val in = biformat.bigSource(maf)
    val its = MafIterator.fromSource(in, target).merged(10240)
    val model = Model(Parameters.fromFile(param))
    val outbls = new PrintWriter(new GZIPOutputStream(new FileOutputStream(bls), 1024 * 1024))
    val outblsa = new PrintWriter(new GZIPOutputStream(new FileOutputStream(blsa), 1024 * 1024))
    val tree =
      if (nameFile.isFile) ModelTree.fromFile(newick).changeNames(ModelTree.fromFile(nameFile).names)
      else ModelTree.fromFile(newick)
    try{
      if(!tree.names.contains(target))
        throw new UnsupportedOperationException("newick formatted tree doesn't contain " + target + ".")
      blsexe(its,tree,model,target,outbls,outblsa)
    }
    catch{
      case e: Throwable => e.printStackTrace()
    }
    finally{
      in.close()
      outbls.close()
      outblsa.close()
    }
  }

  protected def mkCol(mu:MafUnit, names:List[String],target: String): (List[Array[Base]], Array[Int]) = {
    def f(xs:Array[Base], indices:Array[Int]): Array[Base] = indices.map(xs(_))

    val targetX = mu.lines(target)
    if(targetX.strand == "-") throw new UnsupportedOperationException("target strand is -.")
    val indices = targetX.seq.zipWithIndex.withFilter{case (b,_) => !b.nonNuc}.map(_._2)
    val n = indices.length
    val tmp =
      if(n == 0) Nil
      else names.map{
        mu.lines.get(_) match {
          case Some(x) => f(x.seq, indices)
          case None => Array.fill[Base](n)(Base.N)
        }
      }
    (tmp, indices)
  }

  protected def blsexe(its: MafIterator, tree: ModelRoot, model: Model, target: String, outbls: PrintWriter, outblsa: PrintWriter): Unit = {
    outblsa.println("## Branch Length Score in the ancestory of " + target + ".")
    outbls.println("## Branch Length Score in the tree. Target species is " + target + ".")
    its.foreach{
      it =>
        val (preCols, indices) = mkCol(it.Dremoved, tree.names, target)
        val cols = dev(preCols, 1024)
        if(cols.nonEmpty) {
          val hg19 = it.lines(target)
          val (bls, blsa) = cols.foldLeft((ArrayBuffer[Double](),ArrayBuffer[Double]())){
            (n, x) =>
              val (b, ba) = pbls.LDTree.bls(tree, model, x, target)
              (n._1 ++ b, n._2 ++ ba)
          }
          f(bls, indices.map(_+ it.start), hg19.subname, outbls)
          f(blsa, indices.map(_+ it.start), hg19.subname, outblsa)
        }
    }
    def f(bls: ArrayBuffer[Double], indices: Array[Long], chrom: String, w:PrintWriter): Unit ={
      w.println("variableStep\tchrom=" + chrom)
      (bls, indices).zipped.foreach {(b, i) => w.println(i + "\t" + b.toString)}
      w.println()
    }
  }
  /**
    * Devide xs to n-length bins, # of bins is undefinded.
    * */
  protected def dev[T](xs: List[Array[T]], n: Int): Array[List[Array[T]]] = {
    @tailrec
    def f(current: List[Array[T]], result: List[List[Array[T]]] = Nil):
    List[List[Array[T]]] = {
      if(current == Nil || current.head.isEmpty) result.reverse
      else if(current.head.length < n) (current :: result).reverse
      else{
        val (x, y) = current.map(_.splitAt(n)).unzip
        f(y, x :: result)
      }
    }
    f(xs).toArray
  }

}
