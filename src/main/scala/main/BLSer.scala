package main

import java.io._
import java.util.zip.GZIPOutputStream
import alignment.Base
import fdur._
import biformat.{MafIterator,MafUnit}
import scala.annotation.tailrec

/**
  * Main class for caluculating probablistic bls score and it on target ancestory
  */
object BLSer {

  def blser(target: String, maf: File, newick: File, param: File, bls: File, blsa: File, nameFile: File): Unit = {
    val in = biformat.bigSource(maf)
    val its = MafIterator.fromMSA(in, target).merge(10240)
    val model = Model(Parameters.fromFile(param))
    val outbls = new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(bls), 1024 * 1024))
    val outblsa = new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(blsa), 1024 * 1024))
    val tree = {
      val tmp = ModelTree.fromFile(newick)
      if (nameFile.exists())
        tmp.changeNames(ModelTree.fromFile(nameFile).names)
      else
        tmp
    }

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

  protected def blsexe(its: MafIterator, tree: ModelRoot, model: Model, target: String, outbls: Writer, outblsa: Writer): Unit = {
    outblsa.write("## Branch Length Score in the ancestory of " + target + ".\n")
    outbls.write("## Branch Length Score in the tree. Target species is " + target + ".\n")
    its.foreach{
      it =>
        val (preCols, indices) = mkCol(it.Dremoved, tree.names, target)
        val cols = dev(preCols, 1024)
        if(cols.nonEmpty) {
          val hg19 = it.lines(target)
          val (bls, blsa) = cols.foldLeft((Array[Double](),Array[Double]())){
            (n, x) =>
              val (b, ba) = pbls.LDTree.bls(tree, model, x, target)
              (n._1 ++ b, n._2 ++ ba)
          }
          f(bls, indices.map(_+ it.start), hg19.subname, outbls)
          f(blsa, indices.map(_+ it.start), hg19.subname, outblsa)
        }
    }
    def f(bls: Array[Double], indices: Array[Long], chrom: String, w:Writer): Unit ={
      w.write("variableStep\tchrom=" + chrom + "\n")
      (bls, indices).zipped.foreach {(b, i) => w.write(i + "\t" + b.toString + "\n")}
      w.write("\n")
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
