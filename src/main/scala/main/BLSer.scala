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
  /**
    *
    * @param args
    */
  def main(args:Array[String]):Unit = {
    val target = args(3)
    val in = biformat.bigSource(args(0))
    val its = MafIterator.fromMSA(in, target).merge(10240)
    val model = Model(Parameters.fromFile(args(2)))
    val out = new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(args(4)),1024 * 1024))
    val tree = {
      val tmp = ModelTree.fromFile(args(1))
      if (args.length == 7)
        tmp.changeNames(ModelTree.fromFile(args(6)).names)
      else
        tmp
    }

    try{
      if(!tree.names.contains(target))
        sys.error("newick formatted tree doesn't contain " + target + ".")
      blsexe(its,tree,model,target,out,args(5) == "-blsa")
    }
    catch{case e: Throwable => e.printStackTrace()}
    finally{
      in.close()
      out.close()}
  }

  protected def mkCol(mu:MafUnit, names:List[String],target: String): (List[Array[Base]], Array[Int]) = {
    def f(xs:Array[Base], indices:Array[Int]): Array[Base] = indices.map(xs(_))

    val targetX = mu.lines(target)
    if(targetX.strand == "-") sys.error("Error: target strand is -.")
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
  /**
    *
    * */
  protected def blsexe(its: MafIterator, tree: ModelRoot, model: Model, target: String, out: Writer, blsa: Boolean): Unit = {
    if(blsa) out.write("## Branch Length Score in the ancestory of " + target + ".\n")
    else out.write("## Branch Length Score in the tree. Target species is " + target + ".\n")
    its.foreach{
      it =>
        val (preCols, indices) = mkCol(it.Dremoved, tree.names, target)
        val cols = dev(preCols, 1024)
        if(cols.nonEmpty) {
          val hg19 = it.lines(target)
          val bls =
            if(blsa) cols flatMap (eea.tree.LDTree.blsa(tree, model, _, target))
            else     cols flatMap (eea.tree.LDTree.bls(tree, model, _, target))
          out.write("variableStep\tchrom=" + hg19.subname + "\n")
          (bls, indices).zipped.foreach { (b, i) => out.write((i + it.start) + "\t" + b.toString + "\n")}
          out.write("\n")
        }
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
