package phylozzii.branco

import java.io._

import alignment.Base
import biformat.MafIterator
import biformat.MafIterator.MafUnit
import phylozzii.fdur._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Branco {

  /*def blser(target: String, maf: Source, newick: File, param: File, bls: PrintWriter, blsa: PrintWriter): Unit = {
    //blsaval in = biformat.bigSource(maf)

      val its = MafIterator.fromSource(maf, target).merged(10240)
      val model = Model(Parameters.fromFile(param))
      val tree = ModelTree.fromFile(newick)
      if(!tree.names.contains(target))
        throw new UnsupportedOperationException("newick formatted tree doesn't contain " + target + ".")
      else
        exe(its, tree, model, target, bls, blsa)
  }*/
  /**
    *
    * @param mu
    * @param names
    * @param target
    * @return
    */
  protected def mkCol(mu:MafUnit, names:List[String], target: String): (List[Array[Base]], Array[Int]) = {
    def f(xs:Array[Base], indices:Array[Int]): Array[Base] = indices.map(xs(_))

    val targetX = mu.lines(target)
    // target species' strand should be "+" (not inverse strand)
    if(targetX.strand == "-")
      throw new UnsupportedOperationException("target strand is -.")
    val indices = targetX.seq.zipWithIndex.withFilter{case (b, _) => !b.nonNuc}.map(_._2)
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

  def exe(its: MafIterator, tree: ModelRoot, model: Model, target: String, outbls: PrintWriter, outblsa: PrintWriter, scale: Boolean): Unit = {
    outblsa.println("## Branch Length Score in the ancestory of " + target + ".")
    outbls.println("## Branch Length Score in the tree. Target species is " + target + ".")
    val (regbls, regblsa) = if(scale) (tree.branches.sum, tree.anclen(target)) else (1.0, 1.0)
    its.foreach{
      it =>
        // .Dremoved removes column if target nucleotide is deletion
        val (preCols, indices) = mkCol(it.Dremoved, tree.names, target)
        // dev() devide data to avoid heap memory overflow
        val cols = dev(preCols, 1024)
        if(cols.nonEmpty) {
          val _target = it.lines(target)
          val (bls, blsa) = cols.foldLeft((ArrayBuffer[Double](),ArrayBuffer[Double]())){
            (n, x) =>
              val (b, ba) = phylozzii.branco.LDTree.bls(tree, model, x, target)
              (n._1 ++ b, n._2 ++ ba)
          }
          output(bls.map(_ / regbls), indices.map(_+ it.start), _target.subname, outbls)
          output(blsa.map(_ / regblsa), indices.map(_+ it.start), _target.subname, outblsa)
        }
    }
    def output(bls: ArrayBuffer[Double], indices: Array[Int], chrom: String, w: PrintWriter): Unit = {
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
