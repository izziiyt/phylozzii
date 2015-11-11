package main

import java.io.{FileWriter, BufferedWriter}

import alignment.Base
import fdur._
import fdur.Maf._

object BLSer {
  def main(args:Array[String]):Unit = {
    val its = MafUnitIterator.fromMSA(args(0))
    val model = Model(Parameters.fromFile(args(2)))
    val target = args(3)
    val out = new BufferedWriter(new FileWriter(args(4)))
    val tree = {
      val tmp = ModelTree.fromFile(args(1))
      if (args.length == 6)
        tmp.changeNames(ModelTree.fromFile(args(5)).names)
      else
        tmp
    }
    try{exe(its, tree, model, target, out)}
    catch{case e: Throwable => sys.error(e.toString)}
    finally{out.close()}
  }

  def mkCol(mu:MafUnit, names:List[String],target: String): List[Array[Base]] = {
    def getWithIndices(xs:Array[Base],indices:Array[Int]): Array[Base] =
      xs.zipWithIndex.withFilter{case (_,i) => indices.contains(i)}.map(_._1)

    val targetSeq = mu.lines.find(_.name == target).get.seq
    val indices = targetSeq.zipWithIndex.withFilter{case (b,i) => !b.isN}.map(_._2)
    val n = indices.length

    names.map{
      name =>
        val tmp = mu.lines.find(_.name == name)
        if(tmp.isDefined) getWithIndices(tmp.get.seq, indices) else Array.fill[Base](n)(Base.N)
    }
  }

  /*def exe(its: MafUnitIterator, tree: ModelRoot, model: Model, target: String, out: BufferedWriter): Unit = {
    val hg19s = new ListBuffer[MafLine]()
    val xs = its.map{
      it =>
        hg19s += it.lines.find(_.name == target).get
        eea.tree.LDTree.bls(tree,model,mkCol(it,tree.names,target),target)
    }
    val reg = tree.branches.sum
    out.write("## target species is " + target)
    out.newLine()
    (xs.toList, hg19s).zipped.foreach{
      (bls, hg19) =>
        out.write("fixedStep chrom=" + hg19.subname + " start=" + hg19.start + " step=1")
        out.newLine()
        bls.foreach{b => out.write((b / reg).toString); out.newLine()}
        out.newLine()
    }
  }*/

  def exe(its: MafUnitIterator, tree: ModelRoot, model: Model, target: String, out: BufferedWriter): Unit = {
    val reg = tree.branches.sum
    out.write("## target species is " + target)
    out.newLine()
    its.foreach{
      it =>
        val hg19 = it.lines.find(_.name == target).get
        val bls = eea.tree.LDTree.bls(tree,model,mkCol(it,tree.names,target),target)
        out.write("fixedStep chrom=" + hg19.subname + " start=" + hg19.start + " step=1")
        out.newLine()
        bls.foreach{b => out.write((b / reg).toString); out.newLine()}
        out.newLine()
    }
  }
}
