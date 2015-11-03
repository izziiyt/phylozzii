package main

import alignment.Base
import fdur._
import fdur.Maf._
import scala.collection.mutable.ListBuffer

object BLSer {
  def main(args:Array[String]):Unit = {
    val its = MafUnitIterator.fromMSA(args(0))
    val hg19s = new ListBuffer[MafLine]()
    val tree = ModelTree.fromFile(args(1))
    val model = Model(Parameters.fromFile(args(2)))
    val xs = its.map{
      it =>
        hg19s += it.lines.find(_.name == "hg19").get
        eea.tree.LDTree.bls(tree,model,mkCol(it,tree.names),"hg19")
    }
    (xs.toList, hg19s).zipped.foreach{
      (bls, hg19) =>
        println("fixedStep\tchrom=" + hg19.subname + "\tstart=" + hg19.start + "\tstep=1")
        bls.foreach(println)
        println()
    }
  }
  def mkCol(mu:MafUnit, names:List[String]): List[Array[Base]] = {
    val n = mu.length
    names.map{
      name => val tmp = mu.lines.find(_.name == name)
        if(tmp.isDefined) tmp.get.seq else Array.fill[Base](n)(Base.N)
    }
  }
}
