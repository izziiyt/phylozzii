package main

import biformat.WigIterator
import biformat.BedIterator
/**
  * Created by yuto on 15/12/10.
  */
object BedFilter {
  def main(args: Array[String]) {
    val wigfile = args(0)
    val bedfile = args(1)
    val bedit = BedIterator.fromSource(biformat.bigSource(bedfile)).filter(_.chr == "chr20")
    val wigit = WigIterator.fromFile(wigfile).filterWithBed(bedit)
    val tmp = wigit.hist(1000,1)
    println(tmp.mkString(","))
  }
}
