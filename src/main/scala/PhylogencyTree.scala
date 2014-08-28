import breeze.linalg.{DenseMatrix,DenseVector,sum,trace}
import math.{abs,exp,pow}
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source
/**
 * Created by yuto on 14/08/21.
 */
class PhylogencyTree(query:String){
  val root = Tree(query)
  private var likelihood = 0.0 //P(C|theta)
  def ll = likelihood
  def setBranch(x:Array[Double]){root.setBranch(x)}

  def eStep(a:Array[Int]) = {
    root.setAlignment(a)
    root.inside
    root.outside()
    likelihood = root.treeProb
    println(likelihood)
    root.calcPosterior(likelihood)
    root.count(likelihood)
  }
}

/**object readMaf{
  def apply(mafFile:String):Seq[Array[Int]] = {
    val source = Source.fromFile(mafFile)
    val buf = new ListBuffer[(String,Int)]
    for(line <- source.getLines()){
      line match{
        case "" =>
          if(buf.length == 7) fillCount += 1
          buf.clear()
        case x if x startsWith "s" => buf += parseS(line)
        case _ =>
      }
    }
  }
}**/