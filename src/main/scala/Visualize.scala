import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scalax.chart.api._

object Visualize {
  def paramViz(params:List[Parameters]){
    val pis = for(i <- 0 to 3) yield params.map(_.pi(i))
    val Z = for(i <- 0 until pis(0).length) yield i
    val xs = for(i <- 0 to 3) yield ("(" + i + ")",Z zip pis(i))
    val chart1 = XYLineChart(xs.toXYSeriesCollection(),title = "pi")
    chart1.saveAsPNG("target/pi.png")
    val bs = for(i <- 0 to 5) yield params.map(_.Bvec(i))
    val ys = for(i <- 0 to 5) yield ("(" + i + ")",Z zip bs(i))
    val chart2 = XYLineChart(ys.toXYSeriesCollection(),title = "b")
    chart2.saveAsPNG("target/b.png")
  }

  def paramViz(paramFile:String){
    val source = Source.fromFile(paramFile)
    val lines = source.getLines()
    val cache = new StringBuilder
    val paramList = new ListBuffer[Parameters]
    for(line <- lines){
      if(line == ">" && !cache.isEmpty){
        paramList += Parameters.fromString(cache.toString())
        cache.clear()
      }
      else if(line != ">"){
        cache ++= line
      }
    }
    source.close()
    paramViz(paramList.toList)
  }

  def branchViz(branches:List[List[Double]]){
    val bl = branches(0).length
    val bs = for(i <- 0 until bl) yield branches.map(_(i))
    val Z = for(i <- 0 until branches.length) yield i
    val xs =  for(i <- 0 until bs.length) yield (i,Z zip bs(i))
    val chart = XYLineChart(xs.toXYSeriesCollection(),title = "branch")
    chart.saveAsPNG("target/branches.png")
  }

  def branchViz(treeFile:String){
    val source = Source.fromFile(treeFile)
    val lines = source.getLines()
    val cache = new StringBuilder
    val treeList = new ListBuffer[List[Double]]
    for(line <- lines){
      if(line == ">" && !cache.isEmpty){
        val tree = Tree.fromString(cache.toString())
        treeList += tree.branches
        cache.clear()
      }
      else if(line != ">"){
        cache ++= line
      }
    }
    source.close()
    branchViz(treeList.toList)
  }

  def llViz(llFile:String){
    val source = Source.fromFile(llFile)
    val lines = source.getLines()
    val ll = new ArrayBuffer[Double]
    for(line <- lines;if line != ">"){
        ll += line.toDouble
    }
    source.close()
    val xs = (0 until ll.length) zip ll
    val chart = XYLineChart(xs.toXYSeriesCollection(),title = "logLikelihood")
    chart.saveAsPNG("target/logLikelihood.png")
  }

  def llViz(ll:List[Double]){
    val xs = (0 until ll.length) zip ll
    val chart = XYLineChart(xs.toXYSeriesCollection(),title = "logLikelihood")
    chart.saveAsPNG("target/logLikelihood.png")
  }
}
