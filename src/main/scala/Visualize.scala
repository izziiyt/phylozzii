import java.io.File
import scalax.chart.api._

object Visualize {
  def paramViz(params:List[Parameters]){
    val pis = for(i <- 0 to 3) yield params.map(_.pi(i))
    val Z = for(i <- 0 until pis(0).length) yield i
    val xs = for(i <- 0 to 3) yield ("(" + i + ")",Z zip pis(i))
    val chart1 = XYLineChart(xs.toXYSeriesCollection(),title = "pi")
    chart1.saveAsPNG("target/pi.png")
    val bs = for(i <- 0 to 3) yield params.map(_.Bvec(i))
    val ys = for(i <- 0 to 3) yield ("(" + i + ")",Z zip bs(i))
    val chart2 = XYLineChart(ys.toXYSeriesCollection(),title = "b")
    chart2.saveAsPNG("target/b.png")
  }
  def branchViz(branches:List[List[Double]]){
    val bl = branches(0).length
    val bs = for(i <- 0 until bl) yield branches.map(_(i))
    val Z = for(i <- 0 until branches.length) yield i
    val xs =  for(i <- 0 until bs.length) yield (i,Z zip bs(i))
    val chart = XYLineChart(xs.toXYSeriesCollection(),title = "branch")
    chart.saveAsPNG("target/branches.png")
    chart.show()
  }
}
