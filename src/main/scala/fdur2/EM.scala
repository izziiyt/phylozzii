package fdur2

import alignment.Base

object EM {
  type Column = Array[Base]
  def estep(pr:ModelRoot,cols:Array[List[Column]],param:Parameters,branches:Option[List[Double]] = None):
  Array[(VD,List[MD],List[VD],Double,Int)] = {
    val model = Model(param)
    val tr = if(branches.isDefined) pr.changeBranches(branches.get) else pr
    cols.map(Tree.suffStat(tr,model,_))
  }
  def mstep(suffs: Array[(VD, List[MD], List[VD], Double, Int)],model:Model, branches:List[Double]):
  (Double, List[Double], Parameters) = {
    val (rootns, ns, fd, lgl, n):(VD, List[MD], List[VD], Double, Int) = suffs.reduce{
      (n, x) =>
        val ns = n._1 + x._1
        val Ns = (n._2, x._2).zipped.map(_ + _)
        val Fd = (n._3, x._3).zipped.map(_ + _)
        val d = n._4 + x._4
        val i = n._5 + x._5
        (ns, Ns, Fd, d, i)
    }
    val nd = n.toDouble
    val (br, pr) = model.mstep(rootns / nd, ns.map(_ / nd), fd.map(_ / nd), branches)
    (lgl, br, pr)
  }
}
