package fdur2

import alignment.Base

object EM {
  type Column = Array[Base]

  def estep(pr:ModelRoot,cols:List[Column],model:Model): (VD,List[MD],List[VD],Double,Int) = Tree.suffStat(pr,model,cols)

  def mstep(suffs: Array[(VD, List[MD], List[VD], Double, Int)],model:Model, branches:List[Double]):
  (Double, List[Double], Parameters) = {
    val (rootns, ns, fd, lgl, n):(VD, List[MD], List[VD], Double, Int) = suffs.reduce
    {
      (m, x) =>
        val ns = m._1 + x._1
        val Ns = (m._2, x._2).zipped.map(_ + _)
        val Fd = (m._3, x._3).zipped.map(_ + _)
        val l = m._4 + x._4
        val i = m._5 + x._5
        (ns, Ns, Fd, l, i)
    }
    val nd = n.toDouble
    val (br, pr) = model.mstep(rootns / nd, ns.map(_ / nd), fd.map(_ / nd), branches)
    (lgl, br, pr)
  }
}
