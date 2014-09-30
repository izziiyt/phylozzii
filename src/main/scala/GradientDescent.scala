object GradientDescent {
  def apply(loop:Int,delta:Double,nhFile:String,alignments:List[List[DNA]]){
    var pt = new PhylogencyTree(nhFile,GTR())
    val an:Double = alignments.length
    for(i <- 0 until loop){
      val tmp = alignments.map(eStepLike(pt,_))
      val param = tmp.map(_._1).reduceLeft(_ :* _)
      val t = tmp.map(_._2).reduceLeft(acc)
      //val newParam = pt.model.param + param * delta//?? priority order of functions ??
      //pt = new PhylogencyTree(pt,newParam)
      //pt.setBranch()
    }
  }

  def acc(a:List[Double],b:List[Double]) = (a,b).zipped.map(_ * _)

  def eStepLike(pt:PhylogencyTree,column:List[DNA]):(Parameters,List[Double]) = {
    //pt.root.setAlignment(column)
    pt.inside(pt.root)
    pt.outside(pt.root)
    val likelihood = pt.root.likelihood(pt.model)
    pt.root.setPosterior(likelihood,pt.model)
    pt.deriveLogLikelihood(pt.root)
  }
}
