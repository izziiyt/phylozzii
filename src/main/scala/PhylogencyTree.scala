import breeze.linalg.{DenseMatrix,DenseVector,sum,trace,diag}


class PhylogencyTree(val root:Node,val model:EvolutionModel){

  def this(nhFormatQuery:String,m:EvolutionModel) = this(Tree(nhFormatQuery),m)

  def this(that:PhylogencyTree,m:EvolutionModel) = this(that.root.format(),m)

  def setBranch(x:List[Double]){root.setBranch(x)}


  def deriveLogLikelihood(tree:Tree):(Parameters,List[Double]) = {
    val rs = for(i <- 0 to 3;j <- 0 to 3) yield deriveLihdWithLogR(i,j,tree.cont)
    val fbs = for(i <- 0 to 3;j <- 0 to 3) yield tree.cont.alpha(j) * tree.cont.beta(i)
    val ps = (rs,fbs).zipped.map((r,fb) => deriveLihdWithPi(tree.cont,r) * fb).reduceLeft(_ + _)
    val bs = (rs,fbs).zipped.map((r,fb) => deriveLihdWithB(tree.cont,r) * fb).reduceLeft(_ + _)
    val ts = (rs,fbs).zipped.map((r,fb) => deriveLihdWithT(tree.cont,r) * fb)

    tree match{
      case Node(left,right,_) =>
        val fromRight:(Parameters,List[Double]) = deriveLogLikelihood(right)
        val fromLeft:(Parameters,List[Double]) = deriveLogLikelihood(left)
        val param = fromLeft._1 + fromRight._1 + Parameters(bs,ps)
        val tlist:List[Double] = fromLeft._2 ::: fromRight._2 ::: List(ts.sum)
        (param,tlist)
      case Leaf(_,_) =>
        (Parameters(bs,ps),List(ts.sum))
    }
  }

  def deriveLihdWithLogR(a:Int,b:Int,cont:Content):DenseMatrix[Double] = {
    cont.NsMati(a,b,model) - diag(cont.FdVeci(a,b,model)) * model.R * cont.t
  }

  def deriveLihdWithPi(cont:Content,r:DenseMatrix[Double]):DenseVector[Double] = {
    DenseVector((0 to 3).map(i => sum(r(::,i)) / model.pi(i)).toArray)
  }

  def deriveLihdWithB(cont:Content,r:DenseMatrix[Double]):DenseVector[Double] = {
    val tmp = (r + r.t) :/ model.B
    DenseVector((for(i <- 1 to 3;j <- i to 3) yield tmp(i,j)).toArray)
  }

  //No problem.
  def deriveLihdWithT(cont:Content,r:DenseMatrix[Double]):Double = (sum(r) - trace(r)) / cont.t

  def inside(tree:Tree):DenseVector[Double] = {
    tree match{
      case Node(left,right,cont) =>
        val fromLeft = inside(left)
        val fromRight = inside(right)
        for(i <- 0 to 3){cont.alpha(i) = fromLeft(i) * fromRight(i)}
        cont.accumInsideBelief(model)
      case Leaf(_,cont) =>
        if(!cont.nuc.isNuc) cont.alpha(0 to 3) := 1.0
        else cont.alpha(cont.nuc.toInt) = 1.0
        cont.accumInsideBelief(model)
    }
  }

  def outside(tree:Tree,fromBro:DenseVector[Double] = model.pi,fromPar:DenseVector[Double] = DenseVector(1,1,1,1)){
    tree match{
      case Node(left,right,cont) =>
        for(i <- 0 to 3) cont.beta(i) = fromBro(i) * fromPar(i)
        innerOutside(left,right,cont)
      case Leaf(_,cont) =>
        for(i <- 0 to 3){cont.beta(i) = fromBro(i) * fromPar(i)}
    }
  }

  def innerOutside(left:Tree,right:Tree,cont:Content){
    val fromLeft = left.cont.accumInsideBelief(model)
    val fromRight = right.cont.accumInsideBelief(model)
    val fromThis = cont.accumOutsideBelief(model)
    outside(right,fromLeft,fromThis)
    outside(left,fromRight,fromThis)
  }


}