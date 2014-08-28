import breeze.linalg.{DenseVector,DenseMatrix}
import math.exp
/**
 * Created by yuto on 14/08/18.
 */
sealed trait Tree{
  def cont:Content
  def inside:Seq[Double]
  def outside(x:Seq[Double],y:Seq[Double]):Unit
  def calcPosterior(prob:Double):Unit
  def setAlignment(x:Array[Int]):Array[Int]
  def setBranch(x:Seq[Double]):Seq[Double]
  def count(x:Double):(List[DenseVector[Double]],List[DenseMatrix[Double]],List[Double],DenseVector[Double])
  def outside():Unit
  def treeProb = cont.treeProb

  //vector of Fd(i,C,theta)
  def FdVec(lambda:DenseVector[Double],u:DenseMatrix[Double],ut:DenseMatrix[Double]) = {
    def Fds(i:Int) = for(x <- 0 until 4;y<- 0 until 4) yield divExpMatrix(x,y,i,i,lambda,u,ut) * cont.postProb(i)(i)
    val tmp = (0 until 4) map (i => Fds(i).sum)
    new DenseVector(tmp.toArray)
  }

  //Matrix of Ns(i,j,C,theta) i -> j
  def NsMat(r:DenseMatrix[Double],lambda:DenseVector[Double],u:DenseMatrix[Double],ut:DenseMatrix[Double]) = {
    def Nss(i:Int,j:Int) = for(x <- 0 until 4;y <- 0 until 4) yield divExpMatrix(x,y,i,j,lambda,u,ut) * cont.postProb(i)(j)
    val tmp = for(i <- 0 until 4;j <- 0 until 4) yield Nss(i,j).sum * cont.t * r(i,j)
    new DenseMatrix(4,4,tmp.toArray)
  }

  //beg -> end and from -> to
  def divExpMatrix(beg:Int,end:Int,to:Int,from:Int,lambda:DenseVector[Double],u:DenseMatrix[Double],ut:DenseMatrix[Double]) = {
    def k(x:Double,y:Double) = if(x == y) exp(x) else (exp(x) - exp(y)) / (x - y)
    val tmp = for(x <- 0 until 4; y <- 0 until 4) yield u(end,x) * ut(x,to) * u(from,y) * ut(y,beg) * k(lambda(x),lambda(y))
    tmp.sum
  }

  def collectF:List[DenseVector[Double]]

  def collectN:List[DenseMatrix[Double]]

  def collectT:List[Double]

  def collectn(x:Double):DenseVector[Double]

}

case class Node(left:Tree,right:Tree,cont:Content) extends Tree{
  def inside:Seq[Double] = {
      val fromLeft = left.inside
      val fromRight = right.inside
      for(i <- 0 until 4){cont.alpha(i) = fromLeft(i) * fromRight(i)}
      cont.accumInsideBelief
  }

  def count(likelihood:Double) = (collectF,collectN,collectT,collectn(likelihood))

  def collectn(likelihood:Double) = new DenseVector[Double]((cont.alpha,GTR.pi).zipped.map(_ * _ / likelihood))

  def setAlignment(x:Array[Int]) = {
    val y = left.setAlignment(x)
    right.setAlignment(y)
  }

  def setBranch(x:Seq[Double]) = {
    val y = left.setBranch(x)
    val z = right.setBranch(y)
    cont.t = z.head
    z.tail
  }

  def collectF = left.collectF ::: right.collectF ::: List(FdVec(GTR.lambda,GTR.u,GTR.ui))

  def collectN = left.collectN ::: right.collectN ::: List(NsMat(GTR.R,GTR.lambda,GTR.u,GTR.ui))

  def collectT = left.collectT ::: right.collectT ::: List(cont.t)

  def calcProbOfTree = (cont.alpha,GTR.pi).zipped.foldLeft(0.0){case (x,(a,p)) => x + a * p}

  def calcPosterior(prob:Double){
    cont.refactPostProbability(prob)
    right.calcPosterior(prob)
    left.calcPosterior(prob)
  }

  def outside(){
    for(i <- 0 until 4) cont.beta(i) = GTR.pi(i)
    innerOutside()
  }

  def outside(fromBro:Seq[Double],fromPar:Seq[Double]){
    for(i <- 0 until 4) cont.beta(i) = fromBro(i) * fromPar(i)
    innerOutside()
  }

  def innerOutside(){
    val fromLeft = left.cont.accumInsideBelief
    val fromRight = right.cont.accumInsideBelief
    val fromThis = cont.accumOutsideBelief
    right.outside(fromLeft,fromThis)
    left.outside(fromRight,fromThis)
  }
}

case class Leaf(species:String,cont:Content,private var nuc:Int = 4) extends Tree{

  def inside:Seq[Double] = {
      if(nuc >= 4) for(i <- 0 until 4) cont.alpha(i) = 1.0
      else cont.alpha(nuc) = 1.0
      cont.accumInsideBelief
  }

  def setBranch(x:Seq[Double]) = {
    cont.t = x.head
    x.tail
  }

  def collectn(x:Double) = null

  def count(x:Double) = null

  def setAlignment(x:Array[Int]) = {
    nuc = x.head
    x.tail
  }

  def outside(){}

  def outside(fromBro:Seq[Double],fromPar:Seq[Double]){
    for(i <- 0 until 4) cont.beta(i) = fromBro(i) * fromPar(i)
  }

  def calcPosterior(prob:Double){
    cont.refactPostProbability(prob)
  }

  def collectF = List(FdVec(GTR.lambda,GTR.u,GTR.ui))

  def collectN = List(NsMat(GTR.R,GTR.lambda,GTR.u,GTR.ui))

  def collectT = List(cont.t)
}

object Tree{
  def apply(query:String):Tree = {
    val stack = new scala.collection.mutable.Stack[Tree]
    val contents = decode(query)
    for(c <- contents){
      if(c == ")"){
        val a = stack.pop()
        val b = stack.pop()
        stack.push(makeTree(a,b))
      }
      else if(c != "("){
        stack.push(makeTree(c))
      }
    }
    val root = stack.pop()
    root.cont.t = 0.0
    root
  }

  private def makeTree(name:String):Leaf = new Leaf(name,new Content,4)

  private def makeTree(n1:Tree,n2:Tree):Node = new Node(n2,n1,new Content)

  def decode(query:String):Array[String] = {
    val array = new scala.collection.mutable.ArrayBuffer[String]
    val buf = new StringBuilder
    for(c <- query){
      c match{
        case '(' => array += "("
        case ')' => array += ")"
        case ':' => if(buf.nonEmpty) array += buf.toString; buf.clear()
        case x if x >= 'A' && x <= 'z' => buf += c
        case _ =>
      }
    }
    array.toArray
  }

}