package fdur

trait PrimitiveTree{
  def t:Double
  def length:Int
  def leafLength:Int
  def branches:List[Double]
  def names: List[String]
  def ancestory(target:String): List[Double]
}

trait PrimitiveChild extends PrimitiveTree

trait PrimitiveParent extends PrimitiveTree {
  def children: List[PrimitiveChild]
  def length: Int = children.foldLeft(0)((n, c) => n + c.length) + 1
  def leafLength: Int = children.foldLeft(0)((n, c) => n + c.leafLength)
  override def toString = children.foldLeft("(")((n, x) => n + x.toString).init + ")"
  def branches: List[Double] = mappedChildList[Double](_.branches)
  def mappedChildList[T](f: PrimitiveChild => List[T]): List[T] = children.foldLeft(Nil: List[T]) { (n, x) => f(x) ::: n }
  def names = mappedChildList[String](_.names)
  def ancestory(target: String): List[Double] = mappedChildList(_ ancestory target)
}

trait PrimitiveRoot extends PrimitiveParent {
  val t = 0.0
  override def branches:List[Double] = super.branches.reverse
  override def toString = super.toString + ";"
  override def names = super.names.reverse
  override def ancestory(target:String) = super.ancestory(target).reverse
}

trait PrimitiveNode extends PrimitiveParent with PrimitiveChild {
  override def toString = super.toString + ":" + t + ","
  override def branches:List[Double] = t :: super.branches
  override def ancestory(target:String) = {
    val tmp = super.ancestory(target)
    if(tmp.isEmpty) Nil
    else t :: tmp
  }
}

trait PrimitiveLeaf extends PrimitiveChild {
  val length  = 1
  def name: String
  def leafLength = length
  override def toString = name + ":" + t + ","
  def branches:List[Double] = t :: Nil
  def names = name :: Nil
  def ancestory(target:String) = if(name == target) t::Nil else Nil
}
