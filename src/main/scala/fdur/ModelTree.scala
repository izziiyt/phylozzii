package fdur

import java.io.{File, FileReader}
import scala.annotation.tailrec
import scala.util.parsing.combinator.JavaTokenParsers

object ModelTree extends NHParser{

  def fromFile(nhFile:String):ModelRoot = fromFile(new File(nhFile))

  def fromFile(nhFile:File):ModelRoot = {
    val reader = new FileReader(nhFile)
    parseAll(tree,reader).get
  }
  def fromString(nhString:String):ModelRoot = {
    parseAll(tree,nhString).get
  }
}

trait ModelParent extends PrimitiveParent {
  override def children: List[ModelChild]
  def mappedMCL[T](f: ModelChild => List[T]): List[T] = children.foldLeft(Nil:List[T]){(n,x) => f(x) ::: n}
  def leafList = mappedMCL(_.leafList)
  def anclen(target: String):Double = {
    val tmp = children.foldLeft(0.0){(n,x) => n + x.anclen(target)}
    if(tmp == 0.0) 0.0 else tmp + t
  }
}

trait ModelChild extends PrimitiveChild {
  def anclen(target: String):Double
  def leafList: List[ModelLeaf]
  def init: ModelChild
  def changeBranches(branches:List[Double]): (ModelChild, List[Double])
  def changeNames(names:List[String]): (ModelChild, List[String])
}

case class ModelRoot(children:List[ModelChild]) extends ModelParent with PrimitiveRoot{
  override def leafList: List[ModelLeaf] = super.leafList.reverse
  def changeNames(names: List[String]): ModelRoot = {
    @tailrec
    def innerChangeNames(ch: List[ModelChild], ns: List[String], result: List[ModelChild]):
    (List[ModelChild], List[String]) = {
      if (ch == Nil) (result, ns)
      else {
        val (newch, newbr) = ch.head.changeNames(ns)
        innerChangeNames(ch.tail, newbr, newch :: result)
      }
    }
    val (newch, newbr) = innerChangeNames(children, names, Nil)
    if (newbr.nonEmpty) sys.error("BAD branches")
    ModelRoot(newch.reverse)
  }

  def changeBranches(branches: List[Double]):ModelRoot = {
    @tailrec
    def innerChangeBranches(ch: List[ModelChild], br: List[Double], result: List[ModelChild]):
    (List[ModelChild], List[Double]) = {
      if (ch == Nil) (result, br)
      else {
        val (newch,newbr) = ch.head.changeBranches(br)
        innerChangeBranches(ch.tail, newbr, newch :: result)
      }
    }
    val (newch,newbr) = innerChangeBranches(children, branches, Nil)
    if (newbr.nonEmpty) sys.error("BAD branches")
    ModelRoot(newch.reverse)
  }

  def simpleForm: ModelRoot = {
    val n = children.length
    require(n > 1)
    if(n > 2) this
    else{
      val fst = children.head
      val snd = children.last
      (fst, snd) match {
        case (ModelNode(fstch, fstt), ModelNode(sndch, sndt)) =>
          ModelRoot(List(ModelNode(fstch, fstt + sndt)) ::: sndch)
        case (ModelNode(fstch, fstt), ModelLeaf(name, sndt)) =>
          ModelRoot(fstch ::: List(ModelLeaf(name, fstt + sndt)))
        case (ModelLeaf(name, fstt), ModelNode(sndch, sndt)) =>
          ModelRoot(List(ModelLeaf(name, fstt + sndt)) ::: sndch)
        case (ModelLeaf(_, _), ModelLeaf(_, _)) =>
          this
      }
    }
  }

  def init: ModelRoot = {
    require(children.length > 1)
    val x = children.last
    x match {
      case ModelLeaf(_,_) =>
        if(children.length == 2) children.head match {
          case ModelLeaf(_, _) =>
            sys.error("SingleLeafTreeError")
            null
          case ModelNode(ch, _) =>
            ModelRoot(ch)
        }
        else ModelRoot(children.init)
      case _ => ModelRoot(children.init :+ x.init)
    }
  }
}

case class ModelNode(children:List[ModelChild],t:Double) extends ModelChild with ModelParent with PrimitiveNode {

  def changeBranches(branches: List[Double]):(ModelNode,List[Double]) = {
    @tailrec
    def innerChangeBranches(ch: List[ModelChild], br: List[Double], result: List[ModelChild]):
    (List[ModelChild], List[Double]) = {
      if (ch == Nil) (result, br)
      else {
        val (newch,newbr) = ch.head.changeBranches(br)
        innerChangeBranches(ch.tail, newbr, newch :: result)
      }
    }
    val (newch,newbr) = innerChangeBranches(children, branches, Nil)
    (ModelNode(newch.reverse, newbr.head), newbr.tail)
  }

  def changeNames(branches: List[String]):(ModelNode,List[String]) = {
    @tailrec
    def innerChangeNames(ch: List[ModelChild], ns: List[String], result: List[ModelChild]):
    (List[ModelChild], List[String]) = {
      if (ch == Nil) (result, ns)
      else {
        val (newch,newns) = ch.head.changeNames(ns)
        innerChangeNames(ch.tail, newns, newch :: result)
      }
    }
    val (newch,newns) = innerChangeNames(children, branches, Nil)
    (ModelNode(newch.reverse, t), newns)
  }

  def init: ModelChild = {
    val n = children.length
    val x = children.last
    require(n > 1)
    x match {
      case ModelLeaf(_,_) =>
        if(n == 2) {
          val y = children.head
          y match {
            case ModelLeaf(name, tx) => ModelLeaf(name, tx + this.t)
            case ModelNode(ch, tx) => ModelNode(ch, tx + this.t)
          }
        }
        else ModelNode(children.init, this.t)
      case _ => ModelNode(children.init :+ x.init, this.t)
    }
  }
}

case class ModelLeaf(name:String,t:Double) extends ModelChild with PrimitiveLeaf {
  def anclen(target: String):Double = if(name == target) t else 0.0
  def leafList: List[ModelLeaf] = this :: Nil
  def changeBranches(branches:List[Double]): (ModelLeaf, List[Double]) = (ModelLeaf(name,branches.head),branches.tail)
  def changeNames(names:List[String]):(ModelLeaf, List[String]) = (ModelLeaf(names.head,t), names.tail)

  def init = {
    sys.error("LeafInitCalledError")
    null
  }
}

trait NHParser extends JavaTokenParsers {
  def nodeList:Parser[List[ModelChild]] = "("~>repsep(node,",")<~")"  ^^ {case pclist => pclist}

  def tree:Parser[ModelRoot] =  nodeList<~";"  ^^ {case nl => ModelRoot(nl)}

  def node:Parser[ModelChild] = nodeList~":"~value ^^
    {case nl~":"~value => ModelNode(nl,value.toDouble)} | leaf

  def leaf:Parser[ModelLeaf] = name~":"~value ^^
    {case name~":"~value => ModelLeaf(name,value.toDouble)}

  def value:Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  def name:Parser[String] = "[a-zA-Z0-9_.'-]+".r
}
