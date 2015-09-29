package fdur

import java.io.FileReader
import scala.annotation.tailrec
import scala.util.parsing.combinator.JavaTokenParsers

object ModelTree extends NHParser{
  def fromFile(nhFile:String):ModelRoot = {
    val reader = new FileReader(nhFile)
    parseAll(tree,reader).get
  }
  def fromString(nhString:String):ModelRoot = {
    parseAll(tree,nhString).get
  }
}

trait ModelChild extends PrimitiveChild{
  def changeBranches(branches:List[Double]): (ModelChild, List[Double])
  def leafList: List[ModelLeaf]
  def init: ModelChild
}

case class ModelRoot(children:List[ModelChild]) extends PrimitiveRoot {
  def leafList:List[ModelLeaf] = children.foldLeft(Nil:List[ModelLeaf])((n,x) => x.leafList ::: n).reverse
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
    if(newbr.nonEmpty) sys.error("BAD branches")
    ModelRoot(newch.reverse)
  }

  def init: ModelRoot = {
    require(children.length > 1)
    children.last match {
      case ModelLeaf(_,_) => ModelRoot(children.init)
      case _ => ModelRoot(children.init :+ children.last.init)
    }
  }
}

case class ModelNode(children:List[ModelChild],t:Double) extends ModelChild with PrimitiveNode {
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

  def leafList:List[ModelLeaf] = children.foldLeft(Nil:List[ModelLeaf])((n,x) => x.leafList ::: n)

  def init: ModelChild = {
    val n = children.length
    val x = children.last
    require(n > 1)
    x match {
      case ModelLeaf(_,_) =>
        if(n == 2) {
          val ModelLeaf(name, t) = children.head
          ModelLeaf(name,  t + this.t)
        }
        else ModelNode(children.init, this.t)
      case _ =>
        ModelNode(children.init :+ x.init, this.t)
    }
  }
}

case class ModelLeaf(name:String,t:Double) extends ModelChild with PrimitiveLeaf {
  def leafList = this :: Nil
  def changeBranches(branches:List[Double]) = (ModelLeaf(name,branches.head),branches.tail)
  def init = null
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
