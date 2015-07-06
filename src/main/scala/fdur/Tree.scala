package fdur

import scala.util.parsing.combinator.JavaTokenParsers
import alignment.Base

trait Tree{
  override def toString:String
}
/*
trait Root[T] extends HavingChildren[T]

trait HavingChildren[T] extends Tree[T]

trait Node[T] extends HavingChildren[T]

trait Leaf[T] extends Tree[T]
*/
trait NHParser[Tree] extends JavaTokenParsers {

  def tree:Parser[Tree]

  def node:Parser[Tree]

  def leaf:Parser[Tree]

  def nodePair:Parser[(Tree,Tree)] = "("~>node~","~node<~")"  ^^
    {case left~","~right => (left,right)}

  def value:Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  def name:Parser[String] = ident
}

