package eea

import java.io.FileReader
import alignment.Base
import fdur._

trait EEATree extends Tree[EEAContent] {

   def find(name:String):Option[EEALeaf]
}

trait HavingChildren extends EEATree {

  def left:EEATree

  def right:EEATree

  def find(n:String):Option[EEALeaf] = {
    lazy val l = left.find(n)
    lazy val r = right.find(n)
    if(l.isDefined) l
    else if(r.isDefined) r
    else None
  }

  def setColumn(column:Array[Base]) = {
    val tmp = left.setColumn(column)
    right.setColumn(tmp)
  }

}

case class EEALeaf(name:String,br:Double,cont:EEALeafContent) extends EEATree {

  private var base:Base = Base.N


  def setColumn(column:Array[Base]) = {
    base = column.head
    try column.tail
    catch {case Throwable => Array()}
  }

  def find(n: String):Option[EEALeaf] = if (n == name) Some(this) else None
}

case class EEARoot(left:EEATree,right:EEATree,cont:EEARootContent) extends HavingChildren {

}


case class EEANode(left:EEATree,right:EEATree,br:Double,cont:EEAContent) extends HavingChildren {

}
object EEATree extends NHParser4EEA{
  def fromFile(nhFile:String):EEANode = {
    val reader = new FileReader(nhFile)
    parseAll(tree,reader).get
  }
  def fromString(nhString:String):EEANode = {
    parseAll(tree,nhString).get
  }
}
class NHParser4EEA extends NHParser[EEATree] {

  def tree:Parser[EEARoot] =  nodePair<~";"  ^^
    {case (left,right) => EEARoot(left,right,null)}

  def node:Parser[EEATree] = nodePair~":"~value ^^
    {case (left,right)~":"~value => EEANode(left,right,value.toDouble,null)} | leaf

  def leaf:Parser[EEALeaf] = name~":"~value ^^
    {case name~":"~value => EEALeaf(name,value.toDouble,null)}

}