import java.io.PrintWriter

import fdur.FdurTree
import org.scalatest.FunSuite

class TreeTest extends FunSuite {
  val nr = "src/test/resources/hg19.100way.nh"
  val r =  "src/test/resources/hg19.100way.nhr"
  test("4-fold degenerate"){
    val ntree = FdurTree.fromFile(nr)
    println(ntree.names)
    val rtree = FdurTree.fromFile(r)
    val a = (rtree.branches zip ntree.branches).map{case (x,y) => x / y}
    val c = new PrintWriter("src/test/resources/result.txt")
    //c.println(ntree.setBranch(a))
    ntree.setBranch(a)
    c.println(ntree)
    c.close()
  }

  //test("60-way"){
  //  val tree = FdurTree.fromFile("src/test/resources/mm10.60way.commonNames.nh")
  //  tree.names.foreach(println)
  //}
  test("100-way"){
    val tree = FdurTree.fromFile("src/test/resources/hg19.100way.nh")
    tree.names.foreach(println)
  }
  //val source = "src/test/resources/sample.nh"
  //val tree = Tree.fromFile(source)

  /*test("construct"){

    def names(tree:Tree):List[String] = {
      tree match {
        case Node(left,right,_) => names(left) ::: names(right)
        case Leaf(name,_) => List(name)
      }
    }

    def branches(tree:Tree):List[Double] = {
      tree match {
        case Node(left,right,cont) => branches(left) ::: branches(right) ::: List(cont.t)
        case Leaf(_,cont) => List(cont.t)
      }
    }

    assert(names(tree) == List("ce10","cb4","caeRem4"))
    assert(branches(tree) == List(0.5,0.4,0.3,0.2,0.0))
  }

  test("setAlignment"){
    tree.setColumn(Array(1,2,3).map(_.toChar))

    def column(tree:Tree):List[Char] = {
      tree match{
        case Node(left,right,_) => column(left) ::: column(right)
        case Leaf(_,cont) => List(cont.nuc)
      }
    }

    assert(column(tree) == Array(1,2,3))
  }

  test("toString"){
    val txt = Source.fromFile(source).getLines().reduce(_+_)
    assert(tree.toString == txt)
  }*/

  /*test("branch"){
    val tree = Tree.fromFile("src/test/resources/hg19.100way.nh")
    println(tree.branches)
    val branches = tree.branches.map(_/2.0)
    tree.setBranch(branches)
    println(tree.branches)
    (tree.names zip (0 until tree.names.length)).foreach{println}
  }*/
}
