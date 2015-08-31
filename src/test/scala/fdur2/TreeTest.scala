package fdur2

import breeze.linalg.DenseVector
import breeze.numerics.abs
import org.scalatest.FunSuite
import alignment.Base

import scala.util.Random

class TreeTest extends FunSuite {
  test("PrimitiveTree") {
    {
      val t = ModelTree.fromString("((((a:1.0,b:2.0):3.0,c:4.0):5.0,d:6.0):7.0,e:8.0);")
      assert(t.toString == "((((a:1.0,b:2.0):3.0,c:4.0):5.0,d:6.0):7.0,e:8.0);")
      assert(t.length == 9)
      assert(t.leafLength == 5)
      assert(t.branches == List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0))
      val s = t.changeBranches(List(2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0))
      assert(s.branches == List(2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0))
    }
    {
      val t = ModelTree.fromString("(((a:1.0,b:2.0,c:3.0):4.0,d:5.0):6.0,e:7.0);")
      assert(t.toString == "(((a:1.0,b:2.0,c:3.0):4.0,d:5.0):6.0,e:7.0);")
      assert(t.length == 8)
      assert(t.leafLength == 5)
      assert(t.branches == List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))
      val s = t.changeBranches(List(2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0))
      assert(s.branches == List(2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0))
    }
    {
      val t = ModelTree.fromString("((a:1.0,b:2.0,c:3.0):4.0,(d:5.0,e:6.0):7.0);")
      assert(t.toString == "((a:1.0,b:2.0,c:3.0):4.0,(d:5.0,e:6.0):7.0);")
      assert(t.length == 8)
      assert(t.leafLength == 5)
      assert(t.branches == List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0))
      val s = t.changeBranches(List(2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0))
      assert(s.branches == List(2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0))
    }
  }
  test("Tree") {
    {
      val tree = ModelTree.fromString("((a:1.0,b:2.0):3.0,c:4.0);")
      val param = Parameters(DenseVector[Double](0.1, 0.2, 0.3, 0.4, 0.5, 0.6), DenseVector[Double](0.1, 0.2, 0.3, 0.4))
      val cols = List(Array[Base](Base.A), Array[Base](Base.C), Array[Base](Base.G))
      val root = Tree.inout(tree, Model(param), cols)
      assert(root.toList.map(_.alpha.head) == List(
        DenseVector(1.0, 0.0, 0.0, 0.0),
        DenseVector(0.0, 1.0, 0.0, 0.0),
        DenseVector(0.037916113737557565, 0.0061431966414293455, 0.0020895120688616793, 0.0032263653760986927),
        DenseVector(0.0, 0.0, 1.0, 0.0),
        DenseVector(0.003935947883108595, 0.0013438580299404868, 0.0019294855949989174, 0.0014930250881236683)))
      assert(root.toList.map(_.beta.head) == List(
        DenseVector(0.001084463180635185, 0.03190563010964688, 0.011117852783783938, 0.015309646162751444),
        DenseVector(0.019272492583596635, 6.407904172066742E-4, 0.0018309353265542714, 0.0030412928271287545),
        DenseVector(0.017261744780030307, 0.049257030784122885, 0.1219976354441457, 0.11148358899170095),
        DenseVector(0.0022801564576843923, 0.0010913025073152848, 0.0014234186008417078, 0.0021427729072982207),
        DenseVector(0.1, 0.2, 0.3, 0.4)))
    }
  }
  test("diffTest") {
    val gen: Random = new Random()
    val templateBranch: List[(Double, Int)] = List(0.3, 0.4, 0.6, 0.5).zipWithIndex
    val templateTree = ModelTree.fromString("((a:0.3,b:0.4):0.6,c:0.5);")
    val pi = Array(0.1, 0.2, 0.3, 0.4)
    val b = Array(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
    val param = Parameters(DenseVector(b),DenseVector(pi))
    val cols = List(Array[Base](Base.A), Array[Base](Base.C), Array[Base](Base.G))
    val root = Tree.inout(templateTree, Model(param), cols)
    for (_ <- 0 until 10) {
      val h = gen.nextDouble() / 10.0
      println(h)
      for (j <- 0 to 3) {
        val tree1 = templateTree.changeBranches(templateBranch.map { case (x, i) => if (i == j) x + h else x })
        println(tree1)
        val tree2 = templateTree.changeBranches(templateBranch.map { case (x, i) => if (i == j) x - h else x })
        val root1 = Tree.inout(tree1, Model(param), cols)
        val root2 = Tree.inout(tree2, Model(param), cols)
        val diffCal = (root1.loglikelihood, root2.loglikelihood).zipped.map((x, y) => (x - y) / (2.0 * h))
        println("branches length : " + root.diffWithT(j).head + " " + diffCal.head)
      }
      for (j <- 0 to 3) {
        val pi1 = pi.zipWithIndex.map{case (x,i) => if(i == j) x + h else x}
        val pi2 = pi.zipWithIndex.map{case (x,i) => if(i == j) x - h else x}
        val param1 = Parameters(DenseVector(b),DenseVector(pi1))
        val param2 = Parameters(DenseVector(b),DenseVector(pi2))
        val root1 = Tree.inout(templateTree, Model(param1), cols)
        val root2 = Tree.inout(templateTree, Model(param2), cols)
        val diffCal = (root1.loglikelihood, root2.loglikelihood).zipped.map((x, y) => (x - y) / (2.0 * h))
        println("pi : " + root.diffWithPi.head(j) + " " + diffCal.head)
      }
      for (j <- 0 to 5) {
        val b1 = b.zipWithIndex.map{case (x,i) => if(i == j) x + h else x}
        val b2 = b.zipWithIndex.map{case (x,i) => if(i == j) x - h else x}
        val param1 = Parameters(DenseVector(b1),DenseVector(pi))
        val param2 = Parameters(DenseVector(b2),DenseVector(pi))
        val root1 = Tree.inout(templateTree, Model(param1), cols)
        val root2 = Tree.inout(templateTree, Model(param2), cols)
        val diffCal = (root1.loglikelihood, root2.loglikelihood).zipped.map((x, y) => abs(x - y) / (2.0 * h))
        //println("rate : " + root.diffWithB.head(j) + " " + diffCal.head)
      }
    }
  }
}
