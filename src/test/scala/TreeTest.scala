import alignment.Base
import breeze.linalg.{DenseMatrix, DenseVector, diag, sum}
import fdur._
import org.scalatest.FunSuite
import util._

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
    {
      val t = ModelTree.fromFile("src/test/resources/fdur/fdur.nh")
      val br = t.branches
      val newt = t.changeBranches(br)
      val newbr = newt.branches
      (br,newbr).zipped.foreach((x,y) => assert(x == y))
    }
  }

  test("Tree") {
      val tree = ModelTree.fromString("((a:1.0,b:2.0):3.0,c:4.0);")
      val param = Parameters(DenseVector[Double](0.1, 0.2, 0.3, 0.4, 0.5, 0.6), DenseVector[Double](0.1, 0.2, 0.3, 0.4))
      val cols = List(Array[Base](Base.A), Array[Base](Base.C), Array[Base](Base.G))
      val root = Tree.inout(tree, Model(param), cols)
      val l = root.toList
      l.foreach{z => for(i <- 0 to 3) assert(doubleEqual(sum(z.trans(::,i)),1.0))}
      (l,Seq(1.0, 2.0, 3.0, 4.0, 0.0)).zipped.foreach((x,y) => assert(x.t == y))
      l.foreach{z => z.post.foreach(p => assert(doubleEqual(sum(p), 1.0)))}
      root.toList.map(_.alpha.head) zip List(
        DenseVector(1.0, 0.0, 0.0, 0.0),
        DenseVector(0.0, 1.0, 0.0, 0.0),
        DenseVector(0.037916113737557516, 0.006143196641429347, 0.0020895120688616793, 0.003226365376098689),
        DenseVector(0.0, 0.0, 1.0, 0.0),
        DenseVector(0.003935947883108591, 0.0013438580299404874, 0.0019294855949989176, 0.0014930250881236662)
      ) foreach {case (x , y) => assert(doubleEqual(x, y, 1.0E-6))}
      root.toList.map(_.beta.head) zip List(
        DenseVector(0.0010844631806351836, 0.0319056301096469, 0.011117852783783955, 0.015309646162751439),
        DenseVector(0.019272492583596635, 6.407904172066744E-4, 0.0018309353265542714, 0.003041292827128754),
        DenseVector(0.01726174478003031, 0.04925703078412293, 0.12199763544414577, 0.11148358899170092),
        DenseVector(0.0022801564576843897, 0.0010913025073152846, 0.0014234186008417068, 0.002142772907298218),
        DenseVector(0.1, 0.2, 0.3, 0.4)
      ) foreach {case (x, y) => assert(doubleEqual(x, y, 1.0E-6))}
    }
  test("LDTree"){
    val tree = ModelTree.fromString("((a:1.0,b:2.0):3.0,c:4.0);")
    val param = Parameters(DenseVector[Double](0.1, 0.2, 0.3, 0.4, 0.5, 0.6), DenseVector[Double](0.1, 0.2, 0.3, 0.4))
    val cols = List(Array[Base](Base.A), Array[Base](Base.C), Array[Base](Base.G))
    val root = LDTree.inout(tree, Model(param), cols)
    val l = root.toList
    l.foreach{z => for(i <- 0 to 3) assert(doubleEqual(sum(z.trans(::,i).value),1.0,1.0E-5))}
    (l,Seq(1.0, 2.0, 3.0, 4.0, 0.0)).zipped.foreach((x,y) => assert(x.t == y))
    l.foreach{z => z.post.foreach(p => assert(doubleEqual(sum(p), 1.0)))}
    root.toList.map(_.alpha.head.value) zip List(
      DenseVector(1.0, 0.0, 0.0, 0.0), DenseVector(0.0, 1.0, 0.0, 0.0),
      DenseVector(0.03791611373755752, 0.0061431966414293455, 0.002089512068861679, 0.00322636537609869),
      DenseVector(0.0, 0.0, 1.0, 0.0),
      DenseVector(0.003935947883108591, 0.0013438580299404877, 0.0019294855949989192, 0.0014930250881236662)
    ) foreach {case (x, y) => assert(doubleEqual(x, y, 1.0E-6))}
    root.toList.map(_.beta.head.value) zip List(
      DenseVector(0.001084463180635185, 0.03190563010964691, 0.011117852783783957, 0.015309646162751453),
      DenseVector(0.019272492583596645, 6.407904172066745E-4, 0.0018309353265542716, 0.003041292827128754),
      DenseVector(0.017261744780030307, 0.04925703078412295, 0.12199763544414573, 0.11148358899170094),
      DenseVector(0.002280156457684389, 0.001091302507315284, 0.0014234186008417074, 0.0021427729072982173),
      DenseVector(0.10000000000000002, 0.2, 0.3, 0.4)
    ) foreach {case (x, y) => assert(doubleEqual(x, y, 1.0E-6))}
  }
  test("densevector"){
    val x = new DenseMatrix[Int](2,2,Array(3,4,5,6))
    assert(x(0,0) == 3)
    assert(x(1,0) == 4)
    assert(x(0,1) == 5)
    assert(x(1,1) == 6)
    val y = diag(x)
    assert(y(0) == 3)
    assert(y(1) == 6)
    assert(Array.tabulate(3)(i => i*2) sameElements Array(0,2,4))
    val a = DenseVector[Int](1,2)
    val b = DenseVector[Int](3,4)
    assert(DenseMatrix.vertcat(a.asDenseMatrix, b.asDenseMatrix) == new DenseMatrix[Int](2,2,Array(1,3,2,4)))
    assert(DenseVector.vertcat(a,b) == DenseVector[Int](1,2,3,4))
  }
  test("Normal vs LD"){
    val pi = Array(0.22, 0.28, 0.23, 0.27)
    val b = Array(0.15, 0.25, 0.1, 0.13, 0.07, 0.3)
    val templateTree = ModelTree.fromString("((a:0.3,b:0.4):0.6,(c:0.5,d:0.6):0.7,e:0.1);")
    val param = Parameters(DenseVector(b), DenseVector(pi))
    val cols = List(Array[Base](Base.A), Array[Base](Base.C),Array[Base](Base.C), Array[Base](Base.G),Array[Base](Base.N))
    val root = Tree.inout(templateTree, Model(param), cols)
    val ldroot = LDTree.inout(templateTree, Model(param), cols)
    (root.toList,ldroot.toList).zipped.foreach((x,y) => assert(doubleEqual(x.alpha.head,y.alpha.head.value,1.0E-7)))
    (root.toList,ldroot.toList).zipped.foreach((x,y) => assert(doubleEqual(x.beta.head,y.beta.head.value,1.0E-7)))
    (root.toList,ldroot.toList).zipped.foreach((x,y) => assert(doubleEqual(x.post.head,y.post.head,1.0E-7)))
  }
  test("diffTest") {
    val gen: Random = new Random()
    val templateBranch: List[(Double, Int)] = List(0.3, 0.4, 0.6, 0.5, 0.6, 0.7, 0.1).zipWithIndex
    val templateTree = ModelTree.fromString("((a:0.3,b:0.4):0.6,(c:0.5,d:0.6):0.7,e:0.1);")
    val pi = Array(0.22, 0.28, 0.23, 0.27)
    val b = Array(0.15, 0.25, 0.1, 0.13, 0.07, 0.3)
    val param = Parameters(DenseVector(b), DenseVector(pi))
    val cols = List(Array[Base](Base.A), Array[Base](Base.C),Array[Base](Base.C), Array[Base](Base.G),Array[Base](Base.N))
    val root = Tree.inout(templateTree, Model(param), cols)
    val ldroot = LDTree.inout(templateTree, Model(param), cols)
    for (_ <- 0 until 100) {
      val h = gen.nextDouble() / 10000.0
      for (j <- templateBranch.indices) {
        val tree1 = templateTree.changeBranches(templateBranch.map { case (x, i) => if (i == j) x + h else x })
        val tree2 = templateTree.changeBranches(templateBranch.map { case (x, i) => if (i == j) x - h else x })
        val root1 = Tree.inout(tree1, Model(param), cols)
        val root2 = Tree.inout(tree2, Model(param), cols)
        val diffCal = (root1.loglikelihood, root2.loglikelihood).zipped.map((x, y) => (x - y) / (2.0 * h))
        (root.diffWithT(j), diffCal).zipped.foreach{(x,y) => assert(doubleEqual(x, y, 1.0E-5))}
        (ldroot.diffWithT(j), diffCal).zipped.foreach{(x,y) => assert(doubleEqual(x, y, 1.0E-5))}
      }
      for (j <- 0 to 3) {
        val pi1 = pi.zipWithIndex.map{case (x,i) => if(i == j) x + h else x}
        val pi2 = pi.zipWithIndex.map{case (x,i) => if(i == j) x - h else x}
        val param1 = Parameters(DenseVector(b),DenseVector(pi1))
        val param2 = Parameters(DenseVector(b),DenseVector(pi2))
        val root1 = Tree.inout(templateTree, Model(param1), cols)
        val root2 = Tree.inout(templateTree, Model(param2), cols)
        val diffCal = (root1.loglikelihood, root2.loglikelihood).zipped.map((x, y) => (x - y) / (2.0 * h))
        (root.diffWithPi, diffCal).zipped.foreach((x,y) => assert(doubleEqual(x(j), y, 1.0E-4)))
        (ldroot.diffWithPi, diffCal).zipped.foreach((x,y) => assert(doubleEqual(x(j), y, 1.0E-4)))
      }
      var j = 0
      for (s <- 0 to 2;t <- s+1 to 3) {
        val b1 = b.zipWithIndex.map{case (x,i) => if(i == j) x + h else x}
        val b2 = b.zipWithIndex.map{case (x,i) => if(i == j) x - h else x}
        val param1 = Parameters(DenseVector(b1),DenseVector(pi))
        val param2 = Parameters(DenseVector(b2),DenseVector(pi))
        val root1 = Tree.inout(templateTree, Model(param1), cols)
        val root2 = Tree.inout(templateTree, Model(param2), cols)
        val diffCal = (root1.loglikelihood, root2.loglikelihood).zipped.map((x, y) => (x - y) / (2.0 * h))
        (root.diffWithB, diffCal).zipped.foreach((x,y) => assert(doubleEqual(x(s,t), y, 1.0E-4)))
        (ldroot.diffWithB, diffCal).zipped.foreach((x,y) => assert(doubleEqual(x(s,t), y, 1.0E-4)))
        j += 1
      }
    }
  }
}
