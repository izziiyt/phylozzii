package fdur

import breeze.plot._
import org.scalatest.FunSuite
import breeze.linalg._

class Fdur extends FunSuite{
  test("fdur"){
    val nh = ModelTree.fromFile("src/test/resources/fdur/fdur.nh")
    val cols = Maf.readMaf("src/test/resources/fdur/fdur.maf",1000).toParArray
    val pi = DenseVector(0.23137857635453807, 0.28408070157281884, 0.27729375318455474, 0.20724696888808836)
    val b = DenseVector(0.6586096484894902, 2.329377965568423, 0.8207872557873153, 0.9101830004835019, 2.7967009808428305, 0.5488972207907554)
    val (embrnch,emparam) = Optimizer.ldem(10000, nh, cols, Parameters(b,pi))
    val (gdbrnch,gdparam) = Optimizer.ldgd(10000, nh, cols, Parameters(b,pi).asGD)
    val f = Figure()
    val p1 = f.subplot(0)
    val x1 = linspace(gdbrnch.min, gdbrnch.max)
    p1 += plot(gdbrnch, embrnch, '+')
    p1 += plot(x1, x1, '-')
    p1.title = "phylotree's branches length"
    val p2 = f.subplot(2, 2, 1)
    val x2 = linspace(min(gdparam.pi), max(gdparam.pi))
    p2 += plot(gdparam.pi, emparam.pi, '+')
    p2 += plot(x2, x2, '-')
    p2.title = "base frequency"
    val p3 = f.subplot(2, 2, 2)
    val x3 = linspace(min(gdparam.Bvec), max(gdparam.Bvec))
    p3 += plot(gdparam.Bvec, emparam.Bvec, '+')
    p3 += plot(x3, x3, '-')
    p3.title = "mutation rate"
    f.saveas("target/R.png")
  }
}
