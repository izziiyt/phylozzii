import breeze.linalg.{DenseMatrix, DenseVector}
import fdur._

import scala.util.parsing.combinator.JavaTokenParsers

package object main {
  trait SuffStatParser extends JavaTokenParsers {
    //def ns: Parser[DenseVector[Double]] = "ns:" ~> densevector

    def doubleArray: Parser[Array[Double]] = repsep(floatingPointNumber,",") ^^ { case xs => xs.map(_.toDouble).toArray }

    def densevector: Parser[DenseVector[Double]] = "(" ~> doubleArray <~ ")" ^^ { case xs => DenseVector(xs) }

    def Ns: Parser[List[DenseVector[Double]]] = "[Nn]s:".r ~> repsep(densevector,",")

    def Fd: Parser[List[DenseMatrix[Double]]] = "Fd:" ~> repsep(densematrix(4, 4),",")

    def densematrix(n: Int, m: Int): Parser[DenseMatrix[Double]] = "(" ~> doubleArray <~ ")" ^^ { case xs => new DenseMatrix[Double](n, m, xs) }

    def lgl: Parser[Double] = "lgl:" ~> floatingPointNumber ^^ { x => x.toDouble }

    def length: Parser[Long] = "length:" ~> wholeNumber ^^ { x => x.toLong }

    def suffstat: Parser[(VD, List[MD], List[VD], Double, Long)] =
      Ns ~ Fd ~ Ns ~ lgl ~ length ^^ { case x ~ y ~ z ~ w ~ v => (x.head, y, z, w, v) }

    def pi: Parser[DenseVector[Double]] = "pi:" ~> densevector

    def b: Parser[DenseMatrix[Double]] = "b:" ~> densematrix(4, 4)

    //def branch: Parser[List[Double]] = "branch: (" ~> repsep(floatingPointNumber, "") <~ ")" ^^ { case xs => xs.map(_.toDouble) }

   // def gradient: Parser[(VD, MD, List[Double], Double)] =
   //   pi ~ b ~ branch ~ lgl ^^ { case x ~ y ~ z ~ w => (x, y, z, w) }
  }

}