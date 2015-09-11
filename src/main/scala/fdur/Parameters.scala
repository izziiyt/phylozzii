package fdur

import java.io.FileReader

import breeze.linalg.{sum, DenseVector}
import breeze.numerics.exp

import scala.util.parsing.combinator.JavaTokenParsers

sealed class Parameters(val Bvec:VD,val pi:VD){
  require(Bvec.length == 6)
  require(pi.length == 4)

  def * (x:Double) = Parameters(Bvec * x,pi * x)
  def + (that:Parameters) = Parameters(this.Bvec + that.Bvec,this.pi + that.pi)
  def -(that:Parameters) = Parameters(this.Bvec - that.Bvec,this.pi - that.pi)

  def reglize = Parameters(this.Bvec / sum(Bvec),this.pi / sum(pi))

  def a = Bvec(0)
  def b = Bvec(1)
  def c = Bvec(2)
  def d = Bvec(3)
  def e = Bvec(4)
  def f = Bvec(5)

  override def toString:String = "Parameters(" + Bvec + "," + pi + ")"
}

object Parameters extends ParameterParser{
  def apply(Bvec:VD,pi:VD) = new Parameters(Bvec,pi)

  def apply(v:VD) = {
    require(v.length == 10)
    val b = exp(v(0 to 5)) / sum(exp(v(0 to 5)))
    val pi = exp(v(6 to 9)) / sum(exp(v(6 to 9)))
    new Parameters(b,pi)
  }

  def fromFile(fin:String):Parameters = {
    val reader = new FileReader(fin)
    val tmp = parseAll(parameters,reader).get
    reader.close()
    tmp
  }
  def fromString(txt:String):Parameters = parseAll(parameters,txt).get
}

sealed class ParameterParser extends JavaTokenParsers {
  def vector: Parser[VD] =  "DenseVector("~> repsep(value,",") <~")"  ^^
    {case x => DenseVector(x.toArray)}
  def parameters: Parser[Parameters] = "Parameters("~> repsep(vector,",") <~")" ^^
    {case x => Parameters(x.head,x(1))}
  def value: Parser[Double] = floatingPointNumber ^^ (_.toDouble)
}