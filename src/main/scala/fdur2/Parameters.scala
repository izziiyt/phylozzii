package fdur2

import java.io.FileReader

import breeze.linalg.DenseVector

import scala.util.parsing.combinator.JavaTokenParsers

sealed class Parameters(val Bvec:VD,val pi:VD){
  require(Bvec.length == 6)
  require(pi.length == 4)

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