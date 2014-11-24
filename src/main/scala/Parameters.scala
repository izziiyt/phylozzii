import breeze.linalg.DenseVector
import java.io.FileReader
import scala.util.parsing.combinator.JavaTokenParsers

class Parameters(val Bvec:DenseVector[Double],val pi:DenseVector[Double]){
  require(Bvec.length == 6)
  require(pi.length == 4)

  def + (that:Parameters) = Parameters(this.Bvec + that.Bvec,this.pi + that.pi)
  def / (that:Double) = Parameters(this.Bvec / that,this.pi / that)
  def :* (that:Parameters) = Parameters(this.Bvec :* that.Bvec,this.pi :* that.pi)
  def * (that:Double) = Parameters(this.Bvec * that,this.pi * that)

  def a = Bvec(0)
  def b = Bvec(1)
  def c = Bvec(2)
  def d = Bvec(3)
  def e = Bvec(4)
  def f = Bvec(5)

  override def toString:String = "Parameters(" + Bvec + "," + pi + ")"
}

object Parameters extends ParameterParser{
  def apply(Bvec:DenseVector[Double],pi:DenseVector[Double]) = new Parameters(Bvec,pi)
  def fromFile(fin:String):Parameters = {
    val reader = new FileReader(fin)
    val tmp = parseAll(parameters,reader).get
    reader.close()
    tmp
  }
  def fromString(txt:String):Parameters = parseAll(parameters,txt).get
}

class ParameterParser extends JavaTokenParsers {
  def vector: Parser[DenseVector[Double]] =  "DenseVector("~> repsep(value,",") <~")"  ^^
    {case x => DenseVector(x.toArray)}
  def parameters: Parser[Parameters] = "Parameters("~> repsep(vector,",") <~")" ^^
    {case x => Parameters(x(0),x(1))}
  def value: Parser[Double] = floatingPointNumber ^^ (_.toDouble)
}