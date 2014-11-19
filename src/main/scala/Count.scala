import breeze.linalg.{DenseMatrix, DenseVector}
import java.io.FileReader
import scala.util.parsing.combinator.JavaTokenParsers

class Count(val Fd:List[DenseVector[Double]],val Ns:List[DenseMatrix[Double]],
                 val T:List[Double],val ns:DenseVector[Double],val ll:Double){

  def +(that:Count):Count =
    Count((Fd,that.Fd).zipped.map(_ + _),(Ns,that.Ns).zipped.map(_ + _),(T,that.T).zipped.map(_ + _),ns + that.ns,ll + that.ll)

  def *(arg:Double) = Count(Fd.map(_*arg),Ns.map(_*arg),T.map(_*arg),ns * arg,ll)

  def /(arg:Double) = Count(Fd.map(_/arg),Ns.map(_/arg),T.map(_/arg),ns / arg,ll)

  override def toString = "Count(" + Fd + "," + Ns + "," + T + "," + ns + "," + ll + ")"

}

object Count extends CountParser{

  def apply(Fd:List[DenseVector[Double]],Ns:List[DenseMatrix[Double]],
            T:List[Double],ns:DenseVector[Double],ll:Double) = new Count(Fd,Ns,T,ns,ll)

  def fromString(txt:String):Count = parseAll(count,txt).get

  def fromFile(fin:String):Count = {
    val reader = new FileReader(fin)
    val tmp = parseAll(count,reader).get
    reader.close()
    tmp
  }
}

//may include bug in method "matrix"
class CountParser extends JavaTokenParsers {

  def vector: Parser[DenseVector[Double]] =  "DenseVector("~> repsep(value,",") <~")"  ^^
    {case x => DenseVector(x.toArray)}

  def matrix: Parser[DenseMatrix[Double]] = "DenseMatrix(" ~> repsep(value,"," | "),(") <~")" ^^
    {case x => DenseMatrix(4,4,x.toArray).t}

  def mlist: Parser[List[DenseMatrix[Double]]] = "List(" ~> repsep(matrix,",") <~ ")"

  def vlist: Parser[List[DenseVector[Double]]] = "List(" ~> repsep(vector,",") <~ ")"

  def dlist: Parser[List[Double]] = "List(" ~> repsep(value,",") <~ ")"

  def count: Parser[Count] = "Count("~> vlist~","~mlist~","~repsep(value,",")~","~vector~","~value <~")" ^^
    {case a~","~b~","~c~","~d~","~e => Count(a,b,c,d,e)}

  def value: Parser[Double] = floatingPointNumber ^^ (_.toDouble)
}
