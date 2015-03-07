import org.scalatest.FunSuite

class MafTest extends FunSuite {
  test("parseMaf"){
    val mf = FilteredMafParser("src/test/resources/sample.maf")
  }
}
