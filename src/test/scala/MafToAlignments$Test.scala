import org.scalatest.FunSuite

class MafToAlignments$Test extends FunSuite {
  test("test"){
    MafToAlignments("src/test/resources/data.filtered.maf","src/test/resources/ce10.7way.nh","src/test/resources/test.al")
  }
}
