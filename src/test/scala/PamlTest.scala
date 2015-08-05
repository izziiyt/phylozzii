import org.scalatest.FunSuite
import util.Nuc2Al


class PamlTest extends FunSuite{
  test("brown") {
    Nuc2Al("src/test/resources/brown/brown.nuc", "src/test/resources/brown/brown.al")
    val alignments = util.getAlignments("src/test/resources/brown/brown.al")
    new fdur.EM().test(200, "src/test/resources/brown/brown.trees", alignments, System.out)
  }
}
