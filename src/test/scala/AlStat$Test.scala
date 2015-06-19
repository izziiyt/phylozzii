import org.scalatest.FunSuite
import util.SlctSpec

class AlStat$Test extends FunSuite {
  //test("alstat"){AlStat.exe("src/test/resources/alignments","target/huga.txt")}
  test("slct"){SlctSpec.main(Array("src/test/resources/alignments","target","src/test/resources/choice.txt"))}
}
