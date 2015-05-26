import org.scalatest.FunSuite

class AlStat$Test extends FunSuite {
  //test("alstat"){AlStat.exe("src/test/resources/alignments","target/huga.txt")}
  test("slct"){SlctSpec.main(Array("src/test/resources/alignments","target","src/test/resources/choice.txt"))}
}
