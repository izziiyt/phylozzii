import org.scalatest.FunSuite

class AminoAcidTest extends FunSuite {
  test("AminoAcid"){
    val x = AminoAcid.fromString("Thr")
    println(x.toWord)
    val y = AminoAcid.fromString("A")
    println(y)
  }
}
