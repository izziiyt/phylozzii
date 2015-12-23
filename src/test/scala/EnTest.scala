import main.Entrop
import org.scalatest.FunSuite

/**
  * Created by yuto on 15/12/17.
  */
class EnTest extends FunSuite{
  test("hogehoge"){
    val f = "/home/yuto/hoge.aln"
    Entrop.main(Array(f))
  }
}
