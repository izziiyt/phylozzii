import org.scalatest.FunSuite

/**
  * Created by yuto on 15/12/17.
  */
class EnTest extends FunSuite{
  test("entrop"){
    val f = "/home/yuto/hoge.aln"
    main.Main.main(Array("entrop",f))
  }
}
