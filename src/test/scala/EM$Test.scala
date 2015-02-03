import org.scalatest.FunSuite

class EM$Test extends FunSuite {
  val em = new EM

  /*test("EM"){
   /*
   This test is collect when all regularized phylogency tree's branches length are integral.
   test.al: single colmn is written
   test.nh: three-species phylogency tree is written
   */
    Util.printExecutionTime(em.test(2000,"src/test/resources/test.nh","src/test/resources/test.al"),"EM")
  }*/

  /*test("EMQsub"){

    val dir = "src/test/resources/alignments/"
    val cdir = "src/test/resources/count/"
    val alFiles = new java.io.File(dir).listFiles.map(_.getName)
    val countFileList = new java.io.File(cdir).listFiles.map(cdir + _.getName)
    for(i <- 1 to 10){
      alFiles.foreach(f => Estep.exe("src/test/resources/param.txt",dir + f,
        "src/test/resources/ce10.7way.nh",cdir + f + ".ct"))
      Mstep.exe(countFileList,"src/test/resources/param.txt","src/test/resources/ce10.7way.nh","src/test/resources/log")
    }

    PostProc.exe("src/test/resources/log/","src/test/resources/ce10.7way.nh","src/test/resources/param.txt","target/rtree.txt")
    }*/

  test("EMnext"){
    Util.printExecutionTime(em.test(10,"src/test/resources/ce10.7way.nh","src/test/resources/1.al"),"EM")
  }
}
