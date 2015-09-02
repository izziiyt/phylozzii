package fdur2

import breeze.linalg.DenseVector
import org.scalatest.FunSuite

class EMTest extends FunSuite {
  /*test("em"){
    val pi = Array(0.22, 0.28, 0.23, 0.27)
    val b = Array(0.15, 0.25, 0.1, 0.13, 0.07, 0.3)
    var tree = ModelTree.fromString("((a:0.3,b:0.4):0.6,c:0.5);")
    val cols = Maf.readMaf("src/test/resources/fdur2/test1.maf",10)
    var param = Parameters(DenseVector(b), DenseVector(pi))
    for(_ <- 0 until 10){
      val model = Model(param)
      val hoge = EM.estep(tree,cols,model)
      val (lgl,br,pr) = EM.mstep(hoge,model,tree.branches)
      println(lgl)
      tree = tree.changeBranches(br)
      param = pr
    }
    println(param)
    println(tree)
  }
  test("fdur"){
    val pi = Array(0.22, 0.28, 0.23, 0.27)
    val b = Array(0.15, 0.25, 0.1, 0.13, 0.07, 0.3)
    var tree = ModelTree.fromFile("src/test/resources/fdur2/fdur.nh")
    val cols = Maf.readMaf("src/test/resources/fdur2/fdur.maf",1000).toParArray
    var param = Parameters(DenseVector(b), DenseVector(pi))
    for(_ <- 0 until 100) printExeTime({
      val model = Model(param)
      val hoge = cols.map(EM.estep(tree,_,model)).toArray
      val (lgl,br,pr) = EM.mstep(hoge,model,tree.branches)
      println(lgl)
      tree = tree.changeBranches(br)
      param = pr
    },"rec ")
    println(param)
    println(tree)
  }*/
  test("fdurResult") {
    val myparam = Parameters(DenseVector(0.1024224241487677, 0.3082978987546536, 0.07335199358058798,
      0.11473275493834312, 0.29523969385476195, 0.10595523472288547),
      DenseVector(0.2638330906503499, 0.2464216036645157, 0.23994483748444115, 0.2498004682006931))
    val mytree = ModelTree.fromString("(((((((((((((((((hg18: 0.031050061714568592, panTro2: 0.027248907999912756): " +
      "0.01380114419980392, gorGor1: 0.04833980310561208): 0.04678705238720325, ponAbe2: 0.12137232337601672):" +
      " 0.04854632501728731, rheMac2: 0.19853176714398857): 0.10077730874484839, calJac1: 0.41696669179052404):" +
      " 0.33887746807960556, tarSyr1: 0.7260050158999988): 0.037867351323713945, (micMur1: 0.4077057606670751," +
      " otoGar1: 0.6767748942401212): 0.26303103297730884): 0.1029219809208855, tupBel1: 1.0824472011006971): " +
      "0.03327331737818512, (((((mm9: 0.3440043500147498, rn4: 0.35535353311603224): 0.9837933910365027, dipOrd1:" +
      " 1.0288442034507717): 0.23019160685610163, cavPor3: 1.0477603948470833): 0.08409244296798192, speTri1:" +
      " 1.0045136734332125): 0.20676584805616083, (oryCun1: 0.6502676936993182, ochPri2: 1.2782115578807223): " +
      "0.5971807401357273): 0.12684423913010476): 0.20799472201418268, (((vicPac1: 0.5976158194179941," +
      " (turTru1: 0.3135357439913647, bosTau4: 0.5117870537371931): 0.09550051166134998): 0.1650409742100054, " +
      "((equCab2: 0.49737791639535506, (felCat3: 0.4694818999439216, canFam2: 0.33400887510002913): " +
      "0.26204713572743105): 0.03587912595959122, (myoLuc1: 0.8451509043980633, pteVam1: 0.5542767085119715):" +
      " 0.20259983986677585): 0.006156988696660061): 0.058876966981449966, (eriEur1: 1.2944672571288782, sorAra1:" +
      " 1.3399609123039598): 0.5716518521383824): 0.214976442628984): 0.10304212625248713, " +
      "(((loxAfr2: 0.4529038785084617, proCap1: 0.9145780862566241): 0.13363439908347277, echTel1:" +
      " 1.551304656554472): 0.3417887266854045, (dasNov2: 0.6235277023103755, choHof1: 0.5869066815931655): " +
      "0.38164127191490144): 0.14964276371433335): 1.0931027723563065, monDom4: 2.013636981382706):" +
      " 0.4652194381498481, ornAna1: 1.6629134296227563): 0.378844191337151, ((galGal3: 0.5133370512232236, " +
      "taeGut1: 0.3602017398205815): 0.603529569954054, anoCar1: 2.161890981000896): 0.6733078376148556):" +
      " 0.4828035302815552, xenTro2: 1.8077664979778456): 0.6451353404523739, (((tetNig1: 0.646808851323313, " +
      "fr2: 0.48066361274119895): 0.722234607610907, (gasAcu1: 0.594246638318013, oryLat2: 0.9805733740636908):" +
      " 0.28248537330276025): 1.2290344415990553, danRer5: 2.695077539775618): 0.6693645816269147): " +
      "1.055766375210932, petMar1: 1.054436578486862);")
    val kiryupi = Array(0.26375328143435972,0.24618387560234775,0.2405650529813031,0.24949778998198935)
    val kiryub = Array(0.82267865072260782,2.466379816605059,0.59492428047577839,
      0.9198645987634092, 2.3705777570301034,0.84733371118152723)
    val kiryutree = ModelTree.fromString("(((((((((((((((((hg18:0.0038712478608118337,panTro2:0.0033966052743735404):" +
      "0.0017210352035714239,gorGor1:0.0060266906836859771):0.0058322567335449514,ponAbe2:0.015129768119812072):" +
      "0.0060639037747040603,rheMac2:0.024743050779338638):0.012549276255683937,calJac1:0.051986041038003909):" +
      "0.041714988362184839,tarSyr1:0.090153000685856646):0.0053774849720304779,(micMur1:0.050976778292494744,otoGar1:" +
      "0.084284893371154593):0.032591527057817989):0.012549797903102939,tupBel1:0.1347997501602681):" +
      "0.0044556506880791798,(((((mm9:0.04289269820066946,rn4:0.04428235960331154):0.12256198383697471,dipOrd1:" +
      "0.12825647405925045):0.028506031643481124,cavPor3:0.13036025367010298):0.01096647639595767," +
      "speTri1:0.12512509143973641):0.025415660232201591,(oryCun1:0.080591052167369254,ochPri2:0.15868436979707715):" +
      "0.075154645362831923):0.015965169794355485):0.025842517359318837,(((vicPac1:0.074378476109838249," +
      "(turTru1:0.039079912522048628,bosTau4:0.063798757996742686):0.011924693032982879):0.020527245375749133,(" +
      "(equCab2:0.06200106801168867,(felCat3:0.058364270924577709,canFam2:0.041795530183012873):0.032504008183400795):" +
      "0.0044453723485332027,(myoLuc1:0.1053422238004719,pteVam1:0.069081106022699679):0.025232593050103308):" +
      "0.00087071809890752446):0.0074765418645253287,(eriEur1:0.16156795259273227,sorAra1:0.16582363478718798):" +
      "0.071035030810358529):0.026642737991120314):0.013303253043544117,(((loxAfr2:0.056309291747186831,proCap1:" +
      "0.11385565988485748):0.01711790484159777,echTel1:0.19298772191700198):0.042348511015047739," +
      "(dasNov2:0.078223389095947171,choHof1:0.07296649353086819):0.047075224932835849):0.018399916677320698):" +
      "0.13489760510217469,monDom4:0.25123103679190079):0.046421613777038621,ornAna1:0.21941523559863568):" +
      "0.04774096433970608,((galGal3:0.063304131449663972,taeGut1:0.050889377551657955):0.080060726432756524,anoCar1:" +
      "0.24653932934574263):0.082806810923638369):0.048028118325479083,xenTro2:0.2408870729539368):" +
      "0.082440854640119798,(((tetNig1:0.089077352388325717,fr2:0.082292026704934393):0.080384089875649406," +
      "(gasAcu1:0.10099187462006658,oryLat2:0.13831767872222389):0.034971901279222346):0.10933745518692946," +
      "danRer5:0.25467296150162694):0.099036662827716518):0,petMar1:0.5616766454198765);")
    val mypi = myparam.pi
    val myb = myparam.Bvec
    def f(xs:Seq[Double],ys:Seq[Double]):Seq[Double] = (xs,ys).zipped.map((x,y) => x / y)

    f(kiryub,myb.toArray)
    f(kiryupi,mypi.toArray)
    val tmp = f(kiryutree.branches,mytree.branches)
    println(kiryutree.changeBranches(tmp.toList))
  }
}