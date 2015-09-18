package fdur

import breeze.linalg._
import breeze.numerics.{exp,pow}
import breeze.optimize.{DiffFunction, LBFGS}

import org.scalatest.FunSuite
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.ArrayBuffer
import breeze.plot._

class BFGSTest extends FunSuite with LazyLogging {
  val nh = "src/test/resources/fdur/fdur.nh"
  val maf = "src/test/resources/fdur/fdur.maf"
  /*val pi = DenseVector(0.25469972246441502, 0.2452686122063337, 0.24531127848266232, 0.25472038684658888)
  val b = DenseVector(0.81053441227174539, 2.4236183781739533, 0.65677131469221517,
                      0.88544145555567511, 2.4233580444379776, 0.8106412600752263)
*/
  val pi = DenseVector(0.23137857635453807, 0.28408070157281884, 0.27729375318455474, 0.20724696888808836)
  val b = DenseVector(0.6586096484894902, 2.329377965568423, 0.8207872557873153, 0.9101830004835019, 2.7967009808428305, 0.5488972207907554)
  /*test("graddescent") {
    val (brnch,param) = Optimizer.gd(1000,nh,maf,pi,b)
    println(param)
    println(brnch)
    val m = Model(param)
    println(m.R)
  }*/

  test("EM") {
    val (brnch,param) = Optimizer.em(200,nh,maf,pi,b)
    println(param)
    println(brnch)
    val m = Model(param)
    println(m.R)
  }

  test("fdur.nh && fdur.maf"){
    val phyloFitpi = DenseVector(0.237118, 0.277034, 0.271539, 0.214309)
    val phyloFitlgl = -149597.338007
    val phyloFitTree = ModelTree.fromString("(((((((((((((((((hg18:0.00390535,panTro2:0.00342466):0.00174157,gorGor1:0.00608027):0.00589362,ponAbe2:0.0152518):0.00614714,rheMac2:0.0249772):0.0126814,calJac1:0.0524466):0.0427622,tarSyr1:0.0907546):0.00511376,(micMur1:0.0511858,otoGar1:0.084799):0.0331766):0.0129697,tupBel1:0.136038):0.00441703,(((((mm9:0.0428934,rn4:0.0444941):0.12316,dipOrd1:0.128492):0.0294094,cavPor3:0.131532):0.0109285,speTri1:0.124952):0.0263487,(oryCun1:0.0822679,ochPri2:0.163033):0.075949):0.0168081):0.0270982,(((vicPac1:0.0750045,(turTru1:0.0396072,bosTau4:0.064478):0.0120592):0.0212053,((equCab2:0.0629226,(felCat3:0.0586419,canFam2:0.0421736):0.0332806):0.00474687,(myoLuc1:0.106576,pteVam1:0.069563):0.025541):0.000818854):0.00742878,(eriEur1:0.161862,sorAra1:0.167363):0.0727508):0.0274296):0.0127911,(((loxAfr2:0.0568017,proCap1:0.115403):0.0165458,echTel1:0.195213):0.0441114,(dasNov2:0.0793843,choHof1:0.0735249):0.049083):0.0200907):0.144024,monDom4:0.250851):0.0607343,ornAna1:0.209931):0.0497305,((galGal3:0.067314,taeGut1:0.0411634):0.0735401,anoCar1:0.269246):0.0819697):0.0618427,xenTro2:0.220942):0.0786431,(((tetNig1:0.0831341,fr2:0.0585228):0.0908991,(gasAcu1:0.0744971,oryLat2:0.123791):0.0361884):0.16552,danRer5:0.322268):0.0784432):0.0,petMar1:0.308722);")
    val phyloFitbrnch = phyloFitTree.branches
    val phyloR = DenseMatrix((-1.037747,    0.226569,    0.666483,    0.144695),
      (0.193924,   -0.927837,    0.214848,    0.519065),
      (0.581998,    0.219196,   -0.987587,    0.186393),
      (0.160094,    0.670986,    0.236168,   -1.067248) )
    val krypi = DenseVector(0.26375328143435972,0.24618387560234775,0.2405650529813031,0.24949778998198935)
    val kryb = DenseVector(0.82267865072260782,2.466379816605059,0.59492428047577839,0.9198645987634092,2.3705777570301034,0.84733371118152723)
    val krytree = ModelTree.fromString("(((((((((((((((((hg18:0.0038712478608118337,panTro2:0.0033966052743735404):0.0017210352035714239,gorGor1:0.0060266906836859771):0.0058322567335449514,ponAbe2:0.015129768119812072):0.0060639037747040603,rheMac2:0.024743050779338638):0.012549276255683937,calJac1:0.051986041038003909):0.041714988362184839,tarSyr1:0.090153000685856646):0.0053774849720304779,(micMur1:0.050976778292494744,otoGar1:0.084284893371154593):0.032591527057817989):0.012549797903102939,tupBel1:0.1347997501602681):0.0044556506880791798,(((((mm9:0.04289269820066946,rn4:0.04428235960331154):0.12256198383697471,dipOrd1:0.12825647405925045):0.028506031643481124,cavPor3:0.13036025367010298):0.01096647639595767,speTri1:0.12512509143973641):0.025415660232201591,(oryCun1:0.080591052167369254,ochPri2:0.15868436979707715):0.075154645362831923):0.015965169794355485):0.025842517359318837,(((vicPac1:0.074378476109838249,(turTru1:0.039079912522048628,bosTau4:0.063798757996742686):0.011924693032982879):0.020527245375749133,((equCab2:0.06200106801168867,(felCat3:0.058364270924577709,canFam2:0.041795530183012873):0.032504008183400795):0.0044453723485332027,(myoLuc1:0.1053422238004719,pteVam1:0.069081106022699679):0.025232593050103308):0.00087071809890752446):0.0074765418645253287,(eriEur1:0.16156795259273227,sorAra1:0.16582363478718798):0.071035030810358529):0.026642737991120314):0.013303253043544117,(((loxAfr2:0.056309291747186831,proCap1:0.11385565988485748):0.01711790484159777,echTel1:0.19298772191700198):0.042348511015047739,(dasNov2:0.078223389095947171,choHof1:0.07296649353086819):0.047075224932835849):0.018399916677320698):0.13489760510217469,monDom4:0.25123103679190079):0.046421613777038621,ornAna1:0.21941523559863568):0.04774096433970608,((galGal3:0.063304131449663972,taeGut1:0.050889377551657955):0.080060726432756524,anoCar1:0.24653932934574263):0.082806810923638369):0.048028118325479083,xenTro2:0.2408870729539368):0.082440854640119798,(((tetNig1:0.089077352388325717,fr2:0.082292026704934393):0.080384089875649406,(gasAcu1:0.10099187462006658,oryLat2:0.13831767872222389):0.034971901279222346):0.10933745518692946,danRer5:0.25467296150162694):0.099036662827716518):0,petMar1:0.5616766454198765);")
    val krybrnch = krytree.branches
    val kryR = Model(Parameters(kryb,krypi)).R
    val mygdbrnch = List(0.00387009042456805, 0.0033961819651761693, 0.0017204385672641136, 0.006025061083139143, 0.005831535229121633, 0.015127794904954046, 0.006049604607055687, 0.024744202697539425, 0.012564962975304537, 0.0519696787735972, 0.04223775564775963, 0.09048844042878687, 0.004719707904828472, 0.0508155988357742, 0.0843522441099805, 0.0327848006952343, 0.012825349672109385, 0.1349155804612692, 0.004148482923001319, 0.042876533037314, 0.044290100413906276, 0.12261868400616667, 0.12823358150431838, 0.028691004876149943, 0.13059152488114056, 0.010482926772798957, 0.1252008850440978, 0.025770961022354412, 0.08104519042307036, 0.1593096908323115, 0.07443066364300147, 0.01580997830503201, 0.02592487907478772, 0.07448581984223075, 0.039079721114259884, 0.06378594074773543, 0.011902743805952486, 0.020571284373294585, 0.061993002528670245, 0.05851429009731741, 0.04162998340429954, 0.03266164033874709, 0.004471398363061445, 0.10533908404678537, 0.06908775309075157, 0.025251955031092944, 7.669167998627108E-4, 0.007337649015471243, 0.16133867304341995, 0.16701132783714218, 0.07125313812155118, 0.026794680225257054, 0.01283854553789949, 0.05644883401971762, 0.11399274372143715, 0.016654429554428488, 0.1933412424717179, 0.04260357628878169, 0.07772086400815126, 0.07315286037494281, 0.047575868114551056, 0.018656703674376905, 0.13622004276902008, 0.2509939433829721, 0.05802207836372119, 0.2072836475987321, 0.04736095180521815, 0.06398548000804873, 0.044890664487970085, 0.07518024850650384, 0.26950736423544974, 0.08382362417871775, 0.0592238553397513, 0.2261449695816086, 0.07743281376824776, 0.08064119300737563, 0.059852739016512, 0.09063408018657536, 0.07419521943285307, 0.12244752552033736, 0.03439406023281181, 0.15958225543175014, 0.33994801545269376, 0.07826275465367918, 0.0, 0.2922979090035733)
    val mygdpi = DenseVector(0.26380683911679587, 0.24640432955941052, 0.23999616555768957, 0.2497926657661041)
    val mygdb = DenseVector(0.8218480994470997, 2.4734862876176154, 0.5886328833574198, 0.9203858222621918, 2.369021383304364, 0.849783999333609)
    val mygdlgl = -149323.0
    val mygdR = Model(Parameters(mygdb,mygdpi)).R
    val x = {
      val f = Figure()
      val p = f.subplot(0)
      val x = DenseVector(Array.range(0, phyloFitbrnch.length).map(_.toDouble))
      p += plot(x, DenseVector(phyloFitbrnch.toArray), '-')
      p += plot(x, DenseVector(krybrnch.toArray), '.')
      p += plot(x, DenseVector(mygdbrnch.toArray), '+')
      p.xlabel = "branch index"
      p.ylabel = "value"
      f.saveas("target/brnch.png")
    }
    val y = {
      val f = Figure()
      val p = f.subplot(0)
      val x = DenseVector(Array.range(0, 16).map(_.toDouble))
      println(phyloR.toDenseVector)
      println(phyloR.toDenseVector.length)
      p += plot(x, phyloR.toDenseVector, '-')
      p += plot(x, kryR.toDenseVector, '.')
      p += plot(x, mygdR.toDenseVector, '+')
      p.xlabel = "R matrix index"
      p.ylabel = "value"
      f.saveas("target/R.png")
    }
  }
/*  class ParamLog(val lgl:ArrayBuffer[Double],val b:Array[ArrayBuffer[Double]],
                 val pi:Array[ArrayBuffer[Double]],val brnch:Array[ArrayBuffer[Double]]){
    protected def append(lglx:Double,bx:VD,pix:VD,brnchx:Array[Double]){
      lgl += lglx
      (b,bx.toArray).zipped.foreach((x,y) => x += y)
      (pi,pix.toArray).zipped.foreach((x,y) => x += y)
      (brnch,brnchx).zipped.foreach((x,y) => x += y)
    }

    def append(lglx:Double,p:VD): Unit ={
      val param = Parameters(p(0 to 9))
      append(lglx, param.Bvec, param.pi, p(10 until p.length).toArray)
    }

    def show = {
      val n = lgl.length
      require(n == pi.head.length)
      require(b.head.length == n)
      require(n == brnch.head.length)
      innerShow(pi,"target/pi.png","iteration","pi",n)
      innerShow(b,"target/b.png","iteration","b",n)
      innerShow(brnch,"target/brnch.png","iteration","branch_length",n)
      lglshow
    }

    protected def lglshow = {
      val f = Figure()
      val p = f.subplot(0)
      val x = DenseVector(Array.range(0,lgl.length).map(_.toDouble))
      p += plot(x, DenseVector(lgl.toArray))
      p.xlabel = "iteration"
      p.ylabel = "log_likelihood"
      f.saveas("target/lgl.png")
    }

    protected def innerShow(xs:Array[ArrayBuffer[Double]],out:String,xlab:String,ylab:String,n:Int) = {
      val f = Figure()
      val p = f.subplot(0)
      val x = DenseVector(Array.range(0,n).map(_.toDouble))
      for(i <- xs.indices){p += plot(x, DenseVector(xs(i).toArray))}
      p.xlabel = xlab
      p.ylabel = ylab
      f.saveas(out)
    }
  }

  object ParamLog{
    def apply(n:Int) = {
      new ParamLog(ArrayBuffer[Double](),
        Array.fill(6)(ArrayBuffer[Double]()),
        Array.fill(4)(ArrayBuffer[Double]()),
        Array.fill(n)(ArrayBuffer[Double]()))
    }
  }
  */
}

