package fdur

import java.io.PrintWriter

import breeze.linalg.DenseVector
import breeze.numerics.exp
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
      val hoge = cols.map(EM.estep(tree,_,model))
      val (lgl,br,pr) = EM.mstep(hoge,model,tree.branches)
      println(lgl)
      tree = tree.changeBranches(br)
      param = pr
    }
    println(param)
    println(tree)
  }*/
  def em(i : Int,nhf:String,maf:String):(Parameters,ModelRoot)={
    //val pi = Array(0.22, 0.28, 0.23, 0.27)
    //val b = Array(0.15, 0.25, 0.1, 0.13, 0.07, 0.3)
    val pi = Array(0.25469972246441502,0.2452686122063337,0.24531127848266232,0.25472038684658888)
    val b =	Array(0.81053441227174539,2.4236183781739533,0.65677131469221517,0.88544145555567511,2.4233580444379776,0.8106412600752263)
    var tree = ModelTree.fromFile(nhf)
    val cols = Maf.readMaf(maf,1000)//.toParArray
    var param = Parameters(DenseVector(b), DenseVector(pi))

    val lglw = new PrintWriter("target/log/ll.txt")
    val brw = new PrintWriter("target/log/br.txt")
    val bw = new PrintWriter("target/log/b.txt")
    val piw = new PrintWriter("target/log/pi.txt")

    for(_ <- 0 until i) printExeTime({
      val model = Model(param)
      val hoge = cols.map(EM.estep(tree,_,model)).toArray
      val (lgl,br,pr) = EM.mstep(hoge,model,tree.branches)
      lglw.println(lgl)
      brw.println(br.mkString(" "))
      bw.println(pr.Bvec.toArray.mkString(" "))
      piw.println(pr.pi.toArray.mkString(" "))
      tree = tree.changeBranches(br)
      param = pr
    },"rec ")
    lglw.close()
    brw.close()
    bw.close()
    piw.close()
    (param,tree)
  }
  val nh = "src/test/resources/fdur2/small.nh"
  val maf = "src/test/resources/fdur2/tmp.maf"
  /*test("small"){
    val (myparam,mytree) = em(1,nh,maf)
    println(myparam)
    println(mytree)
  }*/
  /*test("fdurResult1"){
    val myparam = Parameters(DenseVector(0.10521890348056277, 0.3086523214043375, 0.0725113057608077,
      0.12186528238712246, 0.2908629992154906, 0.1008891877516788),
      DenseVector(0.2609738851514271, 0.24388552796013088, 0.24376136221300196, 0.2513792246754401))
    val mypi = myparam.pi
    val myb = myparam.Bvec
    val mytree = ModelTree.fromString("(((((((((((((((((hg18:0.003916058819058167,panTro2:0.003500280587640854):0.0017040393644844804,gorGor1:0.00649665492655424):0.0058846034345096795,ponAbe2:0.015060255144664449):0.00711831186742191,rheMac2:0.024849207005034617):0.012433557469725711,calJac1:0.0512514909850635):0.03902191698712884,tarSyr1:0.08713794350359963):0.0077298379263051765,(micMur1:0.05256868277777528,otoGar1:0.08030437783137981):0.030215554342618734):0.011414134553634608,tupBel1:0.1261508616987269):0.005868233684437402,(((((mm9:0.04284477681106022,rn4:0.0449067458371167):0.1137055361699873,dipOrd1:0.12390795750687952):0.02683594796385629,cavPor3:0.12684739806124612):0.014065112563538658,speTri1:0.11534614431195536):0.02292530565333663,(oryCun1:0.07304225504440755,ochPri2:0.13713100626427419):0.07432566459510628):0.01969986233369001):0.022447384866855224,(((vicPac1:0.06802118654696646,(turTru1:0.03853831224670974,bosTau4:0.06386806411095287):0.012955240566717218):0.020963548332629436,((equCab2:0.061487070744227754,(felCat3:0.055385740176103426,canFam2:0.04659907574999758):0.029309769869149533):0.005036208190426941,(myoLuc1:0.09545889259047671,pteVam1:0.06832604065575747):0.024700639225106468):0.0023124060466021166):0.009329597437716115,(eriEur1:0.14622341359914415,sorAra1:0.14217844642348165):0.05770539800029966):0.02462918165652704):0.014215940246355701,(((loxAfr2:0.054054295545546735,proCap1:0.10543682264478002):0.018598345260193622,echTel1:0.1622122747811263):0.04091905344353778,(dasNov2:0.07745004318004843,choHof1:0.06575894864447958):0.039996198508722554):0.01865447240887157):0.10156985109978192,monDom4:0.21333288909906403):0.034449940388498704,ornAna1:0.2124879046552759):0.04247322132152122,((galGal3:0.06855849130808137,taeGut1:0.06566030499065942):0.07845607920037516,anoCar1:0.1911886481510689):0.07421635859815118):0.04540052466265562,xenTro2:0.2479194953786065):0.07840680707637536,(((tetNig1:0.09814629880659151,fr2:0.09811625207406172):0.07887748611282418,(gasAcu1:0.11863592894826928,oryLat2:0.1513286709454074):0.034955125632821375):0.09353626371264721,danRer5:0.21539986884785473):0.09268493017247237):0.2973584659631462,petMar1:0.29671088451282546);")
    val kiryupi = Array(0.2530262388001624,0.25638850344066322,0.24918817608175356,0.24139708167742066)
    val kiryub = Array(0.71441133102484211,2.199050578991844,1.0600945410713229,0.64169502640921217,2.4919482296727868,0.91472062223548334)
    val kiryutree = ModelTree.fromString("(((((((((((((((((hg18:0.0049970763097711198,panTro2:0.0054869744975477672):0.0018042881576319065,gorGor1:0.0076458567655762677):0.0063610037660841142,ponAbe2:0.014157914680421704):0.0087919524066004066,rheMac2:0.025409129891251041):0.012394685719957638,calJac1:0.046716178229180977):0.033383424554035056,tarSyr1:0.084465754902310564):0.0075785328402433825,(micMur1:0.049804775333429106,otoGar1:0.075749474745297793):0.026392547100140076):0.00866507969709953,tupBel1:0.10887472544059644):0.00487015337719446,(((((mm9:0.040345705930923847,rn4:0.043407616095398657):0.09134875667448758,dipOrd1:0.10707615470916683):0.021472843598207961,cavPor3:0.11647192647316176):0.013347920934635332,speTri1:0.092806267203129067):0.018201773838438549,(oryCun1:0.062744145787683989,ochPri2:0.097836121120326719):0.057695344654670584):0.018064652452388117):0.01289717979874536,(((vicPac1:0.061793712267688004,(turTru1:0.037197795089941907,bosTau4:0.064085894357475609):0.014814785254323642):0.022891392990246423,((equCab2:0.059163631962912425,(felCat3:0.053484056395732685,canFam2:0.055888758492430211):0.029958153746091971):0.0058059166669228402,(myoLuc1:0.074298741574217647,pteVam1:0.061614325905777638):0.021090803701371882):0.003700930390014416):0.0078079519669747551,(eriEur1:0.10990192983858506,sorAra1:0.11981833009432183):0.0388531397791715):0.016811514914135291):0.010649676031895981,(((loxAfr2:0.047679493503368239,proCap1:0.079753668985989401):0.014914819373961712,echTel1:0.117506283669783):0.031396852331993859,(dasNov2:0.067684161807279236,choHof1:0.058185844750800748):0.031135553621156793):0.014176851419943647):0.075209253344754604,monDom4:0.17763859567411819):0.027819851675093168,ornAna1:0.18959909881134115):0.039344221387814148,((galGal3:0.070086966341060694,taeGut1:0.070622411795591186):0.075953516432265542,anoCar1:0.17153553245361131):0.06917356208524042):0.044481187335608914,xenTro2:0.2488831741052383):0.076899363757513814,(((tetNig1:0.10017908484429125,fr2:0.10166395483602754):0.078569514185763328,(gasAcu1:0.12233812007080486,oryLat2:0.15413975657989837):0.034918309634882204):0.089984904832019569,danRer5:0.20684476567165336):0.09063972339049664):0.3,petMar1:0.29935029206746866);")
    def f(xs:Seq[Double],ys:Seq[Double]):Seq[Double] = (xs,ys).zipped.map((x,y) => x / y)
    //println(kiryutree.leafLength)
    //println(f(kiryub,myb.toArray))
    //println(f(kiryupi,mypi.toArray))
    val tmp = f(kiryutree.branches,mytree.branches)
    println(kiryutree.changeBranches(tmp.toList).leafList.map(_.t))
  }*/
  /*test("fdurResult1"){
    val (myparam,mytree) = em(1)
    val mypi = myparam.pi
    val myb = myparam.Bvec
    val kiryupi = Array(0.2530262388001624,0.25638850344066322,0.24918817608175356,0.24139708167742066)
    val kiryub = Array(0.71441133102484211,2.199050578991844,1.0600945410713229,0.64169502640921217,2.4919482296727868,0.91472062223548334)
    val kiryutree = ModelTree.fromString("(((((((((((((((((hg18:0.0049970763097711198,panTro2:0.0054869744975477672):0.0018042881576319065,gorGor1:0.0076458567655762677):0.0063610037660841142,ponAbe2:0.014157914680421704):0.0087919524066004066,rheMac2:0.025409129891251041):0.012394685719957638,calJac1:0.046716178229180977):0.033383424554035056,tarSyr1:0.084465754902310564):0.0075785328402433825,(micMur1:0.049804775333429106,otoGar1:0.075749474745297793):0.026392547100140076):0.00866507969709953,tupBel1:0.10887472544059644):0.00487015337719446,(((((mm9:0.040345705930923847,rn4:0.043407616095398657):0.09134875667448758,dipOrd1:0.10707615470916683):0.021472843598207961,cavPor3:0.11647192647316176):0.013347920934635332,speTri1:0.092806267203129067):0.018201773838438549,(oryCun1:0.062744145787683989,ochPri2:0.097836121120326719):0.057695344654670584):0.018064652452388117):0.01289717979874536,(((vicPac1:0.061793712267688004,(turTru1:0.037197795089941907,bosTau4:0.064085894357475609):0.014814785254323642):0.022891392990246423,((equCab2:0.059163631962912425,(felCat3:0.053484056395732685,canFam2:0.055888758492430211):0.029958153746091971):0.0058059166669228402,(myoLuc1:0.074298741574217647,pteVam1:0.061614325905777638):0.021090803701371882):0.003700930390014416):0.0078079519669747551,(eriEur1:0.10990192983858506,sorAra1:0.11981833009432183):0.0388531397791715):0.016811514914135291):0.010649676031895981,(((loxAfr2:0.047679493503368239,proCap1:0.079753668985989401):0.014914819373961712,echTel1:0.117506283669783):0.031396852331993859,(dasNov2:0.067684161807279236,choHof1:0.058185844750800748):0.031135553621156793):0.014176851419943647):0.075209253344754604,monDom4:0.17763859567411819):0.027819851675093168,ornAna1:0.18959909881134115):0.039344221387814148,((galGal3:0.070086966341060694,taeGut1:0.070622411795591186):0.075953516432265542,anoCar1:0.17153553245361131):0.06917356208524042):0.044481187335608914,xenTro2:0.2488831741052383):0.076899363757513814,(((tetNig1:0.10017908484429125,fr2:0.10166395483602754):0.078569514185763328,(gasAcu1:0.12233812007080486,oryLat2:0.15413975657989837):0.034918309634882204):0.089984904832019569,danRer5:0.20684476567165336):0.09063972339049664):0.3,petMar1:0.29935029206746866);")
    def f(xs:Seq[Double],ys:Seq[Double]):Seq[Double] = (xs,ys).zipped.map((x,y) => x / y)
    println(f(kiryub,myb.toArray))
    println(f(kiryupi,mypi.toArray))
    println(f(kiryutree.branches,mytree.branches))
    //val tmp = f(kiryutree.branches,mytree.branches)
    //println(kiryutree.changeBranches(tmp.toList).leafList.map(_.t))
  }*/
  /*test("fdurResult") {
    val (myparam,mytree) = em(100,"src/test/resources/fdur2/hoge.txt","src/test/resources/fdur2/hoge.maf")
    //val kiryupi = Array(0.26375328143435972,0.24618387560234775,0.2405650529813031,0.24949778998198935)
    val kiryupi = Array(0.26515358196840971,0.24912782250152404,0.23943170926734045,0.24628688626272591)
    val kiryub = Array(0.79921966694319446,2.5198921296626606,0.59491464482039957,0.86606680133162484,2.4225727926754299,0.81527070154074543)
    val kiryutree = ModelTree.fromString("((((((((((((hg18:0.0038312602935637137,panTro2:0.0034304423390026473):0.0017179265224519583,gorGor1:0.0060161083838409677):0.0058249112477565847,ponAbe2:0.015118995781854362):0.0060766366759448581,rheMac2:0.02470688986688499):0.01257820997064105,calJac1:0.051898135467001394):0.041487779456310027,tarSyr1:0.089817944228418406):0.0056795515757960555,(micMur1:0.050935309742475288,otoGar1:0.083892833067079009):0.032484669158902892):0.012201357183470703,tupBel1:0.13498680874946947):0.0047013532640501534,(((((mm9:0.042838327445794307,rn4:0.04422417191042595):0.12250580984566982,dipOrd1:0.12827969673285997):0.028568752458573401,cavPor3:0.13018317906287302):0.011194082533324488,speTri1:0.1248264613558127):0.025145678077410267,(oryCun1:0.080263663315010059,ochPri2:0.15848258400709128):0.075713635038925031):0.016143377938733019):0.026733325664735706,(((vicPac1:0.074205507048967015,(turTru1:0.039036625482696902,bosTau4:0.063774431675955534):0.011929769556970518):0.020533742189736543,((equCab2:0.061955217230038981,(felCat3:0.058077265320629221,canFam2:0.041935646639838203):0.032440061786310564):0.0044649241286606424,(myoLuc1:0.10529686876172467,pteVam1:0.069066847204364568):0.025165562070408282):0.00087986838140759584):0.0070928463146936252,(eriEur1:0.16156932751188008,sorAra1:0.16482376463395373):0.071288705107334496):0.026167119642109368):0.01209896076089279,(((loxAfr2:0.056116708381400829,proCap1:0.11354570028664024):0.017351398853788769,echTel1:0.19304733586732334):0.042420619756360071,(dasNov2:0.078605777412030328,choHof1:0.072254497155227179):0.047318934753444758):0.018745580830593477):0,monDom4:0.37067250235428612);")
    //val kiryub = Array(0.82267865072260782,2.466379816605059,0.59492428047577839,
   //   0.9198645987634092, 2.3705777570301034,0.84733371118152723)
   /*val kiryutree = ModelTree.fromString("(((((((((((((((((hg18:0.0038712478608118337,panTro2:0.0033966052743735404):" +
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
      */
    //println(kiryutree.branches)
    val mypi = myparam.pi
    val myb = myparam.Bvec
    def f(xs:Seq[Double],ys:Seq[Double]):Seq[Double] = (xs,ys).zipped.map((x,y) => x / y)
    println(f(kiryub,myb.toArray))
    println(f(kiryupi,mypi.toArray))
    println(f(kiryutree.branches,mytree.branches))
    //println(kiryutree.changeBranches(tmp.toList).leafList.map(_.t))

    //val mybranches = "0.03105006227299572 0.027248909045965004 0.01380114389979824 0.04833980331380212 0.04678705071574641 0.12137233174967278 0.04854632822425685 0.1985317761805513 0.10077734546424315 0.4169666653232112 0.3388774324086247 0.7260052410756739 0.037867398412550414 0.40770570714811377 0.6767749127498849 0.26303092782727366 0.10292198730526808 1.0824473267330808 0.03327331417203134 0.34400437356599234 0.35535353821104143 0.9837933937169284 1.0288442226288987 0.23019157423098663 1.047760417087127 0.08409236682344379 1.0045137401988338 0.20676576664364743 0.6502678459193445 1.2782111298879526 0.5971805896971526 0.1268441753348832 0.20799471183427545 0.5976158567950056 0.31353575184750465 0.5117870390637704 0.09550049714620118 0.16504080416893036 0.4973779139632257 0.4694819640492678 0.3340088916563189 0.26204708398060383 0.03587924466514549 0.8451508647693822 0.5542767245275344 0.2025997784359965 0.006157022662916939 0.05887694454366641 1.2944673043123138 1.3399609633722958 0.5716515783734215 0.21497640146395394 0.10304189021077058 0.4529039225147527 0.9145780250519207 0.1336344504463086 1.5513045343324219 0.34178884514139357 0.623527677328954 0.5869067100134094 0.38164107626582877 0.1496427680436985 1.0931092770065685 2.013632274645472 0.4652023849401676 1.662919856742366 0.37882904099498144 0.5133363030607303 0.3602033009431814 0.6035411802514185 2.1618796363929644 0.6733190071805922 0.4830643833011574 1.8075357459827255 0.6442517905504828 0.646820541553035 0.4806486485364324 0.7222218129068307 0.5942305472452779 0.980558423626165 0.2824988886967049 1.2281477158034757 2.6943172709029524 0.6714587748987874 1.0536421014893573 1.0522156700930443"
    //println(mybranches.split(" ").map(_.toDouble).toList)
    //println(kiryutree.branches)
  }*/
}
