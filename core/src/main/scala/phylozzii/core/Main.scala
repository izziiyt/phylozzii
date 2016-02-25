package phylozzii.core

import java.io._
import phylozzii.fdur.SGEFdur
import phylozzii.pbls.BLSer
import scopt.OptionParser

object Main extends App{
  import Others._
  import SparkFdur._
  import SGEFdur._
  import BLSer._

  val parser = new OptionParser[Conf]("phylozzii") {
    head("phylozzii", "0.2.0-SNAP_SHOT")
    help("help") abbr "h" text "prints this usage text"
    version("version") abbr "v" text "prints version information"
    opt[Unit]("yaml") abbr "yml" valueName "<file>" text "pending"

    cmd("pbls") action { (_, c) => c.copy(mode = "pbls") } text
      "calculates probablistic branch length scores on a phylogenetic tree" children(
      opt[File]("change-name") abbr "cn" valueName "<file>" optional() action {
        (x, c) => c.copy(nameNH = x)
      } text "to change scientific names to common names, and vice versa",
      newick,
      opt[File]('o', "out") abbr "o" required() valueName "<file>" action {
        (x, c) => c.copy(out = x)
      } text "a directory to put output files",
      param,
      opt[String]('t', "target") required() valueName "<string>" action {
        (x, c) => c.copy(target = x)
      } text "target species",
      maf
      )

    cmd("fdur") action { (_, c) => c.copy(mode = "fdur") } text
      "estimates parameters on a phylogenetic tree by EM maximum likelihood estimation" children(
      cbf,
      opt[Int]("maxit") abbr "mi" valueName "<integer>" action {
        (x, c) => c.copy(maxit = x)
      } text "how many iterates at most",
      newick,
      param,
      opt[Int]('P', "partition") valueName "<integer>" action {
        (x, c) => c.copy(partition = x)
      } text "data partition size",
      opt[Unit]('s', "spark") required() action {
        (_, c) => c.copy(spark = true)
      } text "flag to use spark",
      maf
      )

    cmd("wighist") action { (_, c) => c.copy(mode = "wighist") } text
      "filters .wig file on the basis of information of .bed" children(
      opt[File]('w',"wig") required() valueName "<file>" action {
        (x, c) => c.copy(input1 = x)
      } text "input .wig file",
      opt[File]('b', "bed") optional() valueName "<file>" action {
        (x, c) => c.copy(input2 = x)
      } text ".bed file",
      opt[String]('c', "chr") required() valueName "<string>" action {
        (x, c) => c.copy(opt1 = x)
      } text "required chromosome name",
      opt[File]('o', "out") optional() valueName "<file>" action {
        (x, c) => c.copy(out = x)
      } text s"output files, default is stdout"
      )

    cmd("counter") action { (_, c) => c.copy(mode = "ct") } text
      "counts # of [A|C|G|T] contained in input .maf file" children (
      opt[File]('o',"out") optional() valueName "<file>" action {
        (x, c) => c.copy(out = x)
      } text "output file",
      arg[File]("<file>") required() action { (x, c) =>
        c.copy(input1 = x)
      } text "input .maf file"
      )

    cmd("entrop") action { (_, c) => c.copy(mode = "en") } text
      "calculates entropies on alignmented DNA sequences" children (
      arg[File]("<file>") required() action { (x, c) =>
        c.copy(input1 = x)
      } text "input .aln file"
      )

    cmd("bedchrsplit") action { (_, c) => c.copy(mode = "bedsplit") } text
      "split bed file by chromosome" children (
      opt[File]('o',"out") optional() valueName "<directory>" action {
        (x, c) => c.copy(out = x)
      } text "output directory",
      arg[File]("<file>") required() action { (x, c) =>
        c.copy(input1 = x)
      } text "input .bed file"
      )

    cmd("sge-fdur-estep") abbr "sfe" action { (_, c) => c.copy(mode = "qe") } text
      "phylozzii.fdur's estep for runnig on SGE system" children (
      cbf,
      newick,
      param,
      arg[File]("<file>") required() action { (x, c) =>
        c.copy(aln = x)
      } text "input directory"
      )

    cmd("wigwig") action {(_,c) => c.copy(mode = "wigwig")} text
      "puts values in intersections of two input .wig files" children (
      opt[File]('o',"out") optional() valueName "<file>" action {
        (x, c) => c.copy(out = x)
      } text "output file",
      arg[File]("<file>") required() action { (x, c) =>
        c.copy(input1 = x)
      } text "first input .wig file",
      arg[File]("<file>") required() action { (x, c) =>
        c.copy(input2 = x)
      } text "second input .wig file"
      )

    def cbf = opt[Unit]("const-base-frequent") abbr "cbf" action {
      (_, c) => c.copy(constFreq = true)
    } text "how many iterates at most"

    def maf = arg[File]("<file>") required() action { (x, c) =>
      c.copy(maf = x)
    } text "input .maf file"

    def newick = opt[File]("newick") abbr "nh" required() valueName "<file>" action {
      (x, c) =>
        c.copy(nh = x)
    } text "required newick format file"

    def param = opt[File]('p', "param") required() valueName "<file>" action {
      (x, c) => c.copy(param = x)
    } text "file parameter written"

  }

  parser.parse(args, Conf()) match {
    case Some(conf) =>
      conf.mode match {
        case "fdur" =>
          sparkem(conf.maf, conf.param, conf.nh, conf.maxit, conf.partition, conf.constFreq)
        case "phylozzii.pbls/src/main/scala/phylozzii.pbls" =>
          val prefix = conf.maf.getName.split('.').head
          val bls = new File(conf.out.getPath + "/" + prefix + ".bls.wig.gz")
          val blsa = new File(conf.out.getPath + "/" + prefix + ".blsa.wig.gz")
          blser(conf.target, conf.maf, conf.nh, conf.param, bls, blsa, conf.nameNH)
        case "wighist" =>
          val ws = biformat.bigSource(conf.input1)
          val bs = if(conf.input2.isFile) Some(biformat.bigSource(conf.input2)) else None
          conf.out.createNewFile()
          val os = if(conf.out.isFile) f2stream(conf.out) else System.out
          try wighist(ws, bs, conf.opt1, os)
          finally {
            ws.close()
            bs foreach (_.close())
            if(conf.out.isFile) os.close()
          }
        case "wigwig" =>
          val ws1 = biformat.bigSource(conf.input1)
          val ws2 = biformat.bigSource(conf.input2)
          conf.out.createNewFile()
          val os = if(conf.out.isFile) f2stream(conf.out) else System.out
          try wigwigphyloP(ws1, ws2, os)
          finally {
            ws1.close()
            ws2.close()
            if(conf.out.isFile) os.close()
          }
        case "ct" =>
          conf.out.createNewFile()
          val os = if(conf.out.isFile) new PrintStream(conf.out) else System.out
          val ms = biformat.bigSource(conf.input1)
          try counter(ms, os)
          finally {
            ms.close()
            if(conf.out.isFile) os.close()
          }
        case "en" =>
          val as = biformat.bigSource(conf.input1)
          val name = conf.input1.getName.split('.').head
          try entrop(as, name)
          finally as.close()
        case "qe" =>
          qestep(conf.maf, conf.nh, conf.param, conf.out)
        case "qm" =>
          qmstep(conf.targetDir, conf.param, conf.nh, conf.constFreq)
        case "bedsplit" =>
          val bedSource = biformat.bigSource(conf.input1)
          bedChrSplit(bedSource, conf.out)
        case _ =>
          throw new UnsupportedOperationException
      }
    case None => Unit
  }

  def f2stream(f: File): PrintStream =
    new PrintStream(new BufferedOutputStream(new FileOutputStream(f), 1024 * 1024))
}

case class Conf(mode: String = "", maf: File = new File("."), tgtDir: File = new File("."),
                param: File = new File("."), targetDir: File = new File("."), target: String = "",
                isSciName: Boolean = false, prefix: String = "", sp: Boolean = false, nh: File = new File(""),
                maxit: Int = 200, partition: Int = 512, constFreq: Boolean = false, nameNH: File = new File("."),
                bed: File = new File("."), wig: File = new File("."), aln: File = new File("."),
                spark: Boolean = false, out: File = new File("."), input1: File = new File("."), input2: File = new File("."), opt1: String = "")

