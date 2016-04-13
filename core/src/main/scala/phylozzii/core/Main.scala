package phylozzii.core

import java.io._

import phylozzii.pbls.BLSer
import scopt.OptionParser

object Main extends App{
  import BLSer._
  import Others._
  import SparkFdur._

  val parser = new OptionParser[Config]("phylozzii") {
    head("phylozzii", "0.2.0-SNAP_SHOT")
    help("help") abbr "h" text "prints this usage text"
    version("version") abbr "v" text "prints version information"
    opt[Unit]("yaml") abbr "yml" valueName "<file>" text "pending"

    cmd("ebls") action { (_, c) => c.copy(mode = "ebls") } text
      "calculates expected branch length scores on a phylogenetic tree" children(
      opt[File]('c',"change-name") valueName "<file>" optional() action {
        (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
      } text "to change scientific names to common names, and vice versa",
      opt[String]('t', "target") required() valueName "<string>" action {
        (x, c) => c.copy(stringArgs = c.stringArgs :+ x)
      } text "target species",
      opt[File]('o', "out") optional() valueName "<file>" action {
        (x, c) => c.copy(out = x)
      } text "a directory to put output files, default is .",
      maf,
      newick,
      param
      )

    cmd("fdur") action { (_, c) => c.copy(mode = "fdur") } text
      "estimates parameters on a phylogenetic tree by EM maximum likelihood estimation" children(
      cbf,
      opt[Int]("maxit") abbr "mi" valueName "<integer>" action {
        (x, c) => c.copy(optionalIntegers = c.optionalIntegers :+ x)
      } text "how many iterates at most",
      opt[Int]('P', "partition") valueName "<integer>" action {
        (x, c) => c.copy(optionalIntegers = c.optionalIntegers :+ x)
      } text "data partition size",
      opt[Unit]('s', "spark") required() action {
        (_, c) => c.copy(spark = true)
      } text "flag to use on spark",
      maf,
      newick,
      param
      )

    cmd("wighist") action { (_, c) => c.copy(mode = "wighist") } text
      "filters .wig file on the basis of information of .bed" children(
      opt[String]('c', "chr") required() valueName "<string>" action {
        (x, c) => c.copy(stringArgs = c.stringArgs :+ x)
      } text "required chromosome name",
      opt[File]('b', "bed") required() valueName "<string>" action {
        (x, c) => c.copy(optionalFiles = c.optionalFiles :+ x)
      } text ".bed file used for filtering",
      out,
      wig
      )

    cmd("counter") action { (_, c) => c.copy(mode = "counter") } text
      "counts # of [A|C|G|T] contained in input .maf file" children (
      out,
      maf
      )

    cmd("entrop") action { (_, c) => c.copy(mode = "en") } text
      "calculates entropies on alignmented DNA sequences" children (
      arg[File]("<file>") required() action { (x, c) =>
        c.copy(inputFiles = c.inputFiles :+ x)
      } text "input .aln file"
      )

    cmd("bedchrsplit") action { (_, c) => c.copy(mode = "bedsplit") } text
      "split bed file by chromosome" children (
      opt[File]('o',"out") optional() valueName "<directory>" action {
        (x, c) => c.copy(out = x)
      } text "a directory to put output files, default is .",
      bed
      )

    cmd("sge-fdur-estep") abbr "sfe" action { (_, c) => c.copy(mode = "qe") } text
      "phylozzii.fdur's estep for runnig on SGE system" children (
      cbf,
      maf,
      newick,
      param
      )

    cmd("wigwig") action {(_,c) => c.copy(mode = "wigwig")} text
      "puts values in intersections of two input .wig files" children (
      out,
      wig,
      wig
      )

    cmd("geneList") action {(_,c) => c.copy(mode = "geneList")} text
      "print geneIDs" children (
      out,
      opt[Double]("mi") optional() valueName "float" action { (x, c) =>
        c.copy(doubleArgs = c.doubleArgs :+ x)
      } text "minimum ebls score: default = 0.0",
      opt[Double]("ma") optional() valueName "float" action { (x, c) =>
        c.copy(doubleArgs = c.doubleArgs :+ x)
      } text "maximum ebls score: default = 1.0",
      arg[File]("<file>") required() action { (x, c) =>
        c.copy(inputFiles = c.inputFiles :+ x)
      } text "input directory which containsa .tsv file"
      )

    cmd("enhancers") action {(_,c) => c.copy(mode = "enhancers")} text
      "print chromosome, position, score and geneIDs" children (
      out,
      wig,
      bed
      )

    cmd("bedSort") action {(_,c) => c.copy(mode = "bedSort")} text
      "" children (
      out,
      bed
      )

    cmd("extract") action {(_,c) => c.copy(mode = "extenh")} text "" children(
      out,
      wig,
      bed
      )

    def cbf = opt[Unit]("const-base-frequent") abbr "cbf" action {
      (_, c) => c.copy(constFreq = true)
    } text "during parameter training, base frequency is not altered"

    def maf = arg[File]("<file>") required() action { (x, c) =>
      c.copy(inputFiles = c.inputFiles :+ x)
    } text "input .maf file"

    def newick = arg[File]("<File>") required() action {
      (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
    } text "input newick format file"

    def param = arg[File]("<file>") required() action {
      (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
    } text "file on which parameters are written"

    def wig = arg[File]("<file>") required() action {
      (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
    }

    def bed = arg[File]("<file>") required() action {
      (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
    } text "input .bed file"

    def out = opt[File]('o', "out") optional() valueName "<file>" action {
      (x, c) => c.copy(out = x)
    } text "output files, default is stdout"
  }

  parser.parse(args, Config()) match {
    case Some(conf) =>
      conf.mode match {
        case "fdur" =>
          sparkem(conf.inputFiles(0), conf.inputFiles(1), conf.inputFiles(2), conf.optionalIntegers(0), conf.optionalIntegers(1), conf.constFreq)

        case "ebls" =>
          val prefix = conf.inputFiles.head.getName.split('.').head
          val bls = new File(conf.out.getPath + s"/$prefix.bls.wig.gz")
          val blsa = new File(conf.out.getPath + s"/$prefix.blsa.wig.gz")
          blser(conf.stringArgs.head, conf.inputFiles(0), conf.inputFiles(1), conf.inputFiles(2), bls, blsa, conf.optionalFiles.head)

        case "wighist" =>
          val ws = biformat.bigSource(conf.inputFiles.head)
          val bs = if(conf.optionalFiles.nonEmpty) Some(biformat.bigSource(conf.optionalFiles.head)) else None
          conf.out.createNewFile()
          val os = if(conf.out.isFile) f2stream(conf.out) else System.out
          try wighist(ws, bs, conf.stringArgs.head, os)
          finally {
            ws.close()
            bs foreach (_.close())
            if(conf.out.isFile) os.close()
          }

        case "wigwig" =>
          val ws1 = biformat.bigSource(conf.inputFiles(0))
          val ws2 = biformat.bigSource(conf.inputFiles(1))
          conf.out.createNewFile()
          val os = if(conf.out.isFile) f2stream(conf.out) else System.out
          try wigwigphyloP(ws1, ws2, os)
          finally {
            ws1.close()
            ws2.close()
            if(conf.out.isFile) os.close()
          }

        case "counter" =>
          conf.out.createNewFile()
          val os = if(conf.out.isFile) new PrintStream(conf.out) else System.out
          val ms = biformat.bigSource(conf.inputFiles.head)
          try counter(ms, os)
          finally {
            ms.close()
            if(conf.out.isFile) os.close()
          }

        case "en" =>
          val file = conf.inputFiles.head
          val as = biformat.bigSource(file)
          val name = file.getName.split('.').head
          try entrop(as, name)
          finally as.close()

        case "bedsplit" =>
          val bedSource = biformat.bigSource(conf.inputFiles.head)
          bedChrSplit(bedSource, conf.out)

        case "geneList" =>
          val inputSources = conf.inputFiles.head.listFiles.map(biformat.bigSource)
          val minimum = if(conf.doubleArgs(0) < 0.0) 0.0 else conf.doubleArgs(0)
          val maximum = if(conf.doubleArgs(1) < 0.0) 1.0 else conf.doubleArgs(1)
          val ps = if(conf.out.getName == ".") System.out else new PrintStream(conf.out)
          geneList(inputSources, ps, minimum, maximum)
          inputSources.foreach(_.close())
          ps match {
            case _:PrintStream => ps.close()
            case _ =>
          }

        case "enhancers" =>
          val inputSource = biformat.bigSource(conf.inputFiles(0))
          val referenceSource = biformat.bigSource(conf.inputFiles(1))
          val ps = if(conf.out.getName == ".") System.out else new PrintStream(conf.out)
          enhancers(inputSource,referenceSource,ps)
          inputSource.close()
          referenceSource.close()
          ps match {
            case _:PrintStream => ps.close()
            case _ =>
          }

        case "bedSort" =>
          val ps = if(conf.out.getName == ".") System.out else new PrintStream(conf.out)
          val bs = biformat.bigSource(conf.inputFiles.head)
          bedSort(bs, ps)

        case "extenh" =>
          val ps = if(conf.out.getName == ".") System.out else new PrintStream(conf.out)
          val wig = biformat.bigSource(conf.inputFiles.head)
          val enh = biformat.bigSource(conf.inputFiles(1))
          extractEnhancers(enh, wig, ps)
          ps match {
            case _:PrintStream => ps.close()
            case _ =>
          }
          wig.close()
          enh.close()
      }

    case None => Unit
  }

  def f2stream(f: File): PrintStream =
    new PrintStream(new BufferedOutputStream(new FileOutputStream(f), 1024 * 1024))
}

case class Config(mode: String = "", doubleArgs: Array[Double] = Array.empty[Double],
                  inputFiles: Array[File] = Array.empty[File], optionalIntegers: Array[Int] = Array.empty[Int],
                  out: File = new File("."), stringArgs: Array[String] = Array.empty[String],
                  optionalFiles: Array[File] = Array.empty[File], isSciName: Boolean = false, sp: Boolean = false,
                  partition: Int = 512, constFreq: Boolean = false, spark: Boolean = false)

