package phylozzii.util

import java.io._
import java.util.zip.GZIPOutputStream

import phylozzii.fdur.ModelTree
import scopt.OptionParser

object Main extends App{
  import Others._

  val parser = new OptionParser[Config]("phylozzii") {
    head("util", "0.2.1")
    help("help") abbr "h" text "prints this usage text"

    cmd("goHist") action { (_, c) => c.copy(mode = "goHist") } text
      "make histgram with go txv" children(
      out,
      arg[File]("<file>") required() action {
        (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
      } text ".tsv go term refseq genes written",
      arg[File]("<File>") required() action {
        (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
      } text "directory which contains .enh",
      arg[File]("<File>") required() action {
        (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
      } text "directory which contains .wig"
      )

    cmd("4dcollect") action { (_, c) => c.copy(mode = "4d")} text
    "collect 4d sites" children(
      out,
      opt[File]('t', "target") optional() action{
        (x, c) => c.copy(optionalFiles = c.optionalFiles :+ x)
      } text "file target species written for selecting 4d sites. 4d sites are defined as intersection of 4d sites of each species.",
      opt[File]('c', "codon-table") optional() action{
        (x, c) => c.copy(optionalFiles = c.optionalFiles :+ x)
      } text "a file codon table written",
      arg[File]("<File>") required() action {
        (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
      } text "*exon.Nuc,fa",
      arg[File]("<File>") required() action {
        (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
      } text "*exon.AA.fa",
        arg[File]("<File>") required() action {
        (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
      } text ".nh"
      )

    cmd("wighist") action { (_, c) => c.copy(mode = "wighist") } text
      "filters .wig file on the basis of information of .bed" children(
      opt[Unit]('e', "enhancer") optional() action {
        (_, c) => c.copy(sp = true)
      } text "flag to use enhancer not bed",
      opt[File]('b', "bed") required() valueName "<string>" action {
        (x, c) => c.copy(optionalFiles = c.optionalFiles :+ x)
      } text ".bed file used for filtering",
      out,
      wig
      )

    cmd("changename") action { (_, c) => c.copy(mode = "changename") } text
      "change scientific names to common names and vice versa" children(
      out,
      arg[File]("<File>") required() action {
        (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
      } text "input newick format file, branch length",
      arg[File]("<File>") required() action {
        (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
      } text "input newick format file, leaf name"
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


    cmd("wigwig") action {(_,c) => c.copy(mode = "wigwig")} text
      "puts values in intersections of two input .wig files" children (
      opt[Unit]('p', "phyloP") optional() action {
        (_, c) => c.copy(optionalBooleans = c.optionalBooleans :+ true)
      } text "if second .wig contains phyloP value, default phastCons value",
      out,
      wig,
      wig
      )

    cmd("bedUnion") action {(_,c) => c.copy(mode = "bedUnion")} text
      "" children (
      out,
      bed
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
      } text "input wig directory",
      arg[File]("<file>") required() action{ (x,c) =>
        c.copy(inputFiles = c.inputFiles :+ x) }
        text "enhancer_tss bed",
      arg[File]("<file>") required() action{ (x,c) =>
        c.copy(inputFiles = c.inputFiles :+ x) }
        text "Uberon enhancer bed"
      )

    cmd("bedSort") action {(_,c) => c.copy(mode = "bedSort")} text
      "sorts .bed file with chromosome name, starting posisition, end position" children (
      out,
      bed
      )
    cmd("wigSort") action {(_,c) => c.copy(mode = "bedSort")} text
      "sorts .bed file with chromosome name, starting posisition, end position" children (
      out,
      wig
      )
    cmd("extract") action {(_,c) => c.copy(mode = "extenh")} text "" children(
      out,
      wig,
      bed
      )

    cmd("length") action {(_,c) => c.copy(mode = "length")} text
      "returns length of structured formated file" children(
      opt[Unit]('e', "enhancer") optional() action {
        (_, c) => c.copy(optionalIntegers = c.optionalIntegers :+ 0)
      } text "flag to use enhancer not bed",
      opt[Unit]('w', "wig") optional() action {
        (_, c) => c.copy(optionalIntegers = c.optionalIntegers :+ 2)
      } text "flag to use wig",
      opt[Unit]('b', "bed") optional() action {
        (_, c) => c.copy(optionalIntegers = c.optionalIntegers :+ 1)
      } text "flag to use bed",
      arg[File]("<file>") required() action { (x, c) =>
      c.copy(inputFiles = c.inputFiles :+ x)
      } text "input file"
      )

    cmd("randomwig") action {(_,c) => c.copy(mode = "randomwig")} text "" children(
      out,
      wig
      )

    cmd("randombed") action {(_,c) => c.copy(mode = "randombed")} text "" children(
      out,
      bed,
      arg[Int]("<integer>") required() action { (x, c) =>
        c.copy(optionalIntegers = c.optionalIntegers :+ x)
      } text "number of random selected BedLine"
      )

    cmd("wigfilter") action {(_,c) => c.copy(mode = "wigfilter")} text "" children(
      opt[Unit]('e', "enhancer") optional() action {
        (_, c) => c.copy(stringArgs = c.stringArgs :+ "enhancer")
      } text "flag to use enhancer not bed",
      opt[Unit]('t', "tss") optional() action {
        (_, c) => c.copy(stringArgs = c.stringArgs :+ "tss")
      } text "flag to use tss not bed",
      opt[Int]('w',"wing") optional() valueName "<integer>" action { (x, c) =>
        c.copy(optionalIntegers = c.optionalIntegers :+ x)
      } text "filter with bed.start - wing to bed.end + wing",
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
    } text "input .wig file"

    def bed = arg[File]("<file>") required() action {
      (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
    } text "input .bed file"

    def out = opt[File]('o', "out") optional() valueName "<file>" action {
      (x, c) => c.copy(out = x)
    } text "output path, default is stdout"
  }

  parser.parse(args, Config()) match {
    case Some(conf) =>
      conf.mode match {
        case "wighist" =>
          wighist(conf.inputFiles.head, conf.optionalFiles.headOption, conf.out, conf.sp)

        case "4d" =>
          FdFilter.fdfilter(conf.inputFiles.head, conf.inputFiles(1), conf.out, conf.inputFiles(2), conf.optionalFiles.head, conf.optionalFiles(1))

        case "goHist" =>
          def f(wigf: File) = {
            val gotsvf = conf.inputFiles(0)
            val bedf = conf.inputFiles(1)
            val outfileName = wigf.getName.replace(".gz", "").replace(".wig", "") + ".hist"
            goHist(gotsvf, bedf, wigf, conf.out, outfileName)
          }
          val _wigf = conf.inputFiles(2)
          if(_wigf.isDirectory) _wigf.listFiles.foreach(f) else f(_wigf)

        case "changename" =>
          val branch_ = conf.inputFiles.head
          val name_ = conf.inputFiles(1)
          val x = ModelTree.fromFile(branch_).changeNames(ModelTree.fromFile(name_).names)
          if(conf.out.getName == "."){
            println(x.toString)
          }
          else {
            val p = new PrintWriter(conf.out)
            p.println(x.toString)
            p.close()
          }

        case "wigfilter" =>
          def f(wf: File) {
            val prefix = wf.getName
            val isgz = wf.getName.split('.').last == "gz"
            val bf = conf.inputFiles(1)
            val outf = new File(conf.out.getAbsolutePath, prefix)
            val os =
              if (isgz) new PrintStream(new GZIPOutputStream(new FileOutputStream(outf), 1024 * 1024))
              else f2stream(outf)
            if(conf.stringArgs.isEmpty)
              wigfilter(wf, bf, os, conf.optionalIntegers.headOption.getOrElse(0), false)
            else if(conf.stringArgs.head == "enhancer")
              wigfilter(wf, bf, os, conf.optionalIntegers.headOption.getOrElse(0), true)
            else
              wigfilter(wf, bf, os, conf.optionalIntegers.headOption.getOrElse(0), false, true)
            os.close()
          }
          val _wf = conf.inputFiles.head
          if(_wf.isDirectory) {
            conf.out.mkdir()
            _wf.listFiles.foreach(f)
          } else f(_wf)

        case "wigwig" =>
          val ws1 = biformat.bigSource(conf.inputFiles(0))
          val ws2 = biformat.bigSource(conf.inputFiles(1))
          conf.out.createNewFile()
          val os = if(conf.out.isFile) f2stream(conf.out) else System.out
          try wigwig(ws1, ws2, os, conf.optionalBooleans.headOption.getOrElse(false))
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

        case "geneList" =>
          val minimum = if(conf.doubleArgs(0) < 0.0) 0.0 else conf.doubleArgs(0)
          val maximum = if(conf.doubleArgs(1) < 0.0) 1.0 else conf.doubleArgs(1)
          println(minimum + " to " + maximum)
          geneList(conf.inputFiles.tail, conf.inputFiles.head,conf.out, minimum, maximum)

        case "bedUnion" =>
          val os =
          if (conf.out.getName.endsWith(".gz")) new PrintStream(new GZIPOutputStream(new FileOutputStream(conf.out), 1024 * 1024))
          else f2stream(conf.out)
          bedUnion(conf.inputFiles.head, os)
          os.close()

        case "bedSort" =>
          val in = conf.inputFiles.head
          bedSort(in, if(conf.out.getName == ".") in else conf.out)

        case "randomwig" =>
          def f(wigf: File): Unit =  {
            val outf = new File(conf.out.getAbsolutePath, wigf.getName)
            val os = if (wigf.getName.endsWith(".gz")) {
              new PrintStream(new GZIPOutputStream(new FileOutputStream(outf), 2048))
            } else {
              f2stream(outf)
            }
            randomwig(wigf, os, conf.optionalIntegers.headOption.getOrElse(500))
            os.close()
          }
          conf.inputFiles.head.listFiles.foreach(f)

        case "randombed" =>
          randombed(conf.inputFiles.head, conf.out, conf.optionalIntegers.head)

        case "wigSort" =>
          val in = conf.inputFiles.head
          wigSort(in, if(conf.out.getName == ".") in else conf.out)

        case "length" =>
          val f = conf.inputFiles.head
          val mode = conf.optionalIntegers.headOption.getOrElse(Int.MaxValue)
          length(f,mode)
      }

    case None => Unit
  }

  private def f2stream(f: File): PrintStream =
    new PrintStream(new BufferedOutputStream(new FileOutputStream(f), 1024 * 1024))
}

case class Config(mode: String = "", doubleArgs: Array[Double] = Array.empty[Double], optionalBooleans: Array[Boolean] = Array.empty[Boolean],
                  inputFiles: Array[File] = Array.empty[File], optionalIntegers: Array[Int] = Array.empty[Int],
                  out: File = new File("."), stringArgs: Array[String] = Array.empty[String],
                  optionalFiles: Array[File] = Array.empty[File], isSciName: Boolean = false, sp: Boolean = false,
                  partition: Int = 512, constFreq: Boolean = false, spark: Boolean = false)

