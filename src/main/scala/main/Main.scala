package main

import java.io._
import scopt.OptionParser

object Main {
  import Others._
  import SparkFdur._
  import SGEFdur._
  import BLSer._

  def main(args: Array[String]) {
    val parser = new OptionParser[Conf]("pbls") {
      head("main", "0.2.0-SNAP_SHOT")
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
        arg[File]("<file>") required() action { (x, c) =>
          c.copy(maf = x)
        } text "input .maf file"
        )

      cmd("spark-fdur") action { (_, c) => c.copy(mode = "fdur") } text
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

      cmd("wighist") action { (_, c) => c.copy(mode = "wh") } text
        "filters .wig file on the basis of information of .bed" children(
        opt[File]('b', "bed") optional() valueName "<file>" action {
          (x, c) =>
            c.copy(bed = x)
            c.copy(out = new File(x.getName.split(".").head))
        } text ".bed file",
        opt[String]('c', "chr") required() valueName "<string>" action {
          (x, c) => c.copy(target = x)
        } text "required chromosome name",
        opt[File]('o', "out") abbr "o" optional() valueName "<file>" action {
          (x, c) => c.copy(out = x)
        } text s"output files, default is stdout",
        arg[File]("<file>") required() action { (x, c) =>
          c.copy(wig = x)
        } text "input .wig file"
        )

      cmd("counter") action { (_, c) => c.copy(mode = "ct") } text
        "counts # of [A|C|G|T] contained in input .maf file" children (
        arg[File]("<file>") required() action { (x, c) =>
          c.copy(wig = x)
        } text "input .maf file"
        )

      cmd("entrop") action { (_, c) => c.copy(mode = "en") } text
        "calculates entropies on alignmented DNA sequences" children (
        arg[File]("<file>") required() action { (x, c) =>
          c.copy(aln = x)
        } text "input .aln file"
        )

      cmd("sge-fdur-estep") abbr "sfe" action { (_, c) => c.copy(mode = "qe") } text
        "fdur's estep for runnig on SGE system" children (
        cbf,
        newick,
        param,
        arg[File]("<file>") required() action { (x, c) =>
          c.copy(aln = x)
        } text "input directory"
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
          case "pbls" =>
            println(args.mkString(" "))
            val prefix = conf.maf.getName.split('.').head
            val bls = new File(conf.out.getPath + "/" + prefix + ".bls.wig.gz")
            val blsa = new File(conf.out.getPath + "/" + prefix + ".blsa.wig.gz")
            blser(conf.target, conf.maf, conf.nh, conf.param, bls, blsa, conf.nameNH)
          case "wh" if conf.out.getName == "." =>
            wighist(conf.wig, conf.bed, conf.target)
          case "wh" =>
            val tmp = new PrintStream(conf.out)
            try wighist(conf.wig, conf.bed, conf.target, tmp)
            finally tmp.close()
          case "ct" if conf.out.getName == "." =>
            counter(conf.maf)
          case "ct" =>
            val tmp = new PrintStream(conf.out)
            try counter(conf.maf, tmp)
            finally tmp.close()
          case "en" =>
            entrop(conf.aln)
          case "qe" =>
            qestep(conf.maf, conf.nh, conf.param, conf.out)
          case "qm" =>
            qmstep(conf.targetDir, conf.param, conf.nh, conf.constFreq)
          case _ =>
            throw new UnsupportedOperationException
        }
      case None => throw new UnsupportedOperationException
    }
  }

}

case class Conf(mode: String = "pbls", maf: File = new File("."), tgtDir: File = new File("."),
                param: File = new File("."), targetDir: File = new File("."), target: String = "",
                isSciName: Boolean = false, prefix: String = "", sp: Boolean = false, nh: File = new File(""),
                maxit: Int = 200, partition: Int = 512, constFreq: Boolean = false, nameNH: File = new File("."),
                bed: File = new File("."), wig: File = new File("."), aln: File = new File("."),
                spark: Boolean = false, out: File = new File(".")) {

}

