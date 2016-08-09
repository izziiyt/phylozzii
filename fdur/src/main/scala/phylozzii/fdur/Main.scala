package phylozzii.fdur

import scopt.OptionParser
import java.io.File

object Main extends App{

  val parser = new OptionParser[Config]("branco") {
    head("branco", "0.2.1")
    help("help").text("prints this usage text")
    version("version").text("prints version")
      cbf
      opt[Int]("maxit") abbr "mi" valueName "<integer>" action {
        (x, c) => c.copy(optionalIntegers = c.optionalIntegers :+ x)
      } text "how many iterates at most"
      opt[Int]('P', "partition") valueName "<integer>" action {
        (x, c) => c.copy(optionalIntegers = c.optionalIntegers :+ x)
      } text "data partition size"
      opt[Unit]('s', "spark") required() action {
        (_, c) => c.copy(spark = true)
      } text "flag to use on spark"
      maf
      newick
      param

    cmd("sge-fdur-estep") abbr "sfe" action { (_, c) => c.copy(mode = "qe") } text
      "phylozzii.fdur's estep for runnig on SGE system" children (
      cbf,
      maf,
      newick,
      param
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
    case Some(conf) => Unit
    case None => Unit
  }

}

case class Config(mode: String = "", doubleArgs: Array[Double] = Array.empty[Double], optionalBooleans: Array[Boolean] = Array.empty[Boolean],
                  inputFiles: Array[File] = Array.empty[File], optionalIntegers: Array[Int] = Array.empty[Int],
                  out: File = new File("."), stringArgs: Array[String] = Array.empty[String],
                  optionalFiles: Array[File] = Array.empty[File], isSciName: Boolean = false, sp: Boolean = false,
                  partition: Int = 512, constFreq: Boolean = false, spark: Boolean = false)

