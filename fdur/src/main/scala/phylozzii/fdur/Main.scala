package phylozzii.fdur

import scopt.OptionParser
import java.io.File

object Main extends App{

  val parser = new OptionParser[Config]("branco") {
    head("fdur", "0.2.1")
    opt[Int]("maximum-iteration") abbr "mi" valueName "<Integer>" optional() action {
      (x, c) => c.copy(maxit = x)
    } text "how many em iterates at most, default is 200"
    opt[Int]('P', "partition") valueName "<Integer>" action {
      (x, c) => c.copy(partition = x)
    } text "data partition size"
    opt[Unit]('s', "spark") required() action {
      (_, c) => c.copy(spark = true)
    } text "runs on spark"
    newick
    param
    maf

    help("help") abbr "h" text "prints this usage text"
    version("version") abbr "v" text "prints version"

    cmd("estep") action { (_, c) => c.copy(mode = "e") } text
      "runs estep just once" children (
      intermidiate,
      arg[File]("<File>") required() action {
        (x, c) => c.copy(nh = x)
      } text "input newick format file",
      arg[File]("<File>") required() action {
        (x, c) => c.copy(param = x)
      } text "file on which parameters are written",
      maf
      )

    cmd("mstep") action { (_, c) => c.copy(mode = "m") } text
      "runs mstep just once" children (
      const,
      newick,
      param,
      intermidiate
      )

    def intermidiate = arg[File]("intermidiate-product") abbr "ip" optional() action {
      (x, c) =>
        if(!x.isDirectory) x.mkdirs()
        c.copy(ip = x)
    } text "a directory for intermidiate files estep produces"

    def const = opt[Unit]('c', "const-base-frequency") action {
      (_, c) => c.copy(const = true)
    } text "during parameter training, base frequency is fixed"

    def maf = arg[File]("<File>") required() action {
      (x, c) =>
        c.copy(maf = x)
    } text "input .maf file"

    def newick = opt[Seq[File]]('n', "newick") valueName "<File> | <File> <File>" required() action {
      (x, c) =>
        x.length match {
          case 1 =>
            c.copy(nh = x.head)
            c.copy(outnh = x.head)
          case 2 =>
            c.copy(nh = x.head)
            c.copy(outnh = x.last)
          case _ =>
            throw new UnsupportedOperationException
        }
    } text "first file is input and second is output file. If file is single, output is overwritten"

    def param = opt[Seq[File]]('p', "param") valueName "<File> | <File> <File>" required() action {
      (x, c) =>
        x.length match {
          case 1 =>
            c.copy(param = x.head)
            c.copy(outparam = x.head)
          case 2 =>
            c.copy(param = x.head)
            c.copy(outparam = x.last)
          case _ =>
            throw new UnsupportedOperationException
        }
    } text "first file is input and second is output file. If file is single, output is overwritten"

    def out = opt[File]('o', "out") optional() valueName "<file>" action {
      (x, c) => c.copy(out = x)
    } text "output path, default is stdout"
  }

  parser.parse(args, Config()) match {
    case Some(conf) => Unit
    case None => Unit
  }

}

case class Config(ip: File = new File("/tmp/fdurEstepIntermidiate"), out: File = new File("."), mode: String = "", maf: File = new File("."),
                  nh: File = new File("."), param: File = new File("."), const: Boolean = false, spark: Boolean = false,
                  maxit: Int = 200, partition: Int = 0, outnh: File = new File("."), outparam: File = new File("."))

