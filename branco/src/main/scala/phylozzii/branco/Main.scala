package phylozzii.branco

import scopt.OptionParser
import java.io.File

object Main extends App{

  val parser = new OptionParser[Config]("branco") {
    head("branco", "0.2.1")
    opt[File]('c',"change-name") valueName "<file>" optional() action {
      (x, c) => c.copy(inputFiles = c.inputFiles :+ x)
    } text "to change scientific names to common names, and vice versa"
    opt[String]('t', "target") required() valueName "<string>" action {
      (x, c) => c.copy(stringArgs = c.stringArgs :+ x)
    } text "target species"
    opt[File]('o', "out") optional() valueName "<file>" action {
      (x, c) => c.copy(out = x)
    } text "a directory to put output files, default is current working directory"
    help("help") abbr "h" text "prints this usage text"
    version("version") abbr "v" text "prints version"
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
