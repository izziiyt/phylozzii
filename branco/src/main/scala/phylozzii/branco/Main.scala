package phylozzii.branco

import scopt.{OptionParser, RenderingMode}
import java.io.{BufferedOutputStream, File, FileOutputStream, PrintWriter}
import java.util.zip.GZIPOutputStream

import biformat.MafIterator
import phylozzii.fdur.{Model, ModelTree, Parameters}

object Main extends App{

  val parser = new OptionParser[Config]("branco") {
    //override def renderingMode = scopt.RenderingMode.OneColumn
    head("branco", "0.2.1")
    opt[File]('o', "out") optional() valueName "<Prefix-Path>" action {
      (x, c) =>
        c.copy(out = x)
    } text "output prefix path (prefix), it means path will be such as <Prefix-Path>.bls.wig"
    opt[Unit]('r', "reg").action{
      (_, c) => c.copy(reg = true)
    } text "expected branch length scores will be scalled 0 to 1"
    opt[String]('t', "target") required() valueName "<String>" action {
      (x, c) => c.copy(target = x)
    } text "target species' name"
    opt[Unit]("gzip").abbr("z").action{ (_, c) =>
      c.copy(gz = true)
    } text "output will be gzip compressed"
    help("help") abbr "h" text "prints this usage text"
    version("version") abbr "v" text "prints version"
    arg[File]("<File>") required() action {
      (x, c) => c.copy(param = x)
    } text "parameters of probablistic evolution model"
    arg[File]("<File>") required() action {
      (x, c) => c.copy(nh = x)
    } text ".nh file"
    arg[File]("<File>") required() action {
      (x, c) =>
          c.copy(maf = x)
    } text ".maf file"
  }

  parser.parse(args, Config()) match {
    case Some(conf) =>
          val (_bls, _blsa) =
            if(conf.gz) (
              new File(conf.out.getAbsolutePath + "." + "bls.wig.gz"),
              new File(conf.out.getAbsolutePath + "." + "blsa.wig.gz")
              )
            else (
              new File(conf.out.getAbsolutePath + "." + "bls.wig"),
              new File(conf.out.getAbsolutePath + "." + "blsa.wig")
              )

          println("Output path for ebls is : " + _bls.getAbsolutePath)
          println("Output path for eblsa is : " + _blsa.getAbsolutePath)

          val maf = biformat.bigSource(conf.maf)
          val (bls, blsa) = (bigPrintWriter(_bls), bigPrintWriter(_blsa))

          try{
            val its = MafIterator.fromSource(maf, conf.target).merged(10240)
            val model = Model(Parameters.fromFile(conf.param))
            val tree = ModelTree.fromFile(conf.nh)
            if(!tree.names.contains(conf.target))
              throw new UnsupportedOperationException("newick formatted tree doesn't contain " + conf.target + ".")
            else
              Branco.exe(its, tree, model, conf.target, bls, blsa, conf.reg)
          }
          catch{
            case e: Throwable => e.printStackTrace()
          }
          finally{
            maf.close()
            bls.close()
            blsa.close()
          }
    case None => Unit
  }

  protected def bigPrintWriter(f : File): PrintWriter =
    if(f.getName.endsWith(".gz"))
      new PrintWriter(new GZIPOutputStream(new FileOutputStream(f), 1024 * 1024))
    else
      new PrintWriter(new BufferedOutputStream(new FileOutputStream(f), 1024 * 1024))
}

case class Config(maf: File = new File("."), param: File = new File("."), nh: File = new File("."),
                  out: File = new File("."), target: String = "", gz: Boolean = false, reg: Boolean = false)

