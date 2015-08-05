/*import java.io.PrintWriter

import fdur.Estep._
import util.Util
import alignment.Base
package object fdur {
  def main(args: Array[String]): Unit = {
    args.head match {
      case "estep" => estep(args.tail)
      //case "mstep" => mstep(args.tail)
      //case "em" => em(args.tail)
      case _ => sys.error("\"estep\", \"mstep\" and \"em\" are acceptable program names. " + args.head + " is not listed.")
    }
  }

  protected def estep(args: Array[String]): Unit = {
    val opts = args.map(Flag(_)).withFilter(_.isDefined).map(_.get)

    var param: Option[Parameters] = None
    var tree: Option[FdurNode] = None
    var al: Option[Array[Array[Base]]] = None
    var flag = false
    var writer:Option[PrintWriter] = None

    for(op <- opts) op match {
      case Flag.BaseFrequencyFix(b) => flag = b
      case Flag.InNewick(n) => tree = Some(FdurTree.fromFile(n))
      case Flag.Al(n) => al = Some(Util.getAlignments(n))
      case Flag.OutPut(n) => writer = Some(new PrintWriter(n))
      case Flag.Param(n) => param = Some(Parameters.fromFile(n))
    }

    val pt = new PhylogencyTree(tree.get,GTR(param.get))
    val counts = al.get.map(eStep(pt,_))
    val sumCount = counts.reduce(_+_)
    writer.get.println(counts.length)
    writer.get.println(sumCount)
    writer.get.close()
  }


  sealed trait Flag {
    def apply(s: String): Flag
  }

  sealed object Flag {

    case class InNewick(name: String) extends Flag

    case class Maf(name: String) extends Flag

    case class Al(name:String) extends Flag

    case class OutNewick(name: String) extends Flag

    case class BaseFrequencyFix(bool: Boolean) extends Flag

    case class Param(name:String) extends Flag

    case class OutPut(name:String) extends Flag

    def apply(arg: String): Option[Flag] = {
      val kv = arg split '='
      if (kv.length == 2) kv(0) match {
        case "-inh" =>
          Some(Flag.InNewick(kv(1)))
        case "-maf" =>
          Some(Flag.Maf(kv(1)))
        case "-onh" =>
          Some(OutNewick(kv(1)))
        case "-o" =>
          Some(OutPut(kv(1)))
        case "-al" =>
          Some(Al(kv(1)))
        case "-bf" =>
          kv(1) match {
          case "true" | "t" => Some(BaseFrequencyFix(true))
          case "false" | "f" => Some(BaseFrequencyFix(false))
          case _ => None
        }
        case "-p" =>
          Some(Param(kv(1)))
        case _ =>
          sys.error(kv + " isn't a proper parameter.")
      }
      else sys.error(kv + " isn't a proper parameter.")
    }


  }

}
*/