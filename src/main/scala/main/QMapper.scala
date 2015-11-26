package main

import java.io._
import fdur._
import biformat.Maf.readMaf

object QMapper {
  def main(args:Array[String]): Unit = {
    //println(scala.collection.parallel.availableProcessors)
    //${al} ${nh} ${count} target/time/e/${SGE_TASK_ID}.time
    val cols = readMaf(args(1),10000)
    val tree = ModelTree.fromFile(args(3))
    //val out = new BufferedWriter(new FileWriter(args(4)))
      args(0) match {
        case "em" =>
          val param = Parameters.fromFile(args(2))
          val results = cols.map { c => Optimizer.ldestep(tree, c, Model(param)) }
          val summed = results.reduce {
            (m, x) =>
              val ns = m._1 + x._1
              val Ns = (m._2, x._2).zipped.map(_ + _)
              val Fd = (m._3, x._3).zipped.map(_ + _)
              val l = m._4 + x._4
              val i = m._5 + x._5
              (ns, Ns, Fd, l, i)
          }
          EMprinter(summed, args(4))
        case _ => Unit
        /* case "gd" =>
          val param = Parameters.readAsGD(args(2))
          val model = Model(Parameters(DenseVector.vertcat(param,DenseVector(tree.branches.toArray))))
          val results = cols.map{c => Optimizer.ldgradMap(tree, c, model)}
          val summed = results.reduce {
            (x, y) =>
              val n1 = x._1 + y._1
              val n2 = x._2 + y._2
              val n3 = (x._3, y._3).zipped.map(_ + _)
              val n4 = x._4 + y._4
              (n1, n2, n3, n4)
          }
          GDprinter(summed, out)
      }
    }finally {
      out.close()
    }*/
      }
  }

  protected def EMprinter(result:(VD,List[MD],List[VD],Double,Long),out:String): Unit = {
    val w = new BufferedWriter(new FileWriter(out))
    w.write("ns: " + result._1.toArray.mkString("(", ",", ")"))
    w.newLine()
    w.write("Fd: " + result._2.map(x => x.toArray.mkString("(", ",", ")")).mkString(","))
    w.newLine()
    w.write("Ns: " + result._3.map(x => x.toArray.mkString("(",",",")")).mkString(","))
    w.newLine()
    w.write("lgl: " + result._4)
    w.newLine()
    w.write("length: " + result._5)
    w.close()
  }

  protected def GDprinter(result:(VD, MD, List[Double], Double),w:PrintWriter): Unit = {
    w.println("pi: " + result._1.toArray.mkString("(", "\t", ")"))
    w.println("b: " + result._2.toArray.mkString("(", "\t", ")"))
    w.println("branch: " + result._3.mkString("(", "\t", ")"))
    w.println("lgl: " + result._4)
  }
}

